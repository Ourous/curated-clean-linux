"use strict";

var ABC_DEBUG=false;
var ABC_TRACE_LENGTH=50;

class ABCError extends Error {
	constructor(desc,arg) {
		super(desc);

		switch (desc) {
			case 'illegal instruction':
				this.message+=' '+arg+' ('+abc_instructions[arg]+')';
				break;
			case 'unknown instruction':
				this.message+=' '+arg;
				break;
		}
	}
}

function SharedCleanValue(index) {
	return {
		shared_clean_value_index: index
	};
}

const ABC={
	initialized: false,

	prog: undefined,
	memory: undefined,
	memory_array: undefined,

	start: undefined,
	code_offset: undefined,

	stack_size: (512<<10)*2,
	hp_size: 2<<22,

	util: undefined,
	interpreter: undefined,

	log_buffer: '',
	log: function (s) {
		s=String(s);
		ABC.log_buffer+=s;
		if (s.indexOf('\n')>=0) {
			var lines=ABC.log_buffer.split('\n');
			for (var i=0; i<lines.length-1; i++)
				console.log(lines[i]);
			ABC.log_buffer=lines[lines.length-1];
		}
	},
	empty_log_buffer: function(){
		if (ABC.log_buffer.length>0)
			console.log(ABC.log_buffer);
	},

	deserialize: function (string, component) {
		var max_words_needed=string.length/8*4; // rough upper bound
		if (ABC.interpreter.instance.exports.get_hp_free() < max_words_needed) {
			console.warn('gc from js');
			ABC.util.instance.exports.gc();

			if (ABC.interpreter.instance.exports.get_hp_free() < max_words_needed)
				throw 'not enough heap to deserialize: '+string;
		}

		var array=new Int8Array(string.length);
		for (var i in string)
			array[i]=string.charCodeAt(i);
		var graph=new Uint32Array(array.buffer);
		var unused_semispace=ABC.util.instance.exports.get_unused_semispace();
		for (var i=0; i<graph.length; i++)
			ABC.memory_array[unused_semispace/4+i]=graph[i];

		var old_hp=ABC.interpreter.instance.exports.get_hp();
		var new_hp=ABC.util.instance.exports.copy_from_string(
			unused_semispace,
			graph.length/2,
			ABC.interpreter.instance.exports.get_asp()+8,
			ABC.interpreter.instance.exports.get_bsp()-8,
			old_hp,
			ABC.code_offset*8);
		ABC.interpreter.instance.exports.set_hp(new_hp);

		var new_hp_free=ABC.interpreter.instance.exports.get_hp_free()-(new_hp-old_hp)/8;
		if (new_hp_free<0)
			throw 'hp_free was '+new_hp_free+' after deserialize: '+string;

		ABC.interpreter.instance.exports.set_hp_free(new_hp_free);

		var index=ABC.share_clean_value(ABC.memory_array[unused_semispace/4], component);

		return SharedCleanValue(index);
	},

	interpret: null,

	js: [], // javascript objects accessible from Clean
	empty_js_values: [], // empty indexes in the above array
	share_js_value: function(obj) {
		if (ABC.empty_js_values.length > 0) {
			var i=ABC.empty_js_values.pop();
			if (ABC.js[i]!=undefined)
				throw 'internal error in ABC.share_js_value';
			ABC.js[i]=obj;
			return i;
		} else {
			ABC.js.push(obj);
			return ABC.js.length-1;
		}
	},

	shared_clean_values: [], // pointers to the Clean heap
	empty_shared_clean_values: [], // empty indexes in the above array
	share_clean_value: function(ref, component) {
		if (typeof component.shared_clean_values=='undefined')
			throw 'could not attach shared Clean value to an iTasks component';
		if (component.shared_clean_values==null)
			component.shared_clean_values=new Set();

		var record={ref: ref, component: component};
		var i=null;

		if (ABC.empty_shared_clean_values.length > 0) {
			i=ABC.empty_shared_clean_values.pop();
			if (ABC.shared_clean_values[i]!=null)
				throw 'internal error in ABC.share_clean_value';
			ABC.shared_clean_values[i]=record;
		} else {
			i=ABC.shared_clean_values.length;
			ABC.shared_clean_values.push(record);
		}

		component.shared_clean_values.add(i);

		return i;
	},
	clear_shared_clean_value: function(ref, update_component=true) {
		var component=ABC.shared_clean_values[ref].component;
		if (update_component && typeof component.shared_clean_values!='undefined')
			component.shared_clean_values.delete(ref);

		ABC.shared_clean_values[ref]=null;
		ABC.empty_shared_clean_values.push(ref);
	},

	ap: function (index) { // create a JavaScript closure to call the interpreter
		var f=function () {
			var args=[];
			for (var i=0; i<arguments.length; i++)
				args[i]=arguments[i];
			ABC.interpret(SharedCleanValue(index), args);
		};
		f.shared_clean_value_index=index;
		return f;
	},

	_copy_js_to_clean: function (values, store_ptrs, hp, hp_free) {
		for (var i=0; i<values.length; i++) {
			if (values[i]===null) {
				ABC.memory_array[store_ptrs/4]=ABC.addresses.JSNull-10;
			} else if (typeof values[i]=='undefined') {
				ABC.memory_array[store_ptrs/4]=ABC.addresses.JSUndefined-10;
			} else if (typeof values[i]=='number') {
				ABC.memory_array[store_ptrs/4]=hp;
				if (Number.isInteger(values[i])) {
					if (values[i]>2**31)
						console.warn('Copying value',values[i],'>2^31 to Clean; truncating!');
					ABC.memory_array[hp/4]=ABC.addresses.JSInt;
					ABC.memory_array[hp/4+1]=0;
					ABC.memory_array[hp/4+2]=values[i]; // TODO also support >32-bit
					ABC.memory_array[hp/4+3]=0;
				} else {
					ABC.memory_array[hp/4]=ABC.addresses.JSReal;
					ABC.memory_array[hp/4+1]=0;
					const float_array=new Float64Array(ABC.memory_array.buffer, hp+8);
					float_array[0]=values[i];
				}
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='boolean') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=ABC.addresses.JSBool;
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i] ? 1 : 0;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='string') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=ABC.addresses.JSString;
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=hp+16;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
				ABC.memory_array[hp/4]=6*8+2; // _STRING_
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i].length;
				ABC.memory_array[hp/4+3]=0;
				var array=new Int8Array(((values[i].length+3)>>2)<<2);
				for (var j=0; j<values[i].length; j++)
					array[j]=values[i].charCodeAt(j);
				array=new Uint32Array(array.buffer);
				for (var j=0; j<((values[i].length+3)>>2); j++)
					ABC.memory_array[hp/4+4+j]=array[j];
				hp+=16+(((values[i].length+7)>>3)<<3);
				hp_free-=2+((values[i].length+7)>>3);
			} else if (Array.isArray(values[i])) {
				ABC.memory_array[store_ptrs/4]=hp;
				// On the first run, we don't have the JSArray address yet, so we use
				// the dummy 2 to ensure that jsr_eval won't try to evaluate it. The
				// array elements are unwrapped immediately, so the constructor does
				// not matter (apart from the fact that the HNF bit is set).
				ABC.memory_array[hp/4]=ABC.initialized ? ABC.addresses.JSArray : 2;
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=hp+16;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
				ABC.memory_array[hp/4]=1*8+2; // _ARRAY_
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i].length;
				ABC.memory_array[hp/4+3]=0;
				ABC.memory_array[hp/4+4]=0;
				ABC.memory_array[hp/4+5]=0;
				hp+=24;
				hp_free-=3+values[i].length;;
				var copied=ABC._copy_js_to_clean(values[i], hp, hp+8*values[i].length, hp_free);
				hp=copied.hp;
				hp_free=copied.hp_free;
			} else if ('shared_clean_value_index' in values[i]) {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=ABC.addresses.JSCleanRef;
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=values[i].shared_clean_value_index;
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='object' || typeof values[i]=='function') {
				ABC.memory_array[store_ptrs/4]=hp;
				ABC.memory_array[hp/4]=ABC.addresses.JSRef;
				ABC.memory_array[hp/4+1]=0;
				ABC.memory_array[hp/4+2]=ABC.share_js_value(values[i]);
				ABC.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else { // should be handled by copied_node_size
				throw new ABCError('internal in copy_js_to_clean');
			}

			store_ptrs+=8;
		}

		return {
			hp: hp,
			hp_free: hp_free,
		};
	},
	copied_node_size: function (value) {
		if (value===null)
			return 0;
		else if (typeof value=='undefined')
			return 0;
		else if (typeof value=='number')
			return 2;
		else if (typeof value=='boolean')
			return 2;
		else if (typeof value=='string')
			return 2+2+((value.length+7)>>3);
		else if (Array.isArray(value)) {
			var size=2+3+value.length;
			for (var i=0; i<value.length; i++)
				size+=ABC.copied_node_size(value[i]);
			return size;
		} else if ('shared_clean_value_index' in value)
			return 2;
		else if (typeof value=='object' || typeof value=='function')
			return 2;
		else {
			console.error('Cannot pass this JavaScript value to Clean:',value);
			throw new ABCError('missing case in copy_js_to_clean');
		}
	},
	copy_js_to_clean: function (value, store_ptrs, hp, hp_free) {
		var node_size=ABC.copied_node_size(value);
		if (node_size>hp_free) {
			console.warn('gc from js');
			ABC.util.instance.exports.gc();
			hp=ABC.interpreter.instance.exports.get_hp();
			hp_free=ABC.interpreter.instance.exports.get_hp_free();
			if (node_size>hp_free) {
				console.error('not enough memory to copy',value);
				throw new ABCError('out of memory');
			}
		}

		var result=ABC._copy_js_to_clean([value], store_ptrs, hp, hp_free);

		if (hp_free-result.hp_free!=node_size)
			console.warn('copied_node_size: expected',node_size,'; got',hp_free-result.hp_free,'for',value);

		return result;
	},

	get_clean_string: function (hp_ptr) {
		var size=ABC.memory_array[hp_ptr/4+2];
		var string_buffer=new Uint8Array(ABC.memory.buffer, hp_ptr+16);
		var string='';
		for (var i=0; i<size; i++)
			string+=String.fromCharCode(string_buffer[i]);
		return string;
	},

	addresses: {},
};

ABC.loading_promise=fetch('js/app.pbc').then(function(resp){
	if (!resp.ok)
		throw 'failed to fetch bytecode';
	return resp.arrayBuffer();
}).then(function(bytecode){
	ABC.prog=new Uint32Array(bytecode);

	const blocks_needed=Math.floor((ABC.prog.length*4 + ABC.stack_size + ABC.hp_size*2 + 65535) / 65536);

	ABC.memory=new WebAssembly.Memory({initial: blocks_needed});
	ABC.memory_array=new Uint32Array(ABC.memory.buffer);

	for (var i in ABC.prog)
		ABC.memory_array[i]=ABC.prog[i];

	(function(prog){
		var i=0;
		while (prog.length > 0) {
			if (prog[0]==1) /* ST_Code section; see ABCInterpreter's bcprelink.c */
				ABC.code_offset=i+1;
			i+=1+prog[1];
			prog=prog.slice(2+2*prog[1]);
		}
	})(ABC.prog);

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter-util.wasm'),
		{ clean: {
			memory: ABC.memory,

			has_host_reference: function (index) {
				if (index>=ABC.shared_clean_values.length)
					return 0;
				if (ABC.shared_clean_values[index]==null)
					return -1;
				return ABC.shared_clean_values[index].ref;
			},
			update_host_reference: function (index, new_location) {
				ABC.shared_clean_values[index].ref=new_location;
			},

			gc_start: function() {
				ABC.active_js=[];
			},
			js_ref_found: function(ref) {
				ABC.active_js[ref]=true;
			},
			gc_end: function() {
				if (ABC_DEBUG)
					console.log(ABC.interpreter.instance.exports.get_hp_free(),'free words after gc');
				ABC.empty_js_values=[];
				// NB: we cannot reorder ABC.js, because garbage collection may be
				// triggered while computing a string to send to JavaScript which can
				// then contain illegal references.
				for (var i=0; i<ABC.js.length; i++) {
					if (typeof ABC.active_js[i]=='undefined') {
						delete ABC.js[i];
						ABC.empty_js_values.push(i);
					}
				}
				delete ABC.active_js;
			},

			get_asp: () => ABC.interpreter.instance.exports.get_asp(),
			set_hp: hp => ABC.interpreter.instance.exports.set_hp(hp),
			set_hp_free: free => ABC.interpreter.instance.exports.set_hp_free(free),

			debug: function(what,a,b,c) {
				if (!ABC_DEBUG)
					return;
				switch (what) {
					case 0: console.log('loop',a,'/',b,'; hp at',c); break;
					case 1: console.log('desc',a); break;
					case 2: console.log('hnf, arity',a); break;
					case 3: console.log('thunk, arities',a,b,c); break;
				}
			}
		}}
	);
}).then(function(util){
	ABC.util=util;

	const interpreter_imports={
		clean: {
			memory: ABC.memory,

			debug_instr: function (addr, instr) {
				if (ABC_DEBUG)
					console.log(addr/8-ABC.code_offset,abc_instructions[instr]);
			},
			handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp, hp_free) {
				if (abc_instructions[instr]=='instruction') {
					const arg=ABC.memory_array[(pc+8)/4];
					switch (arg) {
						case 0: /* evaluation finished */
							return 0;
						case 1: /* iTasks.UI.JS.Interface: set_js */
							var v=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var x=ABC.get_clean_string(ABC.memory_array[asp/4-2]);
							if (ABC_DEBUG)
								console.log(v,'.=',x);
							try {
								var ref=eval(v+'.shared_clean_value_index');
								if (typeof ref != 'undefined') {
									if (ABC_DEBUG)
										console.log('removing old reference to Clean',ref);
									ABC.clear_shared_clean_value(ref);
								}
							} catch (e) {}
							Function(v+'='+x)();
							break;
						case 2: /* iTasks.UI.JS.Interface: eval_js */
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							Function(string)();
							break;
						case 3: /* iTasks.UI.JS.Interface: eval_js_with_return_value */
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							if (ABC_DEBUG)
								console.log('eval',string);
							var result=eval('('+string+')'); // the parentheses are needed for {}, for instance
							var copied=ABC.copy_js_to_clean(result, asp, hp, hp_free);
							ABC.interpreter.instance.exports.set_hp(copied.hp);
							ABC.interpreter.instance.exports.set_hp_free(copied.hp_free);
							break;
						case 4: /* iTasks.UI.JS.Interface: share */
							var attach_to=ABC.memory_array[bsp/4];
							var index=ABC.share_clean_value(ABC.memory_array[asp/4],ABC.js[attach_to]);
							ABC.memory_array[bsp/4]=index;
							break;
						case 5: /* iTasks.UI.JS.Interface: fetch */
							var index=ABC.memory_array[bsp/4];
							ABC.memory_array[asp/4]=ABC.shared_clean_values[index].ref;
							break;
						case 6: /* iTasks.UI.JS.Interface: deserialize */
							var attach_to=ABC.memory_array[bsp/4];
							var string=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var shared_clean_value=ABC.deserialize(string,ABC.js[attach_to]);
							ABC.memory_array[asp/4]=ABC.shared_clean_values[shared_clean_value.shared_clean_value_index].ref;
							break;
						case 7: /* iTasks.UI.JS.Interface: initialize_client in wrapInitUIFunction */
							var array=ABC.memory_array[asp/4]+24;
							ABC.addresses.JSInt=      ABC.memory_array[ABC.memory_array[array/4]/4];
							ABC.addresses.JSBool=     ABC.memory_array[ABC.memory_array[array/4+2]/4];
							ABC.addresses.JSString=   ABC.memory_array[ABC.memory_array[array/4+4]/4];
							ABC.addresses.JSReal=     ABC.memory_array[ABC.memory_array[array/4+6]/4];
							ABC.addresses.JSNull=     ABC.memory_array[ABC.memory_array[array/4+8]/4];
							ABC.addresses.JSUndefined=ABC.memory_array[ABC.memory_array[array/4+10]/4];
							ABC.addresses.JSArray=    ABC.memory_array[ABC.memory_array[array/4+12]/4];
							ABC.addresses.JSRef=      ABC.memory_array[ABC.memory_array[array/4+14]/4];
							ABC.addresses.JSCleanRef= ABC.memory_array[ABC.memory_array[array/4+16]/4];
							ABC.util.instance.exports.set_js_ref_constructor(ABC.addresses.JSRef);
							ABC.initialized=true;
							break;
						case 10: /* iTasks.UI.JS.Interface: add CSS */
							var url=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var css=document.createElement('link');
							css.rel='stylesheet';
							css.type='text/css';
							css.async=true;
							css.href=url;
							document.head.appendChild(css);
							break;
						case 11: /* iTasks.UI.JS.Interface: add JS */
							var url=ABC.get_clean_string(ABC.memory_array[asp/4]);
							var callback=ABC.get_clean_string(ABC.memory_array[asp/4-2]);
							var js=document.createElement('script');
							js.type='text/javascript';
							js.async=false;
							if (callback.length>0)
								js.onload=Function(callback+'();');
							document.head.appendChild(js);
							js.src=url;
							break;
						default:
							throw new ABCError('unknown instruction',arg);
					}
					return pc+16;
				}
				return 0;
			},
			illegal_instr: function (addr, instr) {
				ABC.empty_log_buffer();
				if (abc_instructions[instr]=='instruction')
					/* `instruction 0` ends the interpretation, so this is no error */
					return;
				throw new ABCError('illegal instruction',instr);
			},
			out_of_memory: function () {
				ABC.empty_log_buffer();
				throw new ABCError('out of memory');
			},
			gc: util.instance.exports.gc,
			halt: function (pc, hp_free, hp_size) {
				ABC.empty_log_buffer();
				throw new ABCError('halt');
			},

			memcpy: util.instance.exports.memcpy,
			strncmp: util.instance.exports.strncmp,

			putchar: function (v) {
				ABC.log(String.fromCharCode(v));
			},
			print_int: function (high,low) {
				if ((high==0 && low>=0) || (high==-1 && low<0)) {
					ABC.log(low);
				} else if (typeof BigInt!='undefined') {
					var n=BigInt(high)*BigInt(2)**BigInt(32);
					if (low<0) {
						n+=BigInt(2)**BigInt(31);
						low+=2**31;
					}
					n+=BigInt(low);
					ABC.log(n);
				} else {
					console.warn('ABC.log: truncating 64-bit integer because this browser has no BigInt');
					ABC.log(low);
				}
			},
			print_bool: function (v) {
				ABC.log(v==0 ? 'False' : 'True');
			},
			print_char: function (v) {
				ABC.log("'"+String.fromCharCode(v)+"'");
			},
			print_real: function (v) {
				ABC.log(Number(0+v).toLocaleString(
					['en-US'],
					{
						useGrouping: false,
						maximumSignificantDigits: 15,
					}
				));
			},

			powR: Math.pow,
			acosR: Math.acos,
			asinR: Math.asin,
			atanR: Math.atan,
			cosR: Math.cos,
			sinR: Math.sin,
			tanR: Math.tan,
			expR: Math.exp,
			lnR: Math.log,
			log10R: Math.log10,
			RtoAC_words_needed: function(v) {
				v=Number(0+v).toLocaleString(
					['en-US'],
					{
						useGrouping: false,
						maximumSignificantDigits: 15,
					}
				);
				return 2+((v.length+7)>>3);
			},
			RtoAC: function (dest, v) {
				v=Number(0+v).toLocaleString(
					['en-US'],
					{
						useGrouping: false,
						maximumSignificantDigits: 15,
					}
				);
				ABC.memory_array[dest/4]=6*8+2; // __STRING__
				ABC.memory_array[dest/4+1]=0;
				ABC.memory_array[dest/4+2]=v.length;
				ABC.memory_array[dest/4+3]=0;
				var arr=new Uint8Array(ABC.memory_array.buffer, dest+16);
				for (var i=0; i<v.length; i++)
					arr[i]=v.charCodeAt(i);
				return dest+16+(((v.length+7)>>3)<<3);
			},
		}
	};

	return WebAssembly.instantiateStreaming(
		fetch('js/abc-interpreter.wasm'),
		interpreter_imports);
}).then(function(intp){
	ABC.interpreter=intp;

	const asp=4*ABC.prog.length;
	const bsp=asp+ABC.stack_size;
	const csp=asp+ABC.stack_size/2;
	const hp=bsp+8;

	ABC.util.instance.exports.setup_gc(hp, ABC.hp_size, asp, 97*8);

	ABC.interpreter.instance.exports.set_asp(asp);
	ABC.interpreter.instance.exports.set_bsp(bsp);
	ABC.interpreter.instance.exports.set_csp(csp);
	ABC.interpreter.instance.exports.set_hp(hp);
	ABC.interpreter.instance.exports.set_hp_free(ABC.hp_size/8);
	ABC.interpreter.instance.exports.set_hp_size(ABC.hp_size);

	ABC.interpret=function (f, args) {
		var asp=ABC.interpreter.instance.exports.get_asp();
		const old_asp=asp;
		var hp=ABC.interpreter.instance.exports.get_hp();
		var hp_free=ABC.interpreter.instance.exports.get_hp_free();

		/* NB: the order here matters: copy_js_to_clean may trigger garbage
		 * collection, so do that first, then set the rest of the arguments and
		 * update asp. */
		const copied=ABC.copy_js_to_clean(args, asp+8, hp, hp_free);
		ABC.memory_array[asp/4]=(31+17*2)*8; // JSWorld: INT 17
		ABC.memory_array[asp/4+4]=ABC.shared_clean_values[f.shared_clean_value_index].ref;
		ABC.interpreter.instance.exports.set_asp(asp+16);

		hp=copied.hp;
		hp_free=copied.hp_free;

		var csp=ABC.interpreter.instance.exports.get_csp();
		ABC.memory_array[csp/4]=659*8; // instruction 0; to return
		csp+=8;

		const old_pc=ABC.interpreter.instance.exports.get_pc();
		ABC.interpreter.instance.exports.set_pc(100*8); // jmp_ap2
		ABC.interpreter.instance.exports.set_csp(csp);
		ABC.interpreter.instance.exports.set_hp(hp);
		ABC.interpreter.instance.exports.set_hp_free(hp_free);

		try {
			ABC.interpreter.instance.exports.interpret();
		} catch (e) {
			if (e.constructor.name!='ABCError' &&
					(e.fileName!='abc-interpreter.js' || e.lineNumber>700))
				throw e;

			var trace=[e.message, '\n'];
			trace.push('  {0}', ABC.interpreter.instance.exports.get_pc()/8-ABC.code_offset,'\n');
			var csp=ABC.interpreter.instance.exports.get_csp();
			for (var i=1; i<=ABC_TRACE_LENGTH; i++) {
				var addr=ABC.memory_array[csp/4];
				if (addr==0)
					break;
				trace.push('  {'+i+'}',addr/8-ABC.code_offset,'\n');
				csp-=8;
			}
			console.error.apply(null,trace);

			throw e.toString();
		}

		ABC.interpreter.instance.exports.set_pc(old_pc);
		ABC.interpreter.instance.exports.set_asp(old_asp);
	};

	delete ABC.prog;
});
