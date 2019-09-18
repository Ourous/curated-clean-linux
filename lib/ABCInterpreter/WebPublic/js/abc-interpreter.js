"use strict";

var ABC_DEBUG=false;
var ABC_TRACE_LENGTH=50;

class ABCError extends Error {
	constructor(desc,arg) {
		super(desc);

		switch (desc) {
			case 'illegal instruction':
				this.message+=' '+arg+' ('+ABCInterpreter.instructions[arg]+')';
				break;
			case 'unknown instruction':
				this.message+=' '+arg;
				break;
		}
	}
}

class SharedCleanValue {
	constructor (index) {
		this.shared_clean_value_index=index;
	}
}

class ABCInterpreter {
	// Just to setup properties. New instances should be created with the static
	// method instantiate() below.
	constructor () {
		this.memory=null;
		this.memory_array=null;

		this.start=null;
		this.code_offset=null;

		this.stack_size=null;
		this.heap_size=null;

		this.encoding=null;

		this.util=null;
		this.interpreter=null;

		this.log_buffer='';

		this.js=[]; // javascript objects accessible from Clean
		this.empty_js_values=[]; // empty indexes in the above array
		this.addresses={
			JSInt:       0,
			JSBool:      0,
			JSString:    0,
			JSReal:      0,
			JSNull:      0,
			JSUndefined: 0,
			JSArray:     0,
			JSRef:       0,
			JSCleanRef:  0,
		};

		this.shared_clean_values=[]; // pointers to the Clean heap
		this.empty_shared_clean_values=[]; // empty indexes in the above array
	}

	log (s) {
		s=String(s);
		this.log_buffer+=s;
		if (s.indexOf('\n')>=0) {
			var lines=this.log_buffer.split('\n');
			for (var i=0; i<lines.length-1; i++)
				console.log(lines[i]);
			this.log_buffer=lines[lines.length-1];
		}
	}
	empty_log_buffer () {
		if (this.log_buffer.length>0)
			console.log(this.log_buffer);
	}

	require_hp (needed_words) {
		var free_words = this.interpreter.instance.exports.get_hp_free();

		// Each gc iteration may cause frees on the JS side , which may in turn
		// free more nodes in Clean. Therefore we run gc as long as the number of
		// free words decreases or until there is enough space. It will be possible
		// to do this much neater in the future when JS has weak references /
		// finalizers and/or when WebAssembly has GC access.
		while (free_words < needed_words) {
			console.warn('gc from js');
			this.util.instance.exports.gc(this.interpreter.instance.exports.get_asp());

			var new_free_words=this.interpreter.instance.exports.get_hp_free();
			if (new_free_words<=free_words)
				throw new ABCError('out of memory');
			free_words=new_free_words;
		}
	}

	_deserialize (addr, size) {
		const old_hp=this.interpreter.instance.exports.get_hp();
		const new_hp=this.util.instance.exports.copy_from_string(
			addr,
			size,
			this.interpreter.instance.exports.get_asp()+8,
			this.interpreter.instance.exports.get_bsp()-8,
			old_hp,
			this.code_offset*8);
		this.interpreter.instance.exports.set_hp(new_hp);

		const new_hp_free=this.interpreter.instance.exports.get_hp_free()-(new_hp-old_hp)/8;
		if (new_hp_free<0)
			throw 'hp_free was '+new_hp_free+' after deserialize: '+string;

		this.interpreter.instance.exports.set_hp_free(new_hp_free);

		return this.memory_array[addr/4];
	}
	deserialize_from_unique_string (str_ptr) {
		const size=this.memory_array[str_ptr/4+2];
		this.require_hp(size/8*4); // rough upper bound

		return this._deserialize(str_ptr+16, size/8);
	}
	deserialize (string) {
		const max_words_needed=string.length/8*4; // rough upper bound
		this.require_hp(max_words_needed);

		var array=new Uint8Array(string.length);
		if (typeof string=='string') {
			for (var i=0; i<string.length; i++)
				array[i]=string.charCodeAt(i);
		} else {
			for (var i=0; i<string.length; i++)
				array[i]=string[i];
		}
		const graph=new Uint32Array(array.buffer);

		const unused_semispace=this.util.instance.exports.get_unused_semispace();
		for (var i=0; i<graph.length; i++)
			this.memory_array[unused_semispace/4+i]=graph[i];

		return this._deserialize(unused_semispace, graph.length/2);
	}

	share_js_value (obj) {
		if (this.empty_js_values.length > 0) {
			const i=this.empty_js_values.pop();
			if (this.js[i]!=undefined)
				throw 'internal error in ABCInterpreter.share_js_value';
			this.js[i]=obj;
			return i;
		} else {
			this.js.push(obj);
			return this.js.length-1;
		}
	}

	ap (index) { // create a JavaScript closure to call the interpreter
		const me=this;
		var f=function () {
			var args=[];
			for (var i=0; i<arguments.length; i++)
				args[i]=arguments[i];
			return me.interpret(new SharedCleanValue(index), args);
		};
		f.shared_clean_value_index=index;
		return f;
	}

	_copy_js_to_clean (values, store_ptrs, hp, hp_free) {
		for (var i=0; i<values.length; i++) {
			if (values[i]===null) {
				this.memory_array[store_ptrs/4]=this.addresses.JSNull-10;
			} else if (typeof values[i]=='undefined') {
				this.memory_array[store_ptrs/4]=this.addresses.JSUndefined-10;
			} else if (typeof values[i]=='number') {
				this.memory_array[store_ptrs/4]=hp;
				if (Number.isInteger(values[i])) {
					this.memory_array[hp/4]=this.addresses.JSInt;
					this.memory_array[hp/4+1]=0;
					if (values[i]>2**31 || values[i]<0-2**31) {
						if (typeof BigInt64Array!='undefined') {
							const bigint_array=new BigInt64Array(this.memory_array.buffer, hp+8);
							bigint_array[0]=BigInt(values[i]);
						} else {
							this.memory_array[hp/4+2]=values[i];
							this.memory_array[hp/4+3]=Math.floor(values[i] / 2**32); // NB: >> is 32-bit in JS, can't use it here
						}
					} else {
						this.memory_array[hp/4+2]=values[i];
						this.memory_array[hp/4+3]=0;
					}
				} else {
					this.memory_array[hp/4]=this.addresses.JSReal;
					this.memory_array[hp/4+1]=0;
					const float_array=new Float64Array(this.memory_array.buffer, hp+8);
					float_array[0]=values[i];
				}
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='boolean') {
				this.memory_array[store_ptrs/4]=hp;
				this.memory_array[hp/4]=this.addresses.JSBool;
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=values[i] ? 1 : 0;
				this.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='string') {
				this.memory_array[store_ptrs/4]=hp;
				this.memory_array[hp/4]=this.addresses.JSString;
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=hp+16;
				this.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
				this.memory_array[hp/4]=5*8+2; // _STRING_
				this.memory_array[hp/4+1]=0;
				var array;
				var length=values[i].length;
				switch (this.encoding) {
					case 'utf-8':
						if (typeof TextEncoder!='undefined') {
							var encoded=new TextEncoder().encode(values[i]);
							length=encoded.length;
							if (length%4) { // length must be divisible by 4 to cast to Uint32Array below
								array=new Uint8Array(((length+3)>>2)<<2);
								for (var j=0; j<length; j++)
									array[j]=encoded[j];
							} else {
								array=encoded;
							}
						}
						break;
					default:
						console.warn('copy_js_to_clean: this browser cannot encode text in '+this.encoding);
					case 'x-user-defined':
						array=new Uint8Array(((values[i].length+3)>>2)<<2);
						for (var j=0; j<values[i].length; j++)
							array[j]=values[i].charCodeAt(j);
						break;
				}
				this.memory_array[hp/4+2]=length;
				this.memory_array[hp/4+3]=0;
				array=new Uint32Array(array.buffer);
				for (var j=0; j<((length+3)>>2); j++)
					this.memory_array[hp/4+4+j]=array[j];
				hp+=16+(((length+7)>>3)<<3);
				hp_free-=2+((length+7)>>3);
			} else if (Array.isArray(values[i])) {
				this.memory_array[store_ptrs/4]=hp;
				// On the first run, we don't have the JSArray address yet, so we use
				// the dummy 2 to ensure that jsr_eval won't try to evaluate it. The
				// array elements are unwrapped immediately, so the constructor does
				// not matter (apart from the fact that the HNF bit is set).
				this.memory_array[hp/4]=this.initialized ? this.addresses.JSArray : 2;
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=hp+16;
				this.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
				this.memory_array[hp/4]=0*8+2; // _ARRAY_
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=values[i].length;
				this.memory_array[hp/4+3]=0;
				this.memory_array[hp/4+4]=0;
				this.memory_array[hp/4+5]=0;
				hp+=24;
				hp_free-=3+values[i].length;;
				var copied=this._copy_js_to_clean(values[i], hp, hp+8*values[i].length, hp_free);
				hp=copied.hp;
				hp_free=copied.hp_free;
			} else if ('shared_clean_value_index' in values[i]) {
				this.memory_array[store_ptrs/4]=hp;
				this.memory_array[hp/4]=this.addresses.JSCleanRef;
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=values[i].shared_clean_value_index;
				this.memory_array[hp/4+3]=0;
				hp+=16;
				hp_free-=2;
			} else if (typeof values[i]=='object' || typeof values[i]=='function') {
				this.memory_array[store_ptrs/4]=hp;
				this.memory_array[hp/4]=this.addresses.JSRef;
				this.memory_array[hp/4+1]=0;
				this.memory_array[hp/4+2]=this.share_js_value(values[i]);
				this.memory_array[hp/4+3]=0;
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
	}
	copied_node_size (value) {
		if (value===null)
			return 0;
		else if (typeof value=='undefined')
			return 0;
		else if (typeof value=='number')
			return 2;
		else if (typeof value=='boolean')
			return 2;
		else if (typeof value=='string') {
			var length=value.length;
			if (this.encoding=='utf-8' && typeof TextEncoder!='undefined')
				length=new TextEncoder().encode(value).length;
			return 2+2+((length+7)>>3);
		} else if (Array.isArray(value)) {
			var size=2+3+value.length;
			for (var i=0; i<value.length; i++)
				size+=this.copied_node_size(value[i]);
			return size;
		} else if ('shared_clean_value_index' in value)
			return 2;
		else if (typeof value=='object' || typeof value=='function')
			return 2;
		else {
			console.error('Cannot pass this JavaScript value to Clean:',value);
			throw new ABCError('missing case in copy_js_to_clean');
		}
	}
	copy_js_to_clean (value, store_ptrs) {
		const node_size=this.copied_node_size(value);
		this.require_hp(node_size);
		const hp=this.interpreter.instance.exports.get_hp();
		const hp_free=this.interpreter.instance.exports.get_hp_free();

		const result=this._copy_js_to_clean([value], store_ptrs, hp, hp_free);

		if (hp_free-result.hp_free!=node_size)
			console.warn('copied_node_size: expected',node_size,'; got',hp_free-result.hp_free,'for',value);

		return result;
	}

	share_clean_value (ref, component) {
		if (typeof component.shared_clean_values=='undefined')
			throw 'could not attach shared Clean value to an iTasks component';
		if (component.shared_clean_values==null)
			component.shared_clean_values=new Set();

		const record={ref: ref, component: component};
		var i=null;

		if (this.empty_shared_clean_values.length > 0) {
			i=this.empty_shared_clean_values.pop();
			if (this.shared_clean_values[i]!=null)
				throw 'internal error in share_clean_value';
			this.shared_clean_values[i]=record;
		} else {
			i=this.shared_clean_values.length;
			this.shared_clean_values.push(record);
		}

		component.shared_clean_values.add(i);

		return i;
	}
	clear_shared_clean_value (ref, update_component=true) {
		const component=this.shared_clean_values[ref].component;
		if (update_component && typeof component.shared_clean_values!='undefined')
			component.shared_clean_values.delete(ref);

		this.shared_clean_values[ref]=null;
		this.empty_shared_clean_values.push(ref);
	}

	get_clean_string (hp_ptr, string_may_be_discarded=false) {
		const size=this.memory_array[hp_ptr/4+2];

		if (string_may_be_discarded) {
			// Try to clean up the Clean heap by discarding the string sent to JS.
			const hp=this.interpreter.instance.exports.get_hp();
			const string_bytes=16+(((size+7)>>3)<<3);
			if (hp_ptr+string_bytes==hp) {
				// The string is at the end of the heap. Simply move the heap pointer back.
				this.interpreter.instance.exports.set_hp(hp_ptr);
				this.interpreter.instance.exports.set_hp_free(this.interpreter.instance.exports.get_hp_free()+string_bytes/8);
			} else {
				const asp=this.interpreter.instance.exports.get_asp();
				if (hp_ptr+string_bytes+24==hp && this.memory_array[asp/4-2]==hp-24) {
					this.memory_array[asp/4-2]=hp_ptr;
					this.interpreter.instance.exports.set_hp(hp_ptr+24);
					this.interpreter.instance.exports.set_hp_free(this.interpreter.instance.exports.get_hp_free()+string_bytes/8);
				} else if (ABC_DEBUG) {
					console.warn('get_clean_string: could not clean up heap:',hp_ptr,hp,string_bytes);
				}
			}
		}

		const string_buffer=new Uint8Array(this.memory.buffer, hp_ptr+16, size);
		if (typeof TextDecoder!='undefined') {
			return new TextDecoder(this.encoding).decode(string_buffer);
		} else {
			if (this.encoding!='x-user-defined')
				console.warn('get_clean_string: this browser does not have TextDecoder; string could not be decoded using '+this.encoding);
			var string='';
			for (var i=0; i<size; i++)
				string+=String.fromCharCode(string_buffer[i]);
			return string;
		}
	}

	get_trace () {
		var trace=['  {0}',this.interpreter.instance.exports.get_pc()/8-this.code_offset,'\n'];
		var csp=this.interpreter.instance.exports.get_csp();
		for (var i=1; i<=ABC_TRACE_LENGTH; i++) {
			var addr=this.memory_array[csp/4];
			if (addr==0)
				break;
			trace.push('  {'+i+'}',addr/8-this.code_offset,'\n');
			csp-=8;
		}
		return trace;
	}

	interpret (f,args) {
		throw new ABCError('the interpreter has not been initialized yet');
	}

	static instantiate (args) {
		const opts={
			bytecode_path: null,
			util_path: '/js/abc-interpreter-util.wasm',
			interpreter_path: '/js/abc-interpreter.wasm',

			stack_size: 512<<10,
			heap_size: 2<<20,

			util_imports: {},
			interpreter_imports: {},

			encoding: 'x-user-defined',
			fetch: path => fetch(path), // to be able to override
		};
		Object.assign(opts,args);

		const me=new ABCInterpreter();

		me.stack_size=opts.stack_size*2;
		me.heap_size=opts.heap_size;

		me.encoding=opts.encoding;

		return opts.fetch(opts.bytecode_path).then(function(resp){
			if (!resp.ok)
				throw new ABCError('failed to fetch bytecode');
			return resp.arrayBuffer();
		}).then(function(bytecode){
			const parse_prelinked_bytecode=function (prog, to_array=null) {
				var prog_offset=0;
				var words_needed=0;

				while (prog.length>0) {
					switch (prog[0]) {
						case 1: /* ST_Code */
							me.code_offset=words_needed;
						case 0: /* ST_Preamble */
						case 2: /* ST_Data */
							const words_in_section=prog[1]*2;
							if (to_array!=null)
								for (var k=0; k<words_in_section; k++)
									to_array[prog_offset+k]=prog[k+2];
							prog_offset+=words_in_section;
							words_needed+=prog[2];
							break;
						case 3: /* ST_Start */
							me.start=prog[2];
							break;
						default:
							throw new ABCError ('could not parse bytecode');
					}

					prog=prog.slice(2+2*prog[1]);
				}

				return words_needed;
			};

			bytecode=new Uint32Array(bytecode);

			me.words_needed_for_program=parse_prelinked_bytecode(bytecode);
			var data_size=me.stack_size+me.heap_size*2;
			if (data_size<bytecode.length/4)
				data_size=bytecode.length/4;
			const blocks_needed=Math.floor((me.words_needed_for_program*8 + data_size + 65535) / 65536);

			me.memory=new WebAssembly.Memory({initial: blocks_needed});
			me.memory_array=new Uint32Array(me.memory.buffer);

			parse_prelinked_bytecode(bytecode, new Uint32Array(me.memory.buffer,me.words_needed_for_program*8));

			const util_imports={
				clean: {
					memory: me.memory,

					has_host_reference: index => 0,
					update_host_reference: function (index, new_location) {
						throw new ABCError('update_host_reference should not be called')
					},

					gc_start: function() {
						me.active_js=[];
					},
					js_ref_found: function(ref) {
						me.active_js[ref]=true;
					},
					gc_end: function() {
						if (ABC_DEBUG)
							console.log(me.interpreter.instance.exports.get_hp_free(),'free words after gc');
						me.empty_js_values=[];
						// NB: we cannot reorder me.js, because garbage collection may be
						// triggered while computing a string to send to JavaScript which
						// can then contain illegal references.
						for (var i=0; i<me.js.length; i++) {
							if (typeof me.active_js[i]=='undefined') {
								delete me.js[i];
								me.empty_js_values.push(i);
							}
						}
						delete me.active_js;
					},

					set_hp: hp => me.interpreter.instance.exports.set_hp(hp),
					set_hp_free: free => me.interpreter.instance.exports.set_hp_free(free),

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
				}
			};
			Object.assign(util_imports.clean, opts.util_imports);

			return opts.fetch(opts.util_path)
				.then(response => response.arrayBuffer())
				.then(buffer => WebAssembly.instantiate(buffer, util_imports));
		}).then(function(util){
			me.util=util;

			me.util.instance.exports.decode_prelinked_bytecode(me.words_needed_for_program*8);

			const interpreter_imports={
				clean: {
					memory: me.memory,

					debug_instr: function (addr, instr) {
						if (ABC_DEBUG)
							console.log(addr/8-me.code_offset,ABCInterpreter.instructions[instr]);
					},
					handle_illegal_instr: (pc, instr, asp, bsp, csp, hp, hp_free) => 0,
					illegal_instr: function (addr, instr) {
						me.empty_log_buffer();
						throw new ABCError('illegal instruction',instr);
					},
					out_of_memory: function () {
						me.empty_log_buffer();
						throw new ABCError('out of memory');
					},
					gc: util.instance.exports.gc,
					halt: function (pc, hp_free, heap_size) {
						me.empty_log_buffer();
						throw new ABCError('halt');
					},

					memcpy: util.instance.exports.memcpy,
					strncmp: util.instance.exports.strncmp,

					putchar: function (v) {
						me.log(String.fromCharCode(v));
					},
					print_int: function (high,low) {
						if ((high==0 && low>=0) || (high==-1 && low<0)) {
							me.log(low);
						} else if (typeof BigInt!='undefined') {
							var n=BigInt(high)*BigInt(2)**BigInt(32);
							if (low<0) {
								n+=BigInt(2)**BigInt(31);
								low+=2**31;
							}
							n+=BigInt(low);
							me.log(n);
						} else {
							console.warn('print_int: truncating 64-bit integer because this browser has no BigInt');
							me.log(low);
						}
					},
					print_bool: function (v) {
						me.log(v==0 ? 'False' : 'True');
					},
					print_char: function (v) {
						me.log("'"+String.fromCharCode(v)+"'");
					},
					print_real: function (v) {
						me.log(Number(0+v).toPrecision(15).replace(/\.?0*$/,''));
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
						v=Number(0+v).toPrecision(15).replace(/\.?0*$/,'');
						return 2+((v.length+7)>>3);
					},
					RtoAC: function (dest, v) {
						v=Number(0+v).toPrecision(15).replace(/\.?0*$/,'');
						me.memory_array[dest/4]=5*8+2; // __STRING__
						me.memory_array[dest/4+1]=0;
						me.memory_array[dest/4+2]=v.length;
						me.memory_array[dest/4+3]=0;
						const arr=new Uint8Array(me.memory_array.buffer, dest+16);
						for (var i=0; i<v.length; i++)
							arr[i]=v.charCodeAt(i);
						return dest+16+(((v.length+7)>>3)<<3);
					},
				}
			};
			Object.assign(interpreter_imports.clean, opts.interpreter_imports);

			return opts.fetch(opts.interpreter_path)
				.then(response => response.arrayBuffer())
				.then(bytes => WebAssembly.instantiate(bytes, interpreter_imports));
		}).then(function(intp){
			me.interpreter=intp;

			const asp=Math.floor((me.words_needed_for_program*8+7)/8)*8;
			delete me.words_needed_for_program;
			const bsp=asp+me.stack_size;
			const csp=asp+me.stack_size/2;
			const hp=bsp+8;

			me.util.instance.exports.setup_gc(hp, me.heap_size, asp, 96*8);

			me.interpreter.instance.exports.set_asp(asp);
			me.interpreter.instance.exports.set_bsp(bsp);
			me.interpreter.instance.exports.set_csp(csp);
			me.interpreter.instance.exports.set_hp(hp);
			me.interpreter.instance.exports.set_hp_free(me.heap_size/8);
			me.interpreter.instance.exports.set_hp_size(me.heap_size/8);

			me.interpret=function (f, args) {
				const asp=me.interpreter.instance.exports.get_asp();
				const old_asp=asp;
				var hp=me.interpreter.instance.exports.get_hp();
				var hp_free=me.interpreter.instance.exports.get_hp_free();

				/* NB: the order here matters: copy_js_to_clean may trigger garbage
				 * collection, so do that first, then set the rest of the arguments and
				 * update asp. */
				const copied=me.copy_js_to_clean(args, asp+8);
				me.memory_array[asp/4]=(30+17*2)*8; // JSWorld: INT 17
				me.memory_array[asp/4+4]=me.shared_clean_values[f.shared_clean_value_index].ref;
				me.interpreter.instance.exports.set_asp(asp+16);

				hp=copied.hp;
				hp_free=copied.hp_free;

				var csp=me.interpreter.instance.exports.get_csp();
				me.memory_array[csp/4]=658*8; // instruction 0; to return
				csp+=8;

				const old_pc=me.interpreter.instance.exports.get_pc();
				me.interpreter.instance.exports.set_pc(99*8); // jmp_ap2
				me.interpreter.instance.exports.set_csp(csp);
				me.interpreter.instance.exports.set_hp(hp);
				me.interpreter.instance.exports.set_hp_free(hp_free);

				try {
					me.interpreter.instance.exports.interpret();
				} catch (e) {
					if (e.constructor.name!='ABCError' &&
							(e.fileName!='abc-interpreter.js' || e.lineNumber>700))
						throw e;

					var trace=[e.message, '\n'].concat(me.get_trace());
					console.error.apply(null,trace);

					throw e.toString();
				}

				me.interpreter.instance.exports.set_pc(old_pc);
				me.interpreter.instance.exports.set_asp(old_asp);
			};

			return me;
		});
	}
}

if (typeof module!='undefined') module.exports={
	ABC_DEBUG: ABC_DEBUG,
	ABCError: ABCError,
	SharedCleanValue: SharedCleanValue,
	ABCInterpreter: ABCInterpreter,
};
ABCInterpreter.instructions=[
	"absR",
	"acosR",
	"addI",
	"addIo",
	"addLU",
	"addR",
	"add_empty_node2",
	"add_empty_node3",
	"add_empty_node4",
	"add_empty_node5",
	"add_empty_node6",
	"add_empty_node7",
	"add_empty_node8",
	"add_empty_node9",
	"add_empty_node10",
	"add_empty_node11",
	"add_empty_node12",
	"add_empty_node13",
	"add_empty_node14",
	"add_empty_node15",
	"add_empty_node16",
	"add_empty_node17",
	"add_empty_node18",
	"add_empty_node19",
	"add_empty_node20",
	"add_empty_node21",
	"add_empty_node22",
	"add_empty_node23",
	"add_empty_node24",
	"add_empty_node25",
	"add_empty_node26",
	"add_empty_node27",
	"add_empty_node28",
	"add_empty_node29",
	"add_empty_node30",
	"add_empty_node31",
	"add_empty_node32",
	"andI",
	"asinR",
	"atanR",
	"build",
	"build0",
	"build1",
	"build2",
	"build3",
	"build4",
	"buildAC",
	"buildh",
	"buildh0",
	"buildh1",
	"buildh2",
	"buildh3",
	"buildh4",
	"buildhr01",
	"buildhr02",
	"buildhr03",
	"buildhr04",
	"buildhr10",
	"buildhr11",
	"buildhr12",
	"buildhr13",
	"buildhr1b",
	"buildhr20",
	"buildhr21",
	"buildhr22",
	"buildhr30",
	"buildhr31",
	"buildhr40",
	"build_node_rtn",
	"build_node2_rtn",
	"build_r",
	"build_ra0",
	"build_ra1",
	"build_r0b",
	"buildBFALSE",
	"buildBTRUE",
	"buildB_b",
	"buildC",
	"buildC_b",
	"buildF_b",
	"buildI",
	"buildI_b",
	"buildR",
	"buildR_b",
	"buildhr",
	"buildhra0",
	"buildhra1",
	"buildhr0b",
	"build_r01",
	"build_r02",
	"build_r03",
	"build_r04",
	"build_r10",
	"build_r11",
	"build_r12",
	"build_r13",
	"build_r1b",
	"build_r20",
	"build_r21",
	"build_r30",
	"build_r31",
	"build_r40",
	"build_u",
	"build_u01",
	"build_u02",
	"build_u03",
	"build_u0b",
	"build_u11",
	"build_u12",
	"build_u13",
	"build_u1b",
	"build_u21",
	"build_u22",
	"build_u2b",
	"build_u31",
	"build_ua1",
	"catAC",
	"ccall",
	"centry",
	"cmpAC",
	"cosR",
	"create",
	"creates",
	"create_array",
	"create_arrayBOOL",
	"create_arrayCHAR",
	"create_arrayINT",
	"create_arrayREAL",
	"create_array_",
	"create_array_BOOL",
	"create_array_CHAR",
	"create_array_INT",
	"create_array_REAL",
	"create_array_r",
	"create_array_r_",
	"create_array_r_a",
	"create_array_r_b",
	"decI",
	"divI",
	"divLU",
	"divR",
	"entierR",
	"eqAC",
	"eqAC_a",
	"eqB",
	"eqB_aFALSE",
	"eqB_aTRUE",
	"eqB_bFALSE",
	"eqB_bTRUE",
	"eqC",
	"eqC_a",
	"eqC_b",
	"eqCc",
	"eqD_b",
	"eq_desc",
	"eq_desc_b",
	"eq_nulldesc",
	"eqI",
	"eqI_a",
	"eqI_b",
	"eqIi",
	"eqR",
	"eqR_b",
	"expR",
	"fill",
	"fill0",
	"fill1_r0101",
	"fill1_r0111",
	"fill1_r02001",
	"fill1_r02010",
	"fill1_r02011",
	"fill1_r02101",
	"fill1_r02110",
	"fill1_r02111",
	"fill1_r11001",
	"fill1_r11011",
	"fill1_r11101",
	"fill1_r11111",
	"fill1_r20111",
	"fill1",
	"fill1001",
	"fill1010",
	"fill1011",
	"fill1101",
	"fill2",
	"fill2a001",
	"fill2a002",
	"fill2a011",
	"fill2a012",
	"fill2ab011",
	"fill2ab013",
	"fill2ab002",
	"fill2ab003",
	"fill2b001",
	"fill2b002",
	"fill2b011",
	"fill2b012",
	"fill2_r00",
	"fill2_r01",
	"fill2_r10",
	"fill2_r11",
	"fill3",
	"fill3a10",
	"fill3a11",
	"fill3a12",
	"fill3aaab13",
	"fill3_r",
	"fill3_r01a",
	"fill3_r01b",
	"fill4",
	"fillcaf",
	"fillh",
	"fillh0",
	"fillh1",
	"fillh2",
	"fillh3",
	"fillh4",
	"fillB_b",
	"fillC_b",
	"fillF_b",
	"fillI",
	"fillI_b",
	"fillR_b",
	"fill_a",
	"fill_r",
	"fill_r01",
	"fill_r02",
	"fill_r03",
	"fill_r0b",
	"fill_r10",
	"fill_r11",
	"fill_r12",
	"fill_r13",
	"fill_r1b",
	"fill_r20",
	"fill_r21",
	"fill_r22",
	"fill_r30",
	"fill_r31",
	"fill_r40",
	"fill_ra0",
	"fill_ra1",
	"fill_u",
	"get_desc_arity_offset",
	"get_node_arity",
	"get_thunk_desc",
	"gtI",
	"halt",
	"incI",
	"instruction",
	"is_record",
	"jmp",
	"jmp_eval",
	"jmp_eval_upd",
	"jmp_false",
	"jmp_i",
	"jmp_i0",
	"jmp_i1",
	"jmp_i2",
	"jmp_i3",
	"jmp_true",
	"jsr",
	"jsr_eval",
	"jsr_eval0",
	"jsr_eval1",
	"jsr_eval2",
	"jsr_eval3",
	"jsr_i",
	"jsr_i0",
	"jsr_i1",
	"jsr_i2",
	"jsr_i3",
	"lnR",
	"load_i",
	"load_module_name",
	"load_si16",
	"load_si32",
	"load_ui8",
	"log10R",
	"ltC",
	"ltI",
	"ltR",
	"ltU",
	"mulI",
	"mulIo",
	"mulR",
	"mulUUL",
	"negI",
	"negR",
	"notB",
	"notI",
	"orI",
	"pop_a",
	"pop_b",
	"powR",
	"print",
	"printD",
	"print_char",
	"print_int",
	"print_real",
	"print_string",
	"print_symbol_sc",
	"pushcaf",
	"pushcaf10",
	"pushcaf11",
	"pushcaf20",
	"pushcaf31",
	"pushA_a",
	"pushBFALSE",
	"pushBTRUE",
	"pushB_a",
	"pushB0_pop_a1",
	"pushC",
	"pushC_a",
	"pushC0_pop_a1",
	"pushD",
	"pushD_a",
	"pushF_a",
	"pushI",
	"pushI_a",
	"pushI0_pop_a1",
	"pushL",
	"pushLc",
	"pushR",
	"pushR_a",
	"push_a",
	"push_a_r_args",
	"push_arg",
	"push_arg1",
	"push_arg2",
	"push_arg2l",
	"push_arg3",
	"push_arg4",
	"push_arg_b",
	"push_args",
	"push_args1",
	"push_args2",
	"push_args3",
	"push_args4",
	"push_args_u",
	"push_array",
	"push_arraysize",
	"push_a_b",
	"push_b",
	"push_finalizers",
	"push_node",
	"push_node0",
	"push_node1",
	"push_node2",
	"push_node3",
	"push_node4",
	"push_node_",
	"push_node_u",
	"push_node_u01",
	"push_node_u02",
	"push_node_u03",
	"push_node_u0b",
	"push_node_u11",
	"push_node_u12",
	"push_node_u13",
	"push_node_u1b",
	"push_node_u21",
	"push_node_u22",
	"push_node_u31",
	"push_node_ua1",
	"push_r_arg_D",
	"push_r_arg_t",
	"push_r_args",
	"push_r_args01",
	"push_r_args02",
	"push_r_args03",
	"push_r_args04",
	"push_r_args0b",
	"push_r_args10",
	"push_r_args11",
	"push_r_args12",
	"push_r_args13",
	"push_r_args1b",
	"push_r_args20",
	"push_r_args21",
	"push_r_args22",
	"push_r_args30",
	"push_r_args31",
	"push_r_args40",
	"push_r_argsa0",
	"push_r_argsa1",
	"push_r_args_a",
	"push_r_args_a1",
	"push_r_args_a2l",
	"push_r_args_a3",
	"push_r_args_a4",
	"push_r_args_aa1",
	"push_r_args_b",
	"push_r_args_b0b11",
	"push_r_args_b0221",
	"push_r_args_b1",
	"push_r_args_b1111",
	"push_r_args_b2l1",
	"push_r_args_b31",
	"push_r_args_b41",
	"push_r_args_b1l2",
	"push_r_args_b2",
	"push_r_args_b22",
	"push_t_r_a",
	"push_t_r_args",
	"replace",
	"replaceBOOL",
	"replaceCHAR",
	"replaceINT",
	"replaceREAL",
	"replace_r",
	"repl_args",
	"repl_args1",
	"repl_args2",
	"repl_args3",
	"repl_args4",
	"repl_args_b",
	"repl_r_args",
	"repl_r_args01",
	"repl_r_args02",
	"repl_r_args03",
	"repl_r_args04",
	"repl_r_args0b",
	"repl_r_args10",
	"repl_r_args11",
	"repl_r_args12",
	"repl_r_args13",
	"repl_r_args14",
	"repl_r_args1b",
	"repl_r_args20",
	"repl_r_args21",
	"repl_r_args22",
	"repl_r_args23",
	"repl_r_args24",
	"repl_r_args2b",
	"repl_r_args30",
	"repl_r_args31",
	"repl_r_args32",
	"repl_r_args33",
	"repl_r_args34",
	"repl_r_args3b",
	"repl_r_args40",
	"repl_r_argsa0",
	"repl_r_argsa1",
	"repl_r_args_a",
	"repl_r_args_aab11",
	"repl_r_args_a2021",
	"repl_r_args_a21",
	"repl_r_args_a31",
	"repl_r_args_a41",
	"repl_r_args_aa1",
	"remI",
	"rtn",
	"select",
	"selectBOOL",
	"selectCHAR",
	"selectINT",
	"selectREAL",
	"select_r",
	"select_r01",
	"select_r02",
	"select_r03",
	"select_r04",
	"select_r0b",
	"select_r10",
	"select_r11",
	"select_r12",
	"select_r13",
	"select_r14",
	"select_r1b",
	"select_r20",
	"select_r21",
	"select_r22",
	"select_r23",
	"select_r24",
	"select_r2b",
	"set_finalizers",
	"shiftlI",
	"shiftrI",
	"shiftrU",
	"sinR",
	"sliceAC",
	"subI",
	"subIo",
	"subLU",
	"subR",
	"sqrtR",
	"tanR",
	"testcaf",
	"update",
	"updateAC",
	"updateBOOL",
	"updateCHAR",
	"updateINT",
	"updateREAL",
	"updatepop_a",
	"updatepop_b",
	"update_a",
	"update_b",
	"update_r",
	"update_r01",
	"update_r02",
	"update_r03",
	"update_r04",
	"update_r0b",
	"update_r10",
	"update_r11",
	"update_r12",
	"update_r13",
	"update_r14",
	"update_r1b",
	"update_r20",
	"update_r21",
	"update_r22",
	"update_r23",
	"update_r24",
	"update_r2b",
	"update_r30",
	"update_r31",
	"update_r32",
	"update_r33",
	"update_r34",
	"update_r3b",
	"xorI",
	"BtoAC",
	"CtoAC",
	"ItoAC",
	"ItoC",
	"ItoR",
	"RtoAC",
	"RtoI",
	"jmp_ap1",
	"jsr_ap1",
	"jmp_ap2",
	"jsr_ap2",
	"jmp_ap3",
	"jsr_ap3",
	"jmp_ap4",
	"jsr_ap4",
	"jmp_ap5",
	"jsr_ap5",
	"jmp_ap6",
	"jsr_ap6",
	"jmp_ap7",
	"jsr_ap7",
	"jmp_ap8",
	"jsr_ap8",
	"jmp_ap9",
	"jsr_ap9",
	"jmp_ap10",
	"jsr_ap10",
	"jmp_ap11",
	"jsr_ap11",
	"jmp_ap12",
	"jsr_ap12",
	"jmp_ap13",
	"jsr_ap13",
	"jmp_ap14",
	"jsr_ap14",
	"jmp_ap15",
	"jsr_ap15",
	"jmp_ap16",
	"jsr_ap16",
	"jmp_ap17",
	"jsr_ap17",
	"jmp_ap18",
	"jsr_ap18",
	"jmp_ap19",
	"jsr_ap19",
	"jmp_ap20",
	"jsr_ap20",
	"jmp_ap21",
	"jsr_ap21",
	"jmp_ap22",
	"jsr_ap22",
	"jmp_ap23",
	"jsr_ap23",
	"jmp_ap24",
	"jsr_ap24",
	"jmp_ap25",
	"jsr_ap25",
	"jmp_ap26",
	"jsr_ap26",
	"jmp_ap27",
	"jsr_ap27",
	"jmp_ap28",
	"jsr_ap28",
	"jmp_ap29",
	"jsr_ap29",
	"jmp_ap30",
	"jsr_ap30",
	"jmp_ap31",
	"jsr_ap31",
	"jmp_ap32",
	"jsr_ap32",
	"add_arg",
	"add_arg0",
	"add_arg1",
	"add_arg2",
	"add_arg3",
	"add_arg4",
	"add_arg5",
	"add_arg6",
	"add_arg7",
	"add_arg8",
	"add_arg9",
	"add_arg10",
	"add_arg11",
	"add_arg12",
	"add_arg13",
	"add_arg14",
	"add_arg15",
	"add_arg16",
	"add_arg17",
	"add_arg18",
	"add_arg19",
	"add_arg20",
	"add_arg21",
	"add_arg22",
	"add_arg23",
	"add_arg24",
	"add_arg25",
	"add_arg26",
	"add_arg27",
	"add_arg28",
	"add_arg29",
	"add_arg30",
	"add_arg31",
	"add_arg32",
	"eval_upd0",
	"eval_upd1",
	"eval_upd2",
	"eval_upd3",
	"eval_upd4",
	"eval_upd5",
	"eval_upd6",
	"eval_upd7",
	"eval_upd8",
	"eval_upd9",
	"eval_upd10",
	"eval_upd11",
	"eval_upd12",
	"eval_upd13",
	"eval_upd14",
	"eval_upd15",
	"eval_upd16",
	"eval_upd17",
	"eval_upd18",
	"eval_upd19",
	"eval_upd20",
	"eval_upd21",
	"eval_upd22",
	"eval_upd23",
	"eval_upd24",
	"eval_upd25",
	"eval_upd26",
	"eval_upd27",
	"eval_upd28",
	"eval_upd29",
	"eval_upd30",
	"eval_upd31",
	"eval_upd32",
	"fill_a01_pop_rtn",
	"swap_a1",
	"swap_a2",
	"swap_a3",
	"swap_a",
	"closeF",
	"endF",
	"endSF",
	"errorF",
	"flushF",
	"openF",
	"openSF",
	"positionF",
	"positionSF",
	"readFC",
	"readFI",
	"readFR",
	"readFS",
	"readFString",
	"readLineF",
	"readLineSF",
	"readSFC",
	"readSFI",
	"readSFR",
	"readSFS",
	"reopenF",
	"seekF",
	"seekSF",
	"shareF",
	"stderrF",
	"stdioF",
	"writeFC",
	"writeFI",
	"writeFR",
	"writeFS",
	"writeFString",
	"addIi",
	"andIi",
	"andIio",
	"buildh0_dup_a",
	"buildh0_dup2_a",
	"buildh0_dup3_a",
	"buildh0_put_a",
	"buildh0_put_a_jsr",
	"buildho2",
	"buildo1",
	"buildo2",
	"dup_a",
	"dup2_a",
	"dup3_a",
	"exchange_a",
	"geC",
	"jmp_b_false",
	"jmp_eqACio",
	"jmp_eqC_b",
	"jmp_eqC_b2",
	"jmp_eqCc",
	"jmp_eqD_b",
	"jmp_eqD_b2",
	"jmp_eqI",
	"jmp_eqI_b",
	"jmp_eqI_b2",
	"jmp_eqIi",
	"jmp_eq_desc",
	"jmp_geI",
	"jmp_ltI",
	"jmp_neC_b",
	"jmp_neCc",
	"jmp_neI",
	"jmp_neI_b",
	"jmp_neIi",
	"jmp_ne_desc",
	"jmp_o_geI",
	"jmp_o_geI_arraysize_a",
	"ltIi",
	"neI",
	"swap_b1",
	"pop_a_jmp",
	"pop_a_jsr",
	"pop_a_rtn",
	"pop_ab_rtn",
	"pop_b_jmp",
	"pop_b_jsr",
	"pop_b_pushBFALSE",
	"pop_b_pushBTRUE",
	"pop_b_rtn",
	"pushD_a_jmp_eqD_b2",
	"push_a_jsr",
	"push_b_decI",
	"push_b_incI",
	"push_b_jsr",
	"push_arraysize_a",
	"push_jsr_eval",
	"push_a2",
	"push_ab",
	"push_b2",
	"push2_a",
	"push2_b",
	"push3_a",
	"push3_b",
	"push_update_a",
	"put_a",
	"put_a_jmp",
	"put_b",
	"put_b_jmp",
	"selectBOOLoo",
	"selectCHARoo",
	"selectINToo",
	"selectREALoo",
	"selectoo",
	"update2_a",
	"update2_b",
	"update2pop_a",
	"update2pop_b",
	"update3_a",
	"update3_b",
	"update3pop_a",
	"update3pop_b",
	"update4_a",
	"updates2_a",
	"updates2_a_pop_a",
	"updates2_b",
	"updates2pop_a",
	"updates2pop_b",
	"updates3_a",
	"updates3_b",
	"updates3pop_a",
	"updates3pop_b",
	"updates4_a",
	"A_data_IIIla", /* Three instruction-width elems, a label-width elem, and an arity */
	"A_data_IIl",
	"A_data_IlI",
	"A_data_IlIla",
	"A_data_lIlI",
	"A_data_la",
	"A_data_a",
	"jsr_eval_host_node",
	"jsr_eval_host_node_1",
	"jsr_eval_host_node_2",
	"jsr_eval_host_node_3",
	"jsr_eval_host_node_4",
	"jsr_eval_host_node_5",
	"jsr_eval_host_node_6",
	"jsr_eval_host_node_7",
	"jsr_eval_host_node_8",
	"jsr_eval_host_node_9",
	"jsr_eval_host_node_10",
	"jsr_eval_host_node_11",
	"jsr_eval_host_node_12",
	"jsr_eval_host_node_13",
	"jsr_eval_host_node_14",
	"jsr_eval_host_node_15",
	"jsr_eval_host_node_16",
	"jsr_eval_host_node_17",
	"jsr_eval_host_node_18",
	"jsr_eval_host_node_19",
	"jsr_eval_host_node_20",
	"jsr_eval_host_node_21",
	"jsr_eval_host_node_22",
	"jsr_eval_host_node_23",
	"jsr_eval_host_node_24",
	"jsr_eval_host_node_25",
	"jsr_eval_host_node_26",
	"jsr_eval_host_node_27",
	"jsr_eval_host_node_28",
	"jsr_eval_host_node_29",
	"jsr_eval_host_node_30",
	"jsr_eval_host_node_31",
];
