"use strict";

var ABC=null;

const ABC_loading_promise=ABCInterpreter.instantiate({
	bytecode_path: '/js/app.pbc',

	heap_size: 8<<20,
	stack_size: 512<<10,

	encoding: 'utf-8',

	util_imports: {
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
	},

	interpreter_imports: {
		handle_illegal_instr: function (pc, instr, asp, bsp, csp, hp, hp_free) {
			if (ABCInterpreter.instructions[instr]=='instruction') {
				const arg=ABC.memory_array[(pc+8)/4];
				switch (arg) {
					case 0: /* evaluation finished */
						return 0;
					case 1: /* iTasks.UI.JS.Interface: set_js */
						var v=ABC.get_clean_string(ABC.memory_array[asp/4], true);
						var x=ABC.get_clean_string(ABC.memory_array[asp/4-2], true);
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
						var string=ABC.get_clean_string(ABC.memory_array[asp/4], true);
						if (ABC_DEBUG)
							console.log('eval',string);
						Function(string)();
						break;
					case 3: /* iTasks.UI.JS.Interface: eval_js_with_return_value */
						var string=ABC.get_clean_string(ABC.memory_array[asp/4], true);
						if (ABC_DEBUG)
							console.log('eval',string);
						var result=eval('('+string+')'); // the parentheses are needed for {}, for instance
						var copied=ABC.copy_js_to_clean(result, asp);
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
						var hp_ptr=ABC.memory_array[asp/4];
						ABC.memory_array[asp/4]=ABC.deserialize_from_unique_string(hp_ptr);
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
						var url=ABC.get_clean_string(ABC.memory_array[asp/4], false);
						var css=document.createElement('link');
						css.rel='stylesheet';
						css.type='text/css';
						css.async=true;
						css.href=url;
						document.head.appendChild(css);
						break;
					case 11: /* iTasks.UI.JS.Interface: add JS */
						var url=ABC.get_clean_string(ABC.memory_array[asp/4], false);
						var callback=ABC.get_clean_string(ABC.memory_array[asp/4-2], true);
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
			if (ABCInterpreter.instructions[instr]=='instruction')
				/* `instruction 0` ends the interpretation, so this is no error */
				return;
			throw new ABCError('illegal instruction',instr);
		},
	},
}).then(function(instance){
	ABC=instance;
	ABC.initialized=false;

	// Overwrite ap to return a result (in the case of jsWrapFunWithResult)
	ABC.ap=function(index){
		var f=function () {
			var args=[];
			for (var i=0; i<arguments.length; i++)
				args[i]=arguments[i];
			ABC.interpret(new SharedCleanValue(index), args);

			var result=undefined;
			const new_asp=ABC.interpreter.instance.exports.get_asp();
			const hp_ptr=ABC.memory_array[new_asp/4];
			if (ABC.memory_array[hp_ptr/4]!=25*8+2) { // INT, i.e. JSWorld
				// Assume we have received a tuple with the first element as the result
				const str_ptr=ABC.memory_array[hp_ptr/4+2];
				const string=ABC.get_clean_string(ABC.memory_array[str_ptr/4+2], false);
				if (ABC_DEBUG)
					console.log('result:',string);
				result=eval('('+string+')');
			}

			if (typeof result!='undefined')
				return result;
		};
		f.shared_clean_value_index=index;
		return f;
	};
});
