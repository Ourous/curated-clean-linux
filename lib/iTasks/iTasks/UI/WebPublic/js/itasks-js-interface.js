//
"use strict";
//
//Small versions

function ___wrapJS(val){
	return [0,"JSVal",val];
}

function ___unwrapJS(ptr){
	return ptr[2];
}

//This function encodes an expression to a fixed format that is decoded on the server
//by the iTasks.UI.JS.Encoding.decodeOnServer function
function ___encodeJS(expr) {
    if (expr instanceof Array) {
        // Constructors
        if (typeof expr[0] === "number") {
            var consname = expr[1];			

            //Specifal cases:
            if(consname === "JSVal"){
                 //Don't try to encode references to javascript objects
                 console.warn('Trying to encode a JSVal object');
                 return null;
            } else if(consname  === "ARRAY"){
                 //Arrays are encoded as standard JSON arrays
                 var ret = [];
                 for(var i=2; i<expr.length; i++){
                     ret.push(___encodeJS(expr[i]));
                 }
                 return ret;
            }

            // Records
            // Very important! Do NOT use splice here! 	
            var consfunc = eval(Sapl.escapeName(consname));
            if (consfunc.$f instanceof Array) {
                var args = expr.slice(2, expr.length);
                var res = {};
                var aarg;

                for (var i = 0; i < args.length; i++) {
                    aarg = ___encodeJS(Sapl.feval(args[i]));
                    if(aarg != null) {
                        res[Sapl.print_consname(consfunc.$f[i])] = aarg;
                    }
                }
                return res;
             }

			// Normal ADTs with special cases for lists and tuples
			var isNil = Sapl.isNil(consname),
                isCons = Sapl.isCons(consname),
                isTuple = consname.startsWith("_Tuple"),
                res = [];

			if (isNil) {
                 return res;
            }
            if (!isCons && !isTuple) {
                res.push(Sapl.print_consname(consname));
            }
            if (isCons) { //Lists: read the whole list
               while(Sapl.isCons(expr[1])){
                   res.push(___encodeJS(Sapl.feval(expr[2])));
                   expr = Sapl.feval(expr[3]);
               }
            } else { //ADTs: eval all aruguments
                for (var i = 2; i < expr.length; i++) {
                    res.push(___encodeJS(Sapl.feval(expr[i])));
				}
            }
            return res;

        } else if (expr[0] === "function" && expr[1] instanceof Array) {
            console.warn('Trying to encode a js function');
            return null;
        }
    } else {
		return expr;
    }
}
	
//jsNull :: (JSVal a)
function __iTasks_UI_JS_Interface_jsNull() {
	return ___wrapJS(null);
}
//jsWindow :: (JSVal JSWindow)
function __iTasks_UI_JS_Interface_jsWindow() {
	return ___wrapJS(window);
}

//jsDocument :: (JSVal JSDocument)
function __iTasks_UI_JS_Interface_jsDocument() {
	return ___wrapJS(document);
}

//jsThis :: !*JSWorld -> (!JSVal a,!*JSWorld)
function __iTasks_UI_JS_Interface_jsThis(world) {
	var me = this;
	world = Sapl.feval(world);
	return ___Tuple2(___wrapJS(me), world);
}

//jsEmptyObject :: !*JSWorld -> (!JSVal a, !*JSWorld)
function __iTasks_UI_JS_Interface_jsEmptyObject(world) {
	world = Sapl.feval(world);
	return ___Tuple2(___wrapJS({}), world);
}

//jsNewObject :: !String ![JSPtr a] !*JSWorld -> *(!JSPtr b, !*JSWorld)
function __iTasks_UI_JS_Interface_jsNewObject(cons_name, args, world){
    world = Sapl.feval(world);
	cons_name = Sapl.feval(cons_name);
	args = Sapl.toJS(Sapl.feval(args));

    var evalStr = "new " + cons_name + "(";
    var argsArr = [];

    for (var i = 0; i < args.length; i++) {
      argsArr[i] = "args[" + i + "]";
    }

    evalStr += argsArr.join(",") + ");";

    var obj = eval(evalStr);
    return ___Tuple2(___wrapJS(obj), world);
}

//jsGetObjectAttr :: !String !(JSVal a) !*JSWorld -> *(!JSVal b, !*JSWorld)
function __iTasks_UI_JS_Interface_jsGetObjectAttr(attr,obj,world) {
	
	world = Sapl.feval(world);	
	attr = Sapl.feval(attr);
	obj = ___unwrapJS(Sapl.feval(obj));

	var value,
		path = attr.split('.'), step,
        undefined;

	try{	
		
		if(obj === null){
			obj = window;	
		}
		while(step =path.shift()) {
			obj = obj[step];
		}
		value = obj;
	}catch(err){
		value = undefined;
	}
	return ___Tuple2(___wrapJS(value), world);
}

//jsGetObjectEl :: !Int !(JSVal o) !*JSWorld -> *(!JSVal b, !*JSWorld)
function __iTasks_UI_JS_Interface_jsGetObjectEl(index,obj,world) {

	world = Sapl.feval(world);
	index = Sapl.feval(index);
	obj = ___unwrapJS(Sapl.feval(obj));

	return ___Tuple2(___wrapJS(obj[index]), world);
}

//jsSetObjectAttr :: !String !(JSVal v) !(JSVal o) !*JSWorld -> *JSWorld
function __iTasks_UI_JS_Interface_jsSetObjectAttr(attr,value,obj,world) {
    
	world = Sapl.feval(world);
	attr = Sapl.feval(attr);   
    value = ___unwrapJS(Sapl.feval(value));
    obj = ___unwrapJS(Sapl.feval(obj));

    var path = attr.split('.');
    while(path.length > 1) {
	obj = obj[path.shift()];
    }
    obj[path[0]] = value;
    
    return world;
}

//jsSetObjectEl :: !Int !(JSVal v) !(JSVal o) !*JSWorld -> *JSWorld
function __iTasks_UI_JS_Interface_jsSetObjectEl(index,value,obj,world) {

    world = Sapl.feval(world);
    index = Sapl.feval(index);   
    value = ___unwrapJS(Sapl.feval(value));
    obj = ___unwrapJS(Sapl.feval(obj));
    	
    obj[index] = value;
    
    return world;
}

//jsDeleteObjectAttr :: !String !(JSVal o) !*JSWorld -> !*JSWorld
function __iTasks_UI_JS_Interface_jsDeleteObjectAttr(attr,obj,world) {
    
	world = Sapl.feval(world);
	attr = Sapl.feval(attr);   
    obj = ___unwrapJS(Sapl.feval(obj));

    var path = attr.split('.');
    while(path.length > 1) {
       obj = obj[path.shift()];
    }
    delete obj[path[0]];
    
    return world;
}

//jsApply :: !(JSVal (JSFunction f)) !(JSVal scope) [JSVal args] !*JSWorld -> *(!JSVal a, !*JSWorld)
function __iTasks_UI_JS_Interface_jsApply(fun,scope,args,world) {
	world = Sapl.feval(world);
	fun = ___unwrapJS(Sapl.feval(fun));
	scope = ___unwrapJS(Sapl.feval(scope));
	args = Sapl.toJS(Sapl.feval(args)); 

    "undefined" === typeof fun && console.warn("jsApply: Evaluating undefined function");
    "undefined" === typeof scope && console.warn("jsApply: Evaluating function with undefined scope");

	return ___Tuple2(___wrapJS(fun.apply(scope,args)), world);
}
//jsWrapFun :: !([JSArg] *JSWorld -> (!*JSVal a, !*JSWorld)) !*JSWorld -> *(!JSVal (JSFunction f)), !*JSWorld)
function __iTasks_UI_JS_Interface_jsWrapFun(fun,world) {
    var wrapped;

    fun = Sapl.feval(fun);
    world = Sapl.feval(world);

    wrapped = function() {
		var args = Array.prototype.slice.call(arguments, 0);
		args = args.map(___wrapJS);
		args = Sapl.toList(args);
	
		var ys = Sapl.fapp(fun,[args,"JSWorld"]), ret;

        if(typeof ys === 'undefined') {
            console.warn('jsWrapFun: evaluation of wrapped function returned undefined',fun);
        }
        Sapl.feval(ys[3]);
        return ___unwrapJS(Sapl.feval(ys[2]));
    }
    return ___Tuple2(___wrapJS(wrapped), world);
}

//jsTypeof :: !(JSVal a) -> !String
function __iTasks_UI_JS_Interface_jsTypeof(obj) {
	return (typeof ___unwrapJS(Sapl.feval(obj)));
}
function __iTasks_UI_JS_Interface_jsAbort(obj) {
    console.log(obj);
	//obj = ___unwrapJS(Sapl.feval(obj));
    throw obj;
}

//jsIsNull :: !(JSVal a) -> !Bool
function __iTasks_UI_JS_Interface_jsIsNull(val) {
  return (___unwrapJS(Sapl.feval(val)) === null);
}

// newJSArray :: !*JSWorld -> *(!JSVal [a], !*JSWorld)
function __iTasks_UI_JS_Interface_newJSArray(world){
	world = Sapl.feval(world);
	return ___Tuple2(___wrapJS([]), world);
}

// toJSVal :: !a -> JSVal b
function __iTasks_UI_JS_Interface_toJSVal(val){
	val = Sapl.feval(val);

	// if an argument is a function, we handle it as an event handler
  if (val instanceof Array) {
    if (val[1] === "JSVal") {
      return val;
    } else if (isFunction(val[0])) {
      var eventHandler = function(expr) {
        var h = function() {
          expr[1].concat(arguments);
          Sapl.feval(expr);
        };
        return h;
      }

      return ___wrapJS(eventHandler(val));
    } else {
      return ___wrapJS(Sapl.toJS(val));
    }
  } else {
    return ___wrapJS(Sapl.toJS(val));
  }
}

// toJSArg :: !a -> JSArg
function __iTasks_UI_JS_Interface_toJSArg(val){

	val = Sapl.feval(val);

    if(typeof val === 'undefined') {
        console.warn('toJSArg: toJSArg of undefined value',val);
    }	

	if(val instanceof Array && val[1]=="JSVal") {
		return val;
	}else{
		return __iTasks_UI_JS_Interface_toJSVal(val);
	}
}		
// fromJSArgUnsafe :: !(JSArg a) -> Dynamic
function __iTasks_UI_JS_Interface_fromJSArgUnsafe(ptr){
	return toDynamic(___unwrapJS(Sapl.feval(ptr)));
}
// fromJSArg :: !JSArg !*JSWorld -> *(!Dynamic, !*JSWorld)
function __iTasks_UI_JS_Interface_fromJSArg(ptr, world){
	world = Sapl.feval(world);
	return (toDynamic(___unwrapJS(Sapl.feval(ptr))), world);
}
// fromJSValUnsafe :: !(JSVal a) -> Dynamic
function __iTasks_UI_JS_Interface_fromJSValUnsafe(ptr){
	return toDynamic(___unwrapJS(Sapl.feval(ptr)));
}

// fromJSVal :: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)
function __iTasks_UI_JS_Interface_fromJSVal(ptr, world){
	world = Sapl.feval(world);
	return (toDynamic(___unwrapJS(Sapl.feval(ptr))), world);
}

// jsUnsafeCoerce :: (JSVal a) -> JSVal b
function __iTasks_UI_JS_Interface_jsUnsafeCoerce(expr){
  return Sapl.feval(expr);
}

// jsUnsafeObjCoerce :: (JSVal a) -> JSObj b
function __iTasks_UI_JS_Interface_jsUnsafeObjCoerce(expr){
  return Sapl.feval(expr);
}

// jsUnsafeFunCoerce :: (JSVal a) -> JSFun b
function __iTasks_UI_JS_Interface_jsUnsafeFunCoerce(expr){
  return Sapl.feval(expr);
}

// jsUnsafeArrCoerce :: (JSVal a) -> JSArr b
function __iTasks_UI_JS_Interface_jsUnsafeArrCoerce(expr){
  return Sapl.feval(expr);
}

// jsPutCleanVal :: !String !a !(JSVal o) !*JSWorld -> *JSWorld
function __iTasks_UI_JS_Interface_jsPutCleanVal(key,val,obj,world){
	key   = Sapl.feval(key);
	val   = Sapl.feval(val);
    obj   = ___unwrapJS(Sapl.feval(obj));
	world = Sapl.feval(world);
	obj[key] = val;
	return world;
}

// jsGetCleanVal :: !String !(JSVal o) !*JSWorld -> *(!a,!*JSWorld)
function __iTasks_UI_JS_Interface_jsGetCleanVal(key,obj,world) {
	key   = Sapl.feval(key);
    obj   = ___unwrapJS(Sapl.feval(obj));
	world = Sapl.feval(world);
    return ___Tuple2(obj[key], world);
}

// decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)
function __iTasks_UI_JS_Encoding_decodeOnClient(val,world) {
	// Do nothing. The value is already in the right representation
	val = ___unwrapJS(Sapl.feval(val));
	world = Sapl.feval(world);
    return ___Tuple2(val, world);
}
// encodeOnClient :: !a *JSWorld -> (!JSVal a, !*JSWorld)
function __iTasks_UI_JS_Encoding_encodeOnClient(val,world) {
    val = ___wrapJS(___encodeJS(Sapl.feval(val)));
	world = Sapl.feval(world);
    return ___Tuple2(val, world);
}

