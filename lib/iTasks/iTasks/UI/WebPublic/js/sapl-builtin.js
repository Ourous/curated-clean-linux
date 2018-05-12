"use strict";

// TO BE SURE
function ___Tuple3(_1, _2, _3) {
	return [0, '_Tuple3', _1, _2, _3];
};

function ___Tuple2(_1, _2) {
	return [0, '_Tuple2', _1, _2];
};

var __Data_Maybe_Nothing = [0,"Data.Maybe.Nothing"];

function __Data_Maybe_Just(val){
	return [1,"Data.Maybe.Just",val];
};

var __Maybe_Nothing = [0, 'Maybe.Nothing'];

function __Maybe_Just(__a1) {
    return [1, 'Maybe.Just', __a1];
};

var ___predefined__Nil = [1,"_predefined._Nil"];
var ___predefined__Unit = [0,"_predefined._Unit"];
var ___system__Unit = [0,"_system._Unit"];
// TO BE SURE

// --------- Primitive functions -----------------------------

function print(val){
	val = Sapl.toJS(Sapl.feval(val));
	var out = document.getElementById("output");
	if(out){
		if(val == "\n"){
			var br = document.createElement("BR");
			out.appendChild(br);
		}else{
			var span = document.createElement("SPAN");
			span.innerHTML = val;
			out.appendChild(span);
		}
	}else{
		console.log(val);
	}
}

function println(val){
	print(val);
	print("\n");
}

function __trace(val, cont){
	print(val);
	return Sapl.feval(cont);
}

function __traceln(val, cont){
	println(val);
	return Sapl.feval(cont);
}

// --------- Primitive functions -----------------------------

function _mult(a,b){
    return Sapl.feval(a)*Sapl.feval(b);
}

function _div(a,b){
    return Math.floor(Sapl.feval(a)/Sapl.feval(b));
}

function _divreal(a,b){
    return Sapl.feval(a)/Sapl.feval(b);
}

function _add(a,b){
    return Sapl.feval(a)+Sapl.feval(b);
}

function _sub(a,b){
    return Sapl.feval(a)-Sapl.feval(b);
}

function _eq(a,b){
    return Sapl.feval(a)==Sapl.feval(b);
}	

function _neq(a,b){
    return Sapl.feval(a)!=Sapl.feval(b);
}	

function _pow(a,b){
    return Math.pow(Sapl.feval(a),Sapl.feval(b));
}	

function _sin(a){
    return Math.sin(Sapl.feval(a));
}	
function _asin(a){
    return Math.asin(Sapl.feval(a));
}	

function _cos(a){
    return Math.cos(Sapl.feval(a));
}	
function _acos(a){
    return Math.acos(Sapl.feval(a));
}

function _tan(a){
    return Math.tan(Sapl.feval(a));
}	
function _atan(a){
    return Math.atan(Sapl.feval(a));
}

function _mod(a,b){
    return Sapl.feval(a)%Sapl.feval(b);
}	

function _gt(a,b){
    return Sapl.feval(a)>Sapl.feval(b);
}	

function _ge(a,b){
    return Sapl.feval(a)>=Sapl.feval(b);
}

function _lt(a,b){
    return Sapl.feval(a)<Sapl.feval(b);
}	

function _not(a){
    return !Sapl.feval(a);
}

function _and(a,b){
    return Sapl.feval(a) && Sapl.feval(b);
}

function _or(a,b){
    return Sapl.feval(a) || Sapl.feval(b);
}

function _abs(a){
    return Math.abs(Sapl.feval(a));
}

function _neg(a){
    return Sapl.feval(a) * -1;
}

function _toInt_char(chr){
    var nchr = Sapl.feval(chr);
    return nchr.charCodeAt(0);
}

function _toInt_str(str){
    return (Sapl.feval(str) | 0);
}

function _toInt_real(r){
    var nr = Sapl.feval(r);
    return Math.floor(nr);
}

function _toReal(str){
    var nstr = Sapl.feval(str);
    return parseFloat(nstr);
}

function _sqrt(num){
    return Math.sqrt(Sapl.feval(num));
}

function _toChar(code){
    var ncode = Sapl.feval(code);
    return String.fromCharCode(ncode);
}

function _toString(a){
    var na = Sapl.feval(a);
    return na + "";
}

function _bitand(a,b){
    return Sapl.feval(a) & Sapl.feval(b);
}

function _bitor(a,b){
    return Sapl.feval(a) | Sapl.feval(b);
}

function _bitxor(a,b){
    return Sapl.feval(a) ^ Sapl.feval(b);
}

function _bitnot(a){
    return ~ Sapl.feval(a);
}

function _shiftleft(a,b){
    return Sapl.feval(a) << Sapl.feval(b);
}

function _shiftright(a,b){
    return Sapl.feval(a) >>> Sapl.feval(b);
}

function __entier(a){
    return Math.floor(Sapl.feval(a));
}

// ----------------------------------------------------------------
// Arrays

function _array_create1$eval(size){
	return _array_create1(Sapl.feval(size), null);
}

function _array_create1(size){
	var arr = new Array(size+2);
	arr[0]=0;
	arr[1]="ARRAY";
	return arr;
}

function _array_create2_lazy$eval(size, e){
	return _array_create2(Sapl.feval(size), e);
}

function _array_create2$eval(size, e){
	return _array_create2(Sapl.feval(size), Sapl.feval(e));
}

function _array_create2(size, e){
	var data = [0,"ARRAY"];

	for(var i = 0; i < size; i++) {
		data.push(e);
	}	
	
	return data;
}

function _array_update_lazy$eval(arr, idx, e){
	return _array_update(Sapl.feval(arr), Sapl.feval(idx), e);
}

function _array_update$eval(arr, idx, e){
	return _array_update(Sapl.feval(arr), Sapl.feval(idx), Sapl.feval(e));
}

function _array_update(arr, idx, e){
	arr[idx+2] = e;
	return arr;
}

function _array_replace_lazy$eval(arr, idx, e){
	return _array_replace(Sapl.feval(arr), Sapl.feval(idx), e);
}

function _array_replace$eval(arr, idx, e){
	return _array_replace(Sapl.feval(arr), Sapl.feval(idx), Sapl.feval(e));
}

function _array_replace(arr, idx, e){
	var o = arr[idx+2];
	arr[idx+2] = e;
	return ___Tuple2(o, arr);
}

function _array_select_lazy(arr, idx){
	return Sapl.feval(Sapl.feval(arr)[Sapl.feval(idx)+2]);
}

function _array_select(arr, idx){
	return Sapl.feval(arr)[Sapl.feval(idx)+2];
}

function _array_uselect(arr, idx){
	return ___Tuple2(Sapl.feval(arr)[Sapl.feval(idx)+2],arr);
}

function _array_size(arr){
	return Sapl.feval(arr).length-2;
}

function _array_usize(arr){
	return ___Tuple2(Sapl.feval(arr).length-2,arr);
}

// ----------------------------------------------------------------
// Strings

function _string_size(str){
    return Sapl.feval(str).length;
}

function _string_usize(str){
    return ___Tuple2(Sapl.feval(str).length,str);
}

function _string_select(str, pos){
	return Sapl.feval(str).charAt(Sapl.feval(pos));
}  

function _string_uselect(str, pos){
	return ___Tuple2(Sapl.feval(str).charAt(Sapl.feval(pos)),str);
} 

function _string_create1$eval(len){
	return _string_create1(Sapl.feval(len));
}

function _string_create1(len){
    return new Array(len + 1).join('\0');
}

function _string_create2$eval(len, c){
    return _string_create2(Sapl.feval(len), Sapl.feval(c));
}

function _string_create2(len, c){
    return new Array(len + 1).join(c);
}

function _string_update(str, idx, c){
    return Sapl.feval(str).replaceAt(Sapl.feval(idx),Sapl.feval(c));
}

function _string_replace$eval(str, idx, c){	
	return _string_replace(Sapl.feval(str), Sapl.feval(idx), Sapl.feval(c));
}
	
function _string_replace(str, idx, c){
    return ___Tuple2(str.charAt(idx), str.replaceAt(idx,c));
}

function _string_slice(str, ind1, ind2){
    var nind1 = Sapl.feval(ind1);
    var nind2 = Sapl.feval(ind2);
    return Sapl.feval(str).substr(nind1, nind2 - nind1 + 1);
}

function _string_append(str1, str2){
    return Sapl.feval(str1)+Sapl.feval(str2);
}

// ----------------------------------------------------------------
// Tuple selectors

function _tupsels1v0(a){
	return Sapl.feval(Sapl.feval(a)[2]);
}

function _tupsels2v0(a){
	return Sapl.feval(Sapl.feval(a)[2]);
}

function _tupsels2v1(a){
	return Sapl.feval(Sapl.feval(a)[3]);
}

function _tupsels3v0(a){
	return Sapl.feval(Sapl.feval(a)[2]);
}

function _tupsels3v1(a){
	return Sapl.feval(Sapl.feval(a)[3]);
}

function _tupsels3v2(a){
	return Sapl.feval(Sapl.feval(a)[4]);
}

function _tupsels4v0(a){
	return Sapl.feval(Sapl.feval(a)[2]);
}

function _tupsels4v1(a){
	return Sapl.feval(Sapl.feval(a)[3]);
}

function _tupsels4v2(a){
	return Sapl.feval(Sapl.feval(a)[4]);
}

function _tupsels4v3(a){
	return Sapl.feval(Sapl.feval(a)[5]);
}

// ----------------------------------------------------------------
// Other stuff

function _error(str){
    throw "ERROR: "+Sapl.feval(str);
}

function _abort(str){
    throw "ABORT: "+Sapl.feval(str);
}

function __sapldebug(str, f){
    if(confirm(str)){
        return f;
    }else{
        _error();
    }
}

// Something unique that can be compared for equality
function __getRunTimeDynamicDescriptor(a){
	if(a instanceof Array && isFunction(a[0])){
		// Wrapped function
		return a[0];
	}else if(a instanceof Array && typeof a[0] === "number") {
		// Data constructor
		return a[1];
	}else{
		return a;
	}
}

function __Text_Encodings_Base64_base64URLEncode(a) {
  return window.btoa(Sapl.feval(a));
}

function __Text_Encodings_Base64_base64URLDecode(a) {
  return window.atob(Sapl.feval(a));
}
