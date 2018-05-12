"use strict";

var Sapl = new function () {

	this.variable_prefix = "__";

	this.escapeName = function (name) {
		if (name.endsWith("$eval")) {
			return Sapl.escapeName(name.substring(0, name.length - 5)) + "$eval";
		} else {
			name = encodeURIComponent(name);
			name = name.replace(/\(/g, "%28");
			name = name.replace(/\)/g, "%29");
			name = name.replace(/!/g, "%21");
			name = name.replace(/\*/g, "%2A");
			name = name.replace(/-/g, "%2D");
			name = name.replace(/~/g, "%7E");
			name = name.replace(/`/g, "%60");
			name = name.replace(/\./g, "_");
			name = name.replace(/%/g, '$');
			return Sapl.variable_prefix + name;
		}
	}

	// An SaplHtml event handler returns a tuple of the document and boolean
	// so, we have to return with the boolean
	this.execEvent = function (event, expr) {
		var tmp;
		eval("tmp = " + Sapl.escapeName(expr) + ";");
		return (Sapl.toJS(tmp(event, document))[1]);
	}

	this.print_ident = function (name) {
		if (typeof name === "string") {
			var a = name.trim();
			a = a.substring(Sapl.variable_prefix.length);
			a = a.replace(/\$/g, '%');
			a = encodeURIComponent(a);
			return a;
		} else {
			return name;
		}
	}

	this.print_consname = function (name) {
		var a = name.trim();
        var dot = a.lastIndexOf(".");
		if(dot > 0){
			a = a.substring(dot+1);
		}
		return a;
	}	
	
	this.isCompound = function (expr) {
		return expr.length > 0 && expr.indexOf(" ") > -1 && expr.indexOf("{") != 0;
	}

	this.toString = function (expr) {
		if (expr instanceof Array) {

			// It's a constructor! We can just drop the selector number
			if (typeof expr[0] === "number") {

				// Very important! Do NOT use splice here! 	
				var args = expr.slice(2, expr.length);
				var consname = expr[1];
				var consfunc = eval(this.escapeName(consname));				

				if (consfunc.$f instanceof Array) {
					var res = "{";					
					
					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						if (i > 0) res += ", ";
						res += this.print_consname(consfunc.$f[i]) + ": ";
						res += this.isCompound(aarg) ? "(" + aarg + ")" : aarg;
					}

					return res + "}";
				} else {
					var res = this.toString(consname);

					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						res += " " + (this.isCompound(aarg) ? "(" + aarg + ")" : aarg);
					}

					return res;
				}

			}

			// It's an application
			if (expr instanceof Array && isFunction(expr[0]) && expr[1] instanceof Array) {
				if (expr[0].length > expr[1].length) {
					var res = this.toString(expr[0]);
					var args = expr[1];

					for (var i = 0; i < args.length; i++) {
						var aarg = this.toString(this.feval(args[i]));
						res += " " + (aarg.indexOf(" ") > 0 ? "(" + aarg + ")" : aarg);
					}

					return res;
				} else {
					return this.toString(this.feval(expr));
				}
			}

		} else if (isFunction(expr)) {
			if (expr.name.endsWith("$eval")) {
				return this.print_ident(expr.name.substring(0, expr.name.length - 5)) + "$eval";
			} else {
				return this.print_ident(expr.name);
			}
		} else {
			if (typeof expr === "number") {
				return expr.toString();
			} else if (isBoolean(expr)) {
				return expr.toString();
			} else {
				return "\"" + expr.replace(/"/g, "\\\"") + "\"";
			}
		}
	}

	this.isJust = function (consname){
		return consname === "StdMaybe.Just" || consname === "Data.Maybe.Just";
	}

	this.isNothing = function (consname){
		return consname === "StdMaybe.Nothing" || consname === "Data.Maybe.Nothing";
	}

	this.isCons = function (consname){
		return consname === "_predefined._Cons" || consname === "cons";
	}

	this.isNil = function (consname){
		return consname === "_predefined._Nil" || consname === "nil";
	}
	
	this.toTuple = function (arr){
		var prefix = [0, '_Tuple'+arr.length];
		// Concat doesn't work here if "arr" is "argument" because "argument" is not an array
		for(var i=0; i<arr.length; i++) prefix.push(arr[i]);
		return prefix;
	}

	this.toList = function (arr){	
		if(arr.length === 0){
			return [1,'_predefined._Nil'];
		}else{
			var e = arr[0];
			arr.shift();
			return [0,'_predefined._Cons', e, this.toList(arr)];
		}	
	}	
	
	this.toJSON = function (expr) {
		return JSON.stringify(this.toJS(expr));
	}

	this.toJS = function (expr) {
		return this._toJS(false, expr);
	}

	// It expects the expression argument to be HNF
	this._toJS = function (inRecord, expr) {

		if (expr instanceof Array) {

			// It's a constructor! We can just drop the selector number
			if (typeof expr[0] === "number") {
			
				var consname = expr[1];			
			
				// No feval! It's strict in its argument
				if(consname === "JSVal"){
					return expr[2]; 
				}else if(consname  === "ARRAY"){
					var ret = [];
					for(var i=2; i<expr.length; i++){
						ret.push(this._toJS(false,expr[i]));
					}
					return ret;
				}

				// Special case for dynamics
				if(consname === "_SystemDynamic._DynamicTemp"){
					// Do not do anything
					return expr;
				}
			
				// Very important! Do NOT use splice here! 	
				var consfunc = eval(this.escapeName(consname));
				if (consfunc.$f instanceof Array) {
				    var args = expr.slice(2, expr.length);
					var res = {};
                    var aarg;

					for (var i = 0; i < args.length; i++) {
						aarg = this._toJS(true, this.feval(args[i]));
						if(aarg != null)
							res[this.print_consname(consfunc.$f[i])] = aarg;
					}

					return res;
				} else {
                    if (inRecord) {
                      if (this.isNothing(consname)) return null;
                      if (this.isJust(consname)) return this._toJS(false,this.feval(expr[2]));
                    }
					if (this.isNil(consname)) return [];
					var res = [];

					var arraycons = this.isCons(consname);

					if (!arraycons && !consname.startsWith("_Tuple")) {

						res.push(this.print_consname(consname));
					}

					if (arraycons) {
						while(this.isCons(expr[1])){
							res.push(this._toJS(false,this.feval(expr[2])));
							expr = this.feval(expr[3]);
						}
					} else {
				        var args = expr.slice(2, expr.length);
                        var aarg;
						for (var i = 0; i < args.length; i++) {
							aarg = this._toJS(false,this.feval(args[i]));
							res.push(aarg);
						}
					}

					return res;
				}
			}

			// It's an application
			if (expr instanceof Array && typeof expr[0] === "function" && expr[1] instanceof Array) {
				if (expr[0].length > expr[1].length) {
					// it's an partial application. leave it like that
					return expr;
				} else {
					return this._toJS(false,this.feval(expr));
				}
			}

		} else {
			return expr;
		}
	}
	
	this.dynamicToString = function(expr) {

		if (expr instanceof Array) {

			var ret = "[";
			for(var i=0;i<expr.length;i++){
				if(i>0) ret += ",";
				ret += this.dynamicToString(expr[i]);
			}
			return ret+"]";
			
		} else if (isFunction(expr)) {
			return expr.name;
		
		} else if (isObject(expr)) {
			return "\"OBJECT\"";
			
		} else {
			if (typeof expr === "number") {
				return expr.toString();
			} else if (isBoolean(expr)) {
				return expr.toString();
			} else if (isString(expr)){
				return "\"" + expr.replace(/"/g, "\\\"") + "\"";
			} else {
				return expr;
			}
		}
	}	
	
	this.apply = function(f, args){
		return Sapl.feval([f,args]);
	}

	this.select = function(expr, idx){
		return Sapl.feval(Sapl.feval(expr)[idx]);
	}

	this.sselect = function(expr, idx){
		return Sapl.feval(expr)[idx];
	}
	
	// hyper(strict) eval
	this.heval = function (expr) {
		expr = Sapl.feval(expr);
		
		if (expr instanceof Array) {

			// It's a constructor
			if (typeof expr[0] === "number") {
				for(var i = 2; i<expr.length; i++){
					expr[i] = Sapl.heval(expr[i]);
				}
			}
		}
			
		return expr;
	}

	this.feval = _feval;
	this.fapp = _fapp;	
}();
