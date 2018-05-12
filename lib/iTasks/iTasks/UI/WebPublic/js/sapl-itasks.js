"use strict";

var _iworld;
var _itask_background_interval;

function __itask_background_process(){
	console.time('background process');
	
	var ret = __iTasks__Framework_Engine_background(_iworld);
	_iworld = Sapl.feval(ret);
	
	ret = Sapl.fapp(__iTasks__Framework_Client_RunOnClient_getUIUpdates,[_iworld]);
	_iworld = Sapl.feval(ret[3]);
	var uiupdates = Sapl.toJS(Sapl.feval(ret[2]));
	
	if(uiupdates){
		uiupdates.forEach(function(upd){
			var instanceId = upd[0];
			var diff = upd[1];
			var tasklet = itwc.controller.instanceProxies[instanceId].rootNode;
			if(tasklet){
				itwc.controller.updateUI({instance: instanceId, updates: JSON.parse(diff)}, tasklet);
			}else{
				console.log("Instance id cannot be found:", instanceId);
			}
		});
	}
	console.timeEnd('background process');
}

function __iTasks__Framework_Client_Tasklet_handleJSEvent(expr,taskId,eventvals){
	var taskId = Sapl.feval(taskId);
	
	var sti = taskId[2]+"-"+taskId[3]; // toString
	var tasklet = itwc.controller.tasklets[sti];
	var state = tasklet.definition.st;
	
	var eventarray = [0,"ARRAY"];
	for(var i=0; i<eventvals.length; i++) eventarray.push(___wrapJS(eventvals[i]));	
	
	// Returns a tuple of the JSWorld and HtmlEventResult	
	// Looks like: [0, "Tuple2", HtmlEventResult, JSWorld]	
	var ys = Sapl.fapp(expr,[taskId,eventarray,state,"WORLD"]);
	
	// The result is only in HNF, so both part of the tuple must be forced,
	// but the document can be dropped after that.
	Sapl.feval(ys[3]);
	
	var newstate = Sapl.feval(ys[2]);
	tasklet.definition.st = newstate;
							 
	// toJS to make the result hyperstrict
	var newres = Sapl.toJS(Sapl.fapp(tasklet.definition.resultFunc,[newstate]));
	
	// Send result to the client if it is changed only
	if(!geq(tasklet.definition.lastResult, newres)){
		tasklet.definition.lastResult = newres;
		itwc.controller.sendEditEvent(tasklet.definition.taskId, "result", newres);
	}
}

// ----------------------------------------------------------------
// JSStore

var _store = {}

function __iTasks__Framework_Client_JSStore_jsStoreValue(namespace,key,value,iworld){
	var iworld = Sapl.feval(iworld);
	var namespace = Sapl.feval(namespace);
	var key = Sapl.feval(key);
	var value = Sapl.feval(value);
	
	if(!_store[namespace]) _store[namespace]={};
	_store[namespace][key] = value;
	
	return iworld;
}

function __iTasks__Framework_Client_JSStore_jsLoadValue(namespace,key,iworld){
	var iworld = Sapl.feval(iworld);
	var namespace = Sapl.feval(namespace);
	var key = Sapl.feval(key);
	
	var ret = __Data_Maybe_Nothing;
	
	if(_store[namespace]){
		var val = _store[namespace][key];
		if(val){
			ret = __Data_Maybe_Just(val);
		}
	}
	
	return ___Tuple2(ret, iworld);
}

// ----------------------------------------------------------------
// Time

function ___time(world){
	world = Sapl.feval(world);
	var d = new Date();	
	
	var t = __System_Time_Timestamp(d.getTime());
	return ___Tuple2(t, world);	
}

function ___localTime(world){
	world = Sapl.feval(world);

	var d = new Date();
	
	var start = new Date(d.getFullYear(), 0, 0);
	var diff = d - start;
	var oneDay = 1000 * 60 * 60 * 24;
	var dayofyear = Math.floor(diff / oneDay);	
	
	var tm = __System_Time__Tm(
				d.getSeconds(), 
				d.getMinutes(), 
				d.getHours(), 
				d.getDate(), // day of the month
				d.getMonth(), 
				d.getYear(), 
				d.getDay(), // day of the week
				dayofyear,
				d.dst());

	return ___Tuple2(tm, world);
}

// ----------------------------------------------------------------

function __iTasks__Framework_RemoteAccess_httpRequest_client(method, url, mbBody, iworld){

	method = Sapl.feval(method);
	url = Sapl.feval(url);
	var body = Sapl.toJS(Sapl.feval(mbBody));
		
	var request = new XMLHttpRequest();
	request.open(method, url, false);  // `false` makes the request synchronous
	request.send(body);

	var response = __Internet_HTTP__HTTPResponse(
		request.status, request.statusText, ___predefined__Nil, request.responseText);

	// TODO: getAllResponseHeaders
		
	console.log("Request",method,url,body,request.responseText);		
		
	return ___Tuple2(response, iworld);
}

// ----------------------------------------------------------------
// General function overrides

function __sapldebug_sapldebug(a,b){
	console.log("DEBUG: "+Sapl.toString(a));
	return b;
}

function __dynamic_string_copy_to_string(a){
	return Sapl.dynamicToString(Sapl.feval(a));
}

function __dynamic_string_copy_from_string(a){
	eval("var tmp="+Sapl.feval(a)+";");
	return ___Tuple2(tmp, a); // TODO: second?
}

function __graph_to_sapl_string_graph_to_sapl_string(a){
	return Sapl.dynamicToString(Sapl.feval(a));
}

function __iTasks__Framework_Client_RunOnClient_newWorld(){
	return "WORLD";
}

function __iTasks__Framework_Client_Override_cast_to_TaskValue(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __iTasks__Framework_Client_Override_cast(___vTC_0, ___vTC_1, __a_2) {
    return Sapl.feval(__a_2);
};

function __iTasks__Framework_Client_Override_unwrapTask(__vTC_0, __a) {
    var d = Sapl.feval(__a);
    return Sapl.feval(d[2]);
};
