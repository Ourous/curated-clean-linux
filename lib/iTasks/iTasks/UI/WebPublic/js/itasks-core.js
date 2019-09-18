//Global itasks namespace
itasks = {};

//Auxiliary definitions for sending Maybe values to server
const Nothing = ["Nothing"];
function Just(x) { return["Just", x]; }

//Global lookup table of itask components indexed by their dom element.
//This makes it possible to find the managing itask component object for arbitrary dom elements.
//Because it is a WeakMap, we can register components without having to unregister them.
itasks.components = new WeakMap();

//Core behavior
itasks.Component = {

	domTag: 'div',
	domEl: null,

	container: true,
	containerEl: null,

	cssPrefix: 'itasks-',
	cssCls: 'component',

	attributes: {},
	parentCmp: null,
	children: [],

	initialized: false,

	init: function() {
		var me = this;
		me.lastFire = 0;

		return me.world=Promise.resolve()
			.then(me.initUI.bind(me))
			.then(me.initComponent.bind(me))
			.then(me.initChildren.bind(me))
			.then(me.renderComponent.bind(me))
			.then(me.registerComponent.bind(me))
			.then(function(){ me.initialized=true; });
	},
	initUI: function() {
		var me=this;
		if (me.attributes.initUI!=null && me.attributes.initUI!='') {
			return ABC_loading_promise.then(function(){
				var initUI=ABC.deserialize(atob(me.attributes.initUI));
				var ref=ABC.share_clean_value(initUI,me);
				ABC.interpret(new SharedCleanValue(ref), [me, ABC.initialized ? 0 : 1]);
				ABC.clear_shared_clean_value(ref);
			});
		}
	},
	initComponent: function() {}, //Abstract method: every component implements this differently
	initChildren: function() {
		var me = this;
		return me.children.reduce((promise,spec,i) => promise.then(function(){
			me.beforeChildInsert(i,spec);
			me.children[i] = me.createChild(spec);
			return me.children[i].init().then(function(){
				me.afterChildInsert(i,me.children[i]);
			});
		}), Promise.resolve());
	},
	renderComponent: function() {
		var me = this;
		if(me.domEl === null) { //Create a new dom element
        	me.domEl = document.createElement(me.domTag);
		} else { //Clear an existing element
			me.domEl.innerHTML = null;
		}
		//Initialially make the outer dom element also the container element that holds the child components
		me.containerEl = me.domEl;
			
		//Style the dom element
		me.domEl.classList.add(me.cssPrefix + me.cssCls);
		if(me.attributes['style']) {
			me.domEl.style = me.attributes['style'];
		}
		if(me.attributes['class']) {
			if(Array.isArray(me.attributes['class'])) {
				var len = me.attributes['class'].length;
				for(var i = 0; i < len; i++) {
					me.domEl.classList.add(me.attributes['class'][i]);
				}
			} else {
				me.domEl.classList.add(me.attributes['class']);
			}
		}
		//Custom initialization after the dom element has been rendered
		me.initDOMEl();
		//Size the element if explicit sizes are given
		me.initDOMElSize();
		//Add the the child renderings 
		me.children.forEach(function(child) {
			if(child.domEl) {
				me.containerEl.appendChild(child.domEl);
			}
		});
	},
	registerComponent: function() {
		if(this.domEl !== null) {
			itasks.components.set(this.domEl,this);
		}	
	},
	initDOMEl: function() {},

	initDOMElSize: function() {
		var me = this,
			el = me.domEl,
			width = me.attributes.width,
			height = me.attributes.height;

		//Set width & height using attributes
		if('width' in me.attributes) {
			if(width === 'flex') {
				el.classList.add(me.cssPrefix + 'flex-width');
			} else if (width === 'wrap') {
				el.classList.add(me.cssPrefix + 'wrap-width');
			} else {
				el.classList.add(me.cssPrefix + 'exact-width');
				el.style.width = width + 'px';
			}
		}
		if('height' in me.attributes) {
			if(height === 'flex') {
				el.classList.add(me.cssPrefix + 'flex-height');
			} else if (height === 'wrap') {
				el.classList.add(me.cssPrefix + 'wrap-height');
			} else {
				el.classList.add(me.cssPrefix + 'exact-height');
				el.style.height = height + 'px';
			}
		}
    },
	doEditEvent: function (taskId, editorId, value) {
		var me = this;
		if(me.parentCmp) {
			//Timeout is set, check if we can fire
			var now = (new Date()).getTime();
			//We are within timeout
			if (me.attributes.eventTimeout && now - me.lastFire < me.attributes.eventTimeout){
				window.clearTimeout(me.queuedFire);
				me.queuedFire = window.setTimeout(function (){
					me.doEditEvent(taskId, editorId, value);
				}, me.attributes.eventTimeout - (now - me.lastFire));
			} else {
				me.lastFire = now;
				me.parentCmp.doEditEvent(taskId, editorId, value);
			}
		}
	},
	findChild: function(obj) {
		var me = this, num = me.children.length, i;

		for( i = 0; i < num; i++) {
			if(me.children[i] === obj) {	
				return i;
			}
		}
	},
	createChild: function(spec) {
		var me = this,
			type = spec.type || 'Component',
			child = {};

		if(type !== 'Data') {
			me.addSpec_(child, itasks.Component);
		}
				
		if(itasks[type]) {
			me.addSpec_(child,itasks[type]);
		}
		child.parentCmp = me;
		child.children = [];

		me.addSpec_(child,spec);

		return child;
	},
	addSpec_:function(obj,spec) {
		var attributes = {};
		Object.assign(attributes,obj.attributes,spec.attributes);
		Object.assign(obj,spec);
		obj.attributes = attributes;
	},
	insertChild: function(idx = 0, spec = {}) {
		var me = this,
			child = null,
			isLast = (idx == me.children.length);
		
		me.beforeChildInsert(idx,spec);

		//Create the child object
		child = me.initialized ? me.createChild(spec) : spec ;

		//Add the child to the collection of children
		me.children.splice(idx,0,child);

		var finish_up=function(){
			me.afterChildInsert(idx,child);
			if (child.onResize)
				child.onResize();
		};

		if(me.initialized) {
			//Initialize, if we are already initialized
			return child.init().then(function(){
				//Add the child to the dom
				if(child.domEl) {
					if(isLast) {
						me.containerEl.appendChild(child.domEl);
					} else {
						me.containerEl.insertBefore(child.domEl,me.containerEl.childNodes[idx]);
					}
					child.onShow();
				}

				finish_up();
			});
		} else {
			finish_up();
		}
	},
	beforeChildInsert: function(idx,spec) {},
	afterChildInsert: function(idx,child) {},
	removeChild: function(idx = 0) {
		var me = this, child = me.children[idx];

		child._beforeRemove();
		me.beforeChildRemove(idx,child);

		if(me.initialized && child.domEl) {
			me.containerEl.removeChild(me.containerEl.childNodes[idx]);
		}
		me.children.splice(idx,1);	
		me.afterChildRemove(idx);
	},
	replaceChild: function(idx,spec) {
		var me = this;
		if(idx >= 0 && idx < me.children.length) {
			me.removeChild(idx);
			return me.insertChild(idx,spec);
		}
	},
	moveChild: function(sidx,didx) {
		var me = this, child;

		if(me.initialized && me.children[sidx].domEl) {
			if(didx == (me.containerEl.children.length - 1)) {
				me.containerEl.appendChild(me.containerEl.children[sidx]);
			} else {
				me.containerEl.insertBefore(me.containerEl.children[sidx],me.containerEl.children[(didx > sidx) ? (didx + 1) : didx]);
			}
		}

		child = me.children.splice(sidx,1)[0]; //Remove followed by insert...
		me.children.splice(didx, 0, child);
	},
	beforeChildRemove: function(idx,child) {},
	afterChildRemove: function(idx) {},
	/* beforeRemove can be overwritten to add a handler for 'destroy' events.
	 * _beforeRemove is internal and should not be overwritten.
	 */
	beforeRemove: function() {
	},
	shared_clean_values: null,
	_beforeRemove: function() {
		this.beforeRemove();

		if (this.shared_clean_values!=null) {
			// garbage collect any remaining values shared with wasm
			this.shared_clean_values.forEach(ref => ABC.clear_shared_clean_value(ref,false));
			this.shared_clean_values.clear();
		}

		this.children.forEach(child => child._beforeRemove());
	},
	setAttribute: function(name,value) {
		var me = this;
	
		me.attributes[name] = value;	
		me.onAttributeChange(name,value);
	},
	onAttributeChange: function(name,value) {},

	onUIChange: function(change) {
		var me = this;
		me.world=me.world.then (function(){
			if(change) {
				switch(change.type) {
					case 'replace':
						return me.onReplaceUI(change.definition);
					case 'change':
						return me.onChangeUI(change.attributes,change.children);
				}
			}
		});
	},
	onReplaceUI: function(spec) {
		var me = this;
		if(me.parentCmp) {
			var idx = me.parentCmp.findChild(me);
			return me.parentCmp.replaceChild(idx,spec);
		}
	},
	onChangeUI: function(attributeChanges,childChanges) {
		var me = this;

		//Handle attribute changes
		if(attributeChanges instanceof Array) {
			attributeChanges.forEach(function(change) {
				me.setAttribute(change.name,change.value);
			});
		}
		//Handle child changes
		if (childChanges instanceof Array) {
			childChanges.reduce((promise,change) => promise.then(function(){
				var idx = change[0];
				switch(change[1]) {
					case 'change':
						if(idx >= 0 && idx < me.children.length) {
							me.children[idx].onUIChange(change[2]);
							me.world = me.world.then(function () {
								me.afterChildChange(idx,change[2]);
							});
							return;
						} else {
							console.log("UNKNOWN CHILD",idx,me.children.length,change);
						}
						break;
					case 'insert':
						return me.insertChild(idx,change[2]);
					case 'remove':
						me.removeChild(idx);
						break;
					case 'move':
						me.moveChild(idx,change[2]);
						break;
				}
			}), Promise.resolve());
		}
	},
	afterChildChange: function(idx,change) {},
	onShow: function() {
		this.children.forEach(function(child) { if(child.onShow) {child.onShow();}});
	},
	onHide: function() {
		this.children.forEach(function(child) { if(child.onHide) {child.onHide();}});
	},
	onResize: function() {
		this.children.forEach(function(child) { if(child.onResize) {child.onResize();}});
	},
	onHtmlEvent: function(msg) { //Abstract
	},
	getViewport: function() {
		var me = this, vp = me.parentCmp;
		while(vp) {
			if(vp.cssCls == 'viewport') { //Bit of a hack...
				return vp;
			}
			vp = vp.parentCmp;
		}
		return null;
	}
};
itasks.Loader = {
	cssCls: 'loader',
	initDOMEl: function() {
		var me = this,
			l = document.createElement('div');
			l.classList.add(me.cssPrefix + 'loader-spinner');
		me.domEl.appendChild(l);
		//Temporary
		me.domEl.classList.add(me.cssPrefix + 'flex-width');
		me.domEl.classList.add(me.cssPrefix + 'flex-height');
		if(me.attributes.taskId){
			me.doEditEvent(me.attributes.taskId, me.attributes.editorId, true);
		}
	}
};
itasks.ExceptionView = {
	cssCls: 'exception',
	container: false,
	initDOMEl: function() {

		//Temporary
		this.domEl.classList.add(this.cssPrefix + 'flex-width');
		this.domEl.classList.add(this.cssPrefix + 'flex-height');

		this.domEl.innerHTML = '<h1>Exception</h1><span>' + (this.attributes.value || '') + '</span>';
	}
};
itasks.Viewport = {
	cssCls: 'viewport',
	syncTitle: false,

	initComponent:function() {
		var me = this;	

		//Create a temporary root element
		me.insertChild(0,{type:'Loader', parentCmp: me});

		me.parentViewport = me.getViewport();

		//Get a connection
		me.taskUrl = me.determineTaskEndpoint();
		me.connection = itasks.ConnectionPool.getConnection(me.taskUrl);

		var uiChangeCallback = me.onInstanceUIChange.bind(me);
		var exceptionCallback = me.onException.bind(me);

		me.changeListeners = [];

		if('instanceNo' in me.attributes) {
			//Connect to an existing task instance
			me.connection.attachTaskInstance(
				me.attributes['instanceNo'],
				me.attributes['instanceKey'],
				uiChangeCallback,
				exceptionCallback);	
		} else {
			//Create a new session
			me.connection.newSession(
				function(instanceNo) { me.attributes['instanceNo'] = instanceNo;},
				uiChangeCallback,
				exceptionCallback);	
		}

		me.addWindowResizeListener();
	},
	addWindowResizeListener: function() {
		var me = this;
		if(me.parentViewport !== null) { //Only listen to window changes as the top level
			return;
		}
		window.addEventListener('resize',me.onResize.bind(me));
	},
	determineTaskEndpoint: function() {
		var me = this;
		if(me.taskUrl) { //Something was configured explicitly
			if(me.taskUrl.startsWith('ws://')) {
				return me.taskUrl;
			} else {
				return 'ws://' + location.host + me.taskUrl + (me.taskUrl.endsWith('/') ? '' : '/') + 'gui-wsock';
			}
		} 
		if(me.parentViewport === null) {
			//If there is no parent, use the default url
			return 'ws://' + location.host + location.pathname + (location.pathname.endsWith('/') ? '' : '/') + 'gui-wsock';
		} else {
			return me.parentViewport.determineTaskEndpoint();
		}
	},
	doEditEvent: function (taskId, editorId, value) {
		var me = this, taskNo = taskId.split("-")[1];
		if(editorId) {
			me.connection.sendEditEvent(me.attributes.instanceNo, taskNo, editorId, value);
		} else {
			me.connection.sendActionEvent(me.attributes.instanceNo, taskNo, value);
		}
	},
	onInstanceUIChange: function(change) {
		var me = this;
	
		me.children[0].onUIChange(change);
		//Sync title of the top level element
		if(me.syncTitle) {
			if(change.type == 'replace' && 'attributes' in change.definition && 'title' in change.definition.attributes) {
				document.title = change.definition.attributes.title;
			}
			if(change.type == 'change' && change.attributes instanceof Array) {
				change.attributes.forEach(function(change) {
					if(change.name == 'title') {
						document.title = change.value;
					}
				});
			}
		}
		//Trigger changelisteners
		me.changeListeners.forEach(function(cl) {
			cl.onViewportChange(change);
		});
	},
	addChangeListener: function(cmp) {
		var me = this;
		me.changeListeners.push(cmp);
	},
	removeChangeListener: function(cmp) {
		var me = this;
		me.changeListeners = me.changeListeners.filter(function(el) {
			return el != cmp;
		});
	},
	onException: function(exception) {
		var me = this;
		//Remove all children and show the exception message
		for(var i = me.children.length - 1; i >= 0; i--) {
			me.removeChild(i);
		}
		me.insertChild(0,{type:'ExceptionView', parentCmp: me, attributes: { value : exception}});
	},
	beforeRemove: function() {
		var me = this, instanceNo = me.attributes['instanceNo'];	
		
		if(instanceNo) {
			me.connection.detachTaskInstance(instanceNo);
		}
	},
	_beforeRemove: function() {
		this.beforeRemove();
	}
};

//Data components are elements in the tree that don't render themselves, but make it possible to
//use the generic incremental change mechanism to update parts of a Component
//This can be used for example to incrementally update the list of options in a dropdown component
itasks.Data = {
	init: function () { return Promise.resolve(); },
	beforeRemove: function() {},
	_beforeRemove: function() {},
};

//Convenience function for concisely creating viewports
itasks.viewport = function(spec,domEl) {
	var vp = {}, vpattr = {};

	Object.assign(vpattr,itasks.Component.attributes,itasks.Viewport.attributes,spec.attributes);
	Object.assign(vp,itasks.Component,itasks.Viewport,spec);
	
	vp.attributes = vpattr;
	vp.domEl = domEl;
	vp.init();
};

itasks.Connection = {
	wsock: null,
	endpointUrl: null,
	pingTimer: null,
	pingInterval: 10000, //10 seconds

	newSessionCallbacks: {},
	taskInstanceCallbacks: {},

	reqId: 0,
	deferred: [],

	connect: function() {
		var me = this;

		me.wsock = new WebSocket(me.endpointUrl);
		me.wsock.onopen = function() {
			//First send deferred messages
			me.deferred.forEach(function(msg) { me.wsock.send(msg);});
			//Start ping timer
			me.pingTimer = window.setInterval(me._ping.bind(me), me.pingInterval);
		};	
		me.wsock.onmessage = me.onMessage_.bind(me);
		me.wsock.onerror = me.onError_.bind(me);
		me.wsock.onclose = me.onClose_.bind(me);
	},
	_ping: function() {
		var me = this, reqId = me.reqId++;
		if(me._isConnected()) {
			me.wsock.send(JSON.stringify([parseInt(reqId),'ping',{}]));
		}
	},
	_send: function(msg) {
		var me = this;	
		if(me._isConnected()) {
			me.wsock.send(JSON.stringify(msg));
		} else {
			me.deferred.push(JSON.stringify(msg));
		}
	},
	_isConnected: function() {
		return (this.wsock !== null && this.wsock.readyState == 1)
	},
	newSession: function(onStart, onUIChange, onException) {
		var me = this, reqId = me.reqId++;
			
		me.newSessionCallbacks[reqId] = 
			{ 'onStart' : onStart
			, 'onUIChange' : onUIChange
			, 'onException' : onException
			};

		me._send([parseInt(reqId),'new',{}]);
	},
	attachTaskInstance: function(instanceNo, instanceKey, onUIChange, onException) {
		var me = this, reqId = me.reqId++;

		me.taskInstanceCallbacks[instanceNo] =
			{ 'onUIChange' : onUIChange
			, 'onException' : onException
			};

		me._send([parseInt(reqId),'attach',{'instanceNo':parseInt(instanceNo),'instanceKey': instanceKey}]);
	},
	detachTaskInstance: function(instanceNo) {
		var me = this, reqId = me.reqId++;

		delete(me.taskInstanceCallbacks[instanceNo]);
		me._send([parseInt(reqId),'detach',{'instanceNo':parseInt(instanceNo)}]);
	},
	sendEditEvent: function(instanceNo, taskNo, edit, value) {
		var me = this, reqId = me.reqId++;
		me._send([parseInt(reqId),'ui-event',{'instanceNo':parseInt(instanceNo),'taskNo':parseInt(taskNo),'edit':edit, 'value': value}]);
	},
	sendActionEvent: function(instanceNo, taskNo, action) {
		var me = this, reqId = me.reqId++;
		me._send([parseInt(reqId),'ui-event',{'instanceNo':parseInt(instanceNo),'taskNo':parseInt(taskNo),'action': action}]);
	},
	disconnect: function() {
		var me = this;
		if(me.pingTimer !== null) {
			window.clearInterval(me.pingTimer);
			me.pingTimer = null;
		}
		if(me.wsock !== null) {
			me.wsock.close();
			me.wsock = null;
		}
	},
	onError_: function(e) {
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException(e);});
	},
	onReset_: function(e) {
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException(e);});
	},
	onClose_: function(e) {
		//If there are still attached task instances, we consider it an exeption for those viewports
		Object.values(this.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException("The connection to the server closed unexpectedly");});
	},
	onMessage_: function(e) {
		var me = this,
			msg = JSON.parse(e.data);

		//Check the message is a triplet
		if(msg instanceof Array && msg.length == 3) {
			var reqId = msg[0], reqType = msg[1], reqArgs = msg[2];

			switch(reqType) {
				case 'new':
					if(reqId in me.newSessionCallbacks) {
						var callbacks = me.newSessionCallbacks[reqId];

						callbacks.onStart(reqArgs.instanceNo, reqArgs.instanceKey);
						me.attachTaskInstance(reqArgs.instanceNo, reqArgs.instanceKey, callbacks.onUIChange,callbacks.onException);

						delete me.newSessionCallbacks[reqId];
					}
					break;
				case 'attach':
					break;
				case 'detach':
					break;
				case 'ping':
					break;
				case 'ui-change':
					if('instanceNo' in reqArgs && 'change' in reqArgs && reqArgs.instanceNo in me.taskInstanceCallbacks) {
						me.taskInstanceCallbacks[reqArgs.instanceNo].onUIChange(reqArgs.change);
					} 
					break;
				case 'exception':
					if('instanceNo' in reqArgs && 'description' in reqArgs) {
						//The exception targeted at one task instance, so notify all viewports
						if (reqArgs.instanceNo in me.taskInstanceCallbacks) {
							me.taskInstanceCallbacks[reqArgs.instanceNo].onException(reqArgs.description);
						} 
					} else {
						//The exception is not specific for one task instance, so notify all viewports
						Object.values(me.taskInstanceCallbacks).forEach(function(callbacks) { callbacks.onException(reqArgs.description);});
					}
					break;
			}
		}
	}
};

itasks.ConnectionPool = {
	connections: {}, //Multiple connections, indexed by endpointUrl

	getConnection: function (endpointUrl) {
		var me = this, connection;

		//Reuse an existing connection
		if(me.connections[endpointUrl]) {
			return me.connections[endpointUrl];
		} else {
			//Create the connection
			connection = Object.assign(Object.create(itasks.Connection),{endpointUrl: endpointUrl});
			connection.connect();
			me.connections[endpointUrl] = connection;
			return connection;
		}
	}
};

//Global functions that you can use to trigger edit events from pieces of html code displayed by components
itasks.htmlEvent = function(event, msg) {
	var domEl = event.target;
	var component = null;

	event.preventDefault();

	while(domEl !== null && (component = itasks.components.get(domEl)) == null) {
		domEl = domEl.parentElement;
	}
	if(component !== null) {
		component.onHtmlEvent(msg);
	}
}
