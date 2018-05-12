//Test script for new itasks embedding style
itasks = {};

//Core behavior
itasks.Component = {

	domTag: 'div',
	domEl: null,

	container: true,
	containerEl: null,

	cssPrefix: 'itasks-',
	cssCls: 'component',

	attributes: {
		width: 'flex',
		height: 'flex',
		direction: 'vertical'
	},

	parentCmp: null,
	children: [],

	initialized: false,

	init: function() {
		var me = this;
		me.lastFire = 0;
		me.initSaplCustomization();
		me.initComponent();
		me.initChildren();
		me.renderComponent();

		me.initialized = true;
		return me;
	},
	initSaplCustomization: function() { //When necessary, apply customizatons for Check if some of the component's methods are custom defined using sapl/js
		var me = this, fun, evalfun;
		//Initialize linked sapl functions 
		if(me.attributes.saplDeps != null && me.attributes.saplDeps != '') {
			if(typeof SAPL_DEBUG !== 'undefined' && SAPL_DEBUG) {
				console.log("BEGIN SAPL DEBUG");
				console.log(me.attributes.saplDeps);
				console.log("END SAPL DEBUG");
			} else {
				me.evalJs(me.attributes.saplDeps);
        	}
			me.replaceJsDynamicUnify();
		}
		//Decode and evaluate the sapl initialization function
		if(me.attributes.saplInit !=null && me.attributes.saplInit!= '') {
			Sapl.feval([me.evalJsVal(me.attributes.saplInit),[___wrapJS(me),"JSWorld"]]);
		}
	},
	initComponent: function() {}, //Abstract method: every component implements this differently
	initChildren: function() {
		var me = this;
		me.children.forEach(function(spec,i) {
			me.beforeChildInsert(i,spec);
			me.children[i] = me.createChild(spec);
			me.children[i].init();
			me.afterChildInsert(i,me.children[i]);
		});
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
			me.domEl.classList.add(me.attributes['class']);
		}
		//Custom initialization after the dom element has been rendered
		me.initDOMEl();
		//Size the element
		me.initDOMElSize();
		//Set margins and alignment
		me.initDOMElMargins();
		me.initContainerEl();

		//Add the the child renderings 
		me.children.forEach(function(child) {
			if(child.domEl) {
				me.containerEl.appendChild(child.domEl);
			}
		});
	},
	initDOMEl: function() {},

	initDOMElSize: function() {
		var me = this,
			el = me.domEl,
			width = me.attributes.width,
			height = me.attributes.height,
			direction = (me.parentCmp && me.parentCmp.attributes.direction) || 'vertical';

		//Set width
		if(width === 'flex') {
			if(direction == 'horizontal') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (width === 'wrap') {
			if(direction == 'horizontal') {
				el.classList.add(me.cssPrefix + 'wrapping-horizontal');
            }
        } else {
			el.style.width = width + 'px';
			el.style.minWidth = width + 'px';
			me.containerEl.style.overflowX = 'auto';
        }
		//Set height
		if(height === 'flex') {
			if(direction == 'vertical') {
				el.style.flex = 1;
				el.style.webkitFlex = 1;
			} else {
				el.style.alignSelf = 'stretch';
				el.style.webkitAlignSelf = 'stretch';
			}
		} else if (height === 'wrap') {
			if(direction == 'vertical') {
				el.classList.add(me.cssPrefix + 'wrapping-vertical');
			}
		} else {
			el.style.height = height + 'px';
			el.style.minHeight = height + 'px';
			me.containerEl.style.overflowY = 'auto';
		}
    },
	initDOMElMargins: function() {
		var me = this,
			el = me.domEl,
            width = me.attributes.width,
            height = me.attributes.height;

		if(!me.parentCmp) { //Do not set margins on the root component, let the embedding page handle that
			return;
		}
		var parentDirection = (me.parentCmp && me.parentCmp.attributes.direction) || 'vertical',
            parentVAlign = (me.parentCmp && me.parentCmp.attributes.valign) || 'top',
            parentHAlign = (me.parentCmp && me.parentCmp.attributes.halign) || 'left',
			curIdx = me.parentCmp.findChild(me),
			lastIdx = me.parentCmp.children.length - 1,
			isFirst = (curIdx == 0),
			isLast = (curIdx == lastIdx);

        //Set left and right margins as specified
		if('marginLeft' in me.attributes) { el.style.marginLeft = me.attributes.marginLeft + 'px'; }
		if('marginRight' in me.attributes) { el.style.marginRight = me.attributes.marginRight + 'px' ; }
	
		//Because vertical borders 'collapse' into each other, we never set the
		//bottom-margin, but set top-margin's that also include the bottom margin of
		//the previous element
		if(parentDirection == 'vertical' && !isFirst) {
			//The first element never sets a top-margin. Its top-margin is added to the parent's padding
			//and its bottom margin is added to the next elements top-margin 
			el.style.marginTop = ((me.attributes.marginTop || 0) + (me.parentCmp.children[curIdx - 1].attributes.marginBottom || 0)) + 'px';
		}

		//Set margins to auto based on alignment of parent
        if(parentDirection == 'vertical') {
			if(width !== 'flex') {
				switch(parentHAlign) {
					case 'left': el.style.marginRight = 'auto'; break;
					case 'center': el.style.marginRight = 'auto'; el.style.marginLeft = 'auto'; break;
					case 'right': el.style.marginLeft = 'auto'; break;
				}
			}
            //If this element is the first, maybe also adjust top margin;
            if(curIdx === 0 && (parentVAlign == 'middle' || parentVAlign == 'bottom')) {
				el.style.marginTop = 'auto';
			}
			//If this element is the last, maybe also adjust bottom margin;
			if(curIdx === lastIdx && (parentVAlign == 'middle' || 'top')) {
				el.style.marginBottom = 'auto';
			}
		} else {
			if(height !== 'flex') {
				switch(parentVAlign) {
					case 'top': el.style.marginBottom = 'auto'; break;
					case 'middle': el.style.marginBottom = 'auto'; el.style.marginTop = 'auto'; break;
					case 'bottom': el.style.marginTop = 'auto'; break;
				}
			}
			//If this element is the first, maybe also adjust left margin;
			if(curIdx === 0 && (parentHAlign == 'center' || parentHAlign == 'right')) {
                el.style.marginLeft = 'auto';
            }
            //If this element is the last, maybe also adjust right margin;
            if(curIdx === lastIdx && (parentHAlign == 'center' || parentHAlign == 'left')) {
                el.style.marginRight = 'auto';
			}
		}
	},
	initContainerEl: function() {
		var me = this,
            el = me.containerEl,
            horizontal = (me.attributes.direction && (me.attributes.direction === 'horizontal')) || false,
			paddingTop, paddingBottom;
	
		if(me.container === false) {
			return;
		}

        el.classList.add(me.cssPrefix + (horizontal ? 'hcontainer' : 'vcontainer'));

		//Set padding
		if(me.attributes.paddingRight) { el.style.paddingRight = me.attributes.paddingRight + 'px' ; }
		if(me.attributes.paddingLeft) { el.style.paddingLeft = me.attributes.paddingLeft + 'px' ; }

		paddingTop = me.attributes.paddingTop || 0;			
		paddingBottom = me.attributes.paddingBottom || 0;
		if(me.children.length > 0) {
			paddingTop += (me.children[0].attributes.marginBottom || 0);
			paddingBottom += (me.children[me.children.length - 1].attributes.marginTop || 0);
		}
		el.style.paddingTop = paddingTop + 'px';
		el.style.paddingBottom = paddingBottom + 'px';
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

		if(me.initialized) {
			//Initialize, if we are already initialized
			child.init();
			//Add the child to the dom
			if(child.domEl) {
				if(isLast) {
					me.containerEl.appendChild(child.domEl);
				} else {
					me.containerEl.insertBefore(child.domEl,me.containerEl.childNodes[idx]);
				}
				child.onShow();
			}
		} 
		me.afterChildInsert(idx,child);

		//When the child is first added, we trigger a resize event
		if(child.onResize) {
			child.onResize();
		}
	},
	beforeChildInsert: function(idx,spec) {},
	afterChildInsert: function(idx,child) {},
	removeChild: function(idx = 0) {
		var me = this, child = me.children[idx];

		child.beforeRemove();
		me.beforeChildRemove(idx,child);

		if(me.initialized && child.domEl) {
			me.containerEl.removeChild(me.containerEl.childNodes[idx]);
		}
		me.children.splice(idx,1);	
	},
	replaceChild: function(idx,spec) {
		var me = this;
		if(idx >= 0 && idx < me.children.length) {
			me.removeChild(idx);
			me.insertChild(idx,spec);
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
	beforeRemove: function() {
		this.children.forEach(function (child){
			child.beforeRemove();
		});
	},
	setAttribute: function(name,value) {
		var me = this;
	
		me.attributes[name] = value;	
		me.onAttributeChange(name,value);
	},
	onAttributeChange: function(name,value) {},
	onUIChange: function(change) {
		var me = this;
		if(change) {
			switch(change.type) {
				case 'replace':
					me.onReplaceUI(change.definition);
					break;
				case 'change':
					me.onChangeUI(change.attributes,change.children);
					break;
			}
		}
	},
	onReplaceUI: function(spec) {
		var me = this, idx;

		if(me.parentCmp) {
			idx = me.parentCmp.findChild(me);
			me.parentCmp.replaceChild(idx,spec);
		}
	},
	onChangeUI: function(attributeChanges,childChanges) {
		var me = this, idx;

		//Handle attribute changes
		if(attributeChanges instanceof Array) {
			attributeChanges.forEach(function(change) {
				me.setAttribute(change.name,change.value);
			});
		}
		//Handle child changes
		childChanges.forEach(function(change) {
			var idx = change[0];
			switch(change[1]) {
				case 'change':
					if(idx >= 0 && idx < me.children.length) {
						me.children[idx].onUIChange(change[2]);
					} else {
						console.log("UNKNOWN CHILD",idx,me.children.length,change);
					}
					break;
				case 'insert':
					me.insertChild(idx,change[2]);
					break;
				case 'remove':
					me.removeChild(idx);
					break;
				case 'move':
					me.moveChild(idx,change[2]);
					break;
			}
		});
	},
	onShow: function() {
		this.children.forEach(function(child) { if(child.onShow) {child.onShow();}});
	},
	onHide: function() {
		this.children.forEach(function(child) { if(child.onHide) {child.onHide();}});
	},
	onResize: function() {
		this.children.forEach(function(child) { if(child.onResize) {child.onResize();}});
	},
	/* Utility methods */
	evalJs: function(js) {
		var h = document.getElementsByTagName("head")[0],
			s = document.createElement("script");
		s.type = "text/javascript";
		s.appendChild(document.createTextNode(js));
		h.appendChild(s);
		h.removeChild(s);
		return null;
	},
	evalJsVal: function(js) {
		var out;
 		eval("out = " + js + ";");
        return out;
	},
	replaceJsDynamicUnify: function() {
		//Make sure that the dynamics unification is specialized for javavascript functions
		if(typeof ___SystemDynamic__unify === "function" && ___SystemDynamic__unify != _gen_unify){
			_orig_unify_fun = ___SystemDynamic__unify;
			___SystemDynamic__unify = _gen_unify;
		}
	}
};
itasks.Loader = {
	cssCls: 'loader',
	initDOMEl: function() {
		var me = this,
			l = document.createElement('div');
			l.classList.add(me.cssPrefix + 'loader-spinner');
		me.domEl.appendChild(l);
		if(me.attributes.taskId){
			me.doEditEvent(me.attributes.taskId, me.attributes.editorId, true);
		}
	}
};
itasks.ExceptionView = {
	cssCls: 'exception',
	container: false,
	initDOMEl: function() {
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

		//Get a connection
		me.taskUrl = me.determineTaskEndpoint();
		me.connection = itasks.ConnectionPool.getConnection(me.taskUrl);

		var uiChangeCallback = me.onInstanceUIChange.bind(me);
		var exceptionCallback = me.onException.bind(me);

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
	},
	getParentViewport: function() {
		var me = this, parentVp = me.parentCmp;
		while(parentVp) {
			if(parentVp.cssCls == 'viewport') { //Bit of a hack...
				return parentVp;
			}
			parentVp = parentVp.parentCmp;
		}
		return null;
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
		var parentVp = me.getParentViewport();
		if(parentVp === null) {
			//If there is no parent, use the default url
			return 'ws://' + location.host + location.pathname + (location.pathname.endsWith('/') ? '' : '/') + 'gui-wsock';
		} else {
			return parentVp.determineTaskEndpoint();
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
			if(change.type == 'replace' && change.definition.attributes.title) {
				document.title = change.definition.attributes.title;
			}
			if(change.type == 'change' && change.attributes.length > 0) {
				change.attributes.forEach(function(change) {
					if(change.name == 'title') {
						document.title = change.value;
					}
            	});
			}
		}
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
	}
};

//Data components are elements in the tree that don't render themselves, but make it possible to
//use the generic incremental change mechanism to update parts of a Component
//This can be used for example to incrementally update the list of options in a dropdown component
itasks.Data = {
	init: function () { return this; },
    beforeRemove: function() {}
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

