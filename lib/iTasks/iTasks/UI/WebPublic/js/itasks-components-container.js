itasks.Container = {
	cssCls: 'container',
	initDOMEl: function() {
		var me = this;
		if(me.baseCls) {
			me.domEl.classList.add(me.baseCls);
		}
	}
};

itasks.Panel = {
	cssCls: 'panel',
	initDOMEl: function() {
		var me = this,
			isTab = (me.parentCmp && me.parentCmp.type == 'TabSet');

		//Add top sizer
		if(me.attributes.resizable && me.attributes.resizable.includes('top')) { 
			me.domEl.append(me.createSizer());
		}

		//Create header
		if(me.attributes.title && !isTab) {
			me.headerEl = document.createElement('div');
			me.headerEl.classList.add(me.cssPrefix + 'header');
			me.headerEl.innerHTML = '<span>' + me.attributes.title + '</span>';
			me.domEl.appendChild(me.headerEl);
		}

		//Create separate container div
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);

		//Add bottom sizer
		if(me.attributes.resizable && me.attributes.resizable.includes('bottom')) { 
			me.domEl.append(me.createSizer());
		}

		if(me.frame) {
			me.domEl.classList.add(me.cssPrefix + 'framed');
		}
		if(me.baseCls) {
			me.domEl.classList.add(me.attributes.baseCls);
		}

		//Fullscreenable
		if(me.attributes.fullscreenable){
			me.domEl.style.position = 'relative';
			var fullscreener = document.createElement('a');
			var button = document.createElement('div');
			button.classList.add(me.cssPrefix + "button-icon");
			button.classList.add("icon-fullscreen");
			fullscreener.appendChild(button);
			fullscreener.style.position = 'absolute';
			fullscreener.style.bottom = 0;
			fullscreener.style.right = 0;
			fullscreener.href = '#';

			me.fullscreen = false;
			fullscreener.onclick = function () {
				if(me.fullscreen){
					me.domEl.style.position = 'relative';
					me.domEl.style.top = null;
					me.domEl.style.left = null;
					me.domEl.style.right = null;
					me.domEl.style.bottom = null;
					me.domEl.style.width = null;
					me.domEl.style.height = null;
					fullscreener.style.zIndex = null;
					me.domEl.style.zIndex = null;
					me.fullscreen = false;
				} else {
					me.domEl.style.position = 'absolute';
					me.domEl.style.top = 0;
					me.domEl.style.left = 0;
					me.domEl.style.right = 0;
					me.domEl.style.bottom = 0;
					me.domEl.style.width = '100%';
					me.domEl.style.height = '100%';
					fullscreener.style.zIndex = 999;
					me.domEl.style.zIndex = 998;
					me.fullscreen = true;
				}
			};
			me.domEl.appendChild(fullscreener);
		}
	},
	createSizer: function() {
	
		var me = this,
		    el = document.createElement('div');

		el.classList.add(me.cssPrefix + 'vsizer');
		el.addEventListener('mousedown', function init (e){
			
			var startPos = e.clientY;
			var startSize = parseInt(window.getComputedStyle(me.domEl).getPropertyValue('height').slice(0,-2));
			
			var resize = function resize(ev) {
				me.domEl.style['height'] = (startSize + (ev.clientY - startPos)) + 'px';
			};

			window.addEventListener('mousemove', resize, false);
			window.addEventListener('mouseup', function stop(e){
					window.removeEventListener('mousemove', resize, false);
					window.removeEventListener('mouseup', stop, false);
				}, false);
			}, false);
	
		return el;	
	},

};
itasks.TabSet = {
	cssCls: 'tabset',
    activeTab: 0,
	attributes: {
		height: 'flex',
		width: 'flex'
	},
	replacing: false,

	initComponent: function() {
		var me = this;
		me.children.forEach(function(child,i) {
			child.selected = (i == me.activeTab);
		});
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl;

        me.tabBar = document.createElement('ul');
        me.tabBar.classList.add(me.cssPrefix + 'tabbar');

		//Create tabs for the initial children
		me.children.forEach(function(child,i) {
			me.tabBar.appendChild(me.createTabEl(child));
		});	
		
        me.domEl.appendChild(me.tabBar);

        me.containerEl = document.createElement('div');
        me.containerEl.classList.add(me.cssPrefix + 'tabitems');
        me.domEl.appendChild(me.containerEl);
    },
	createTabEl: function (cmp) {
		var me = this, tab, label, icon;
		tab = document.createElement('li');
		label = document.createElement('a');
		if (cmp.type == 'Button'){
			label.innerHTML = '<span>'+ (cmp.attributes.text || '-')+'</span>';
			label.href = '#';
			label.addEventListener('click',function(e) {
            	if(cmp.attributes.enabled) {
					cmp.doEditEvent(cmp.attributes.taskId,cmp.attributes.editorId,cmp.attributes.value);
            	}
				e.preventDefault();
			},me);
			if(!cmp.attributes.enabled) {
				tab.classList.add(me.cssPrefix + 'tab-disabled');
			}
			cmp.domEl.style.display = "none";
		} else {
			label.innerHTML = '<span>'+ (cmp.attributes.title || '-')+'</span>';
			label.href = '#';
	
			label.addEventListener('click',function(e) {
				var tabEl = e.target.parentElement.parentElement,	
					tabBar = tabEl.parentElement,
					idx = Array.prototype.indexOf.call(tabBar.children,tabEl);
	
				me.setActiveTab(idx);
				e.preventDefault();
			},me);
		}

		if(cmp.attributes.iconCls) {
			icon = document.createElement('div');
			icon.classList.add(me.cssPrefix + 'tabicon');
			icon.classList.add(cmp.attributes.iconCls);
			label.insertBefore(icon,label.childNodes[0]);
		}
		tab.appendChild(label);
	
		if (cmp.type !== 'Button'){
			if(cmp.attributes.closeTaskId) {
				closeLink = document.createElement('a');
				closeLink.innerHTML = 'x';
				closeLink.href = '#';
				closeLink.classList.add(me.cssPrefix + 'tabclose');
				closeLink.addEventListener('click',function(e) {
					me.doEditEvent(cmp.attributes.closeTaskId,null,'Close');
					e.preventDefault();
				},me);
	
				tab.appendChild(closeLink);
			}
			if(cmp.selected) {
				tab.classList.add(me.cssPrefix + 'selected');
			}
		}
		return tab;
	},
	setActiveTab: function(idx) {
        var me = this;

		//Deselect previously selected tab
        if(me.children[me.activeTab]) {
            me.children[me.activeTab].domEl.classList.remove(me.cssPrefix + 'selected');
            me.tabBar.children[me.activeTab].classList.remove(me.cssPrefix + 'selected');
			me.children[me.activeTab].onHide();
        }
		
        me.activeTab = idx || 0;
		//Select new tab 
        if(me.children[me.activeTab]) {
            me.children[me.activeTab].domEl.classList.add(me.cssPrefix + 'selected');
            me.tabBar.children[me.activeTab].classList.add(me.cssPrefix + 'selected');
			me.children[me.activeTab].onShow();
        }
	},
	beforeChildInsert: function(idx,spec) {
		var me = this;

		//Overwrite size to always be flex items
		spec.width = 'flex';
		spec.height = 'flex';
	},
	afterChildInsert: function(idx) {
		var me = this,
			child = me.children[idx];

		//Add tab style
		child.domEl.classList.add(me.cssPrefix + 'tabitem');
		if(child.selected) {
			child.domEl.classList.add(me.cssPrefix + 'selected');
		}

		if(me.initialized) {
			var tabEl = me.createTabEl(child);
			if(idx >= me.tabBar.children.length) {
				me.tabBar.appendChild(tabEl);
			} else {
				me.tabBar.insertBefore(tabEl,me.tabBar.children[idx]);
			}

			if(me.replacing || me.children.length == 1) { //Automatically select the first tab
				me.setActiveTab(idx);
			}
		}
	},
	beforeChildRemove: function(idx) {
		var me = this;
		if(me.initialized) {
			if(!me.replacing && (idx == me.activeTab) && (me.children.length > 1)) { //Unless we remove the last tab, select another tab
				me.setActiveTab( (idx == 0) ? 1 : (idx - 1));
			}
			me.tabBar.removeChild(me.tabBar.children[idx]);
		}
	},
	replaceChild: function(idx,spec) {
		var me = this;
		me.replacing = true;
		if(idx >= 0 && idx < me.children.length) {
			me.removeChild(idx);
			me.insertChild(idx,spec);
		}
		me.replacing = false;
	}
}

itasks.Window = {
	attributes: {
    	marginTop: 10, marginRight: 10, marginBottom: 10, marginLeft: 10,
		movable: true,
		resizable: true,
		windowType: 'floating',
		hpos: 'center',
		vpos: 'top',
	},

    initDOMEl: function() {
        var me = this,left,top;

        switch(me.attributes.windowType) {
            case 'bubble':
                me.domEl.classList.add(me.cssPrefix + 'notification-bubble');
                break;
            default:
                me.domEl.classList.add(me.cssPrefix + 'floating-window');
        }
		//Create header
		if(me.attributes.windowType === 'floating' || me.attributes.title) {
			me.headerEl = document.createElement('div');
			me.headerEl.classList.add(me.cssPrefix + 'header');
			me.headerEl.innerHTML = '<span>' + (me.attributes.title || '') + '</span>';
			me.domEl.appendChild(me.headerEl);

        	if(me.attributes.movable) {
            	me.headerEl.addEventListener('mousedown', me.onStartDrag.bind(me));
	            me.headerEl.style.cursor = 'move';
			}
		}
		if(me.attributes.resizable) {
			me.domEl.style.resize = 'both';
			me.domEl.style.overflow = 'scroll';
		}
		//Create separate container div
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);

		//Intially position the window offscreen, it will be repositioned once its dimensions are known
		me.domEl.style.top = '-10000px';
		me.domEl.style.left = '-10000px';
	},
    initSize: function() {},
	onShow: function() {
		//Position the window when it is is first shown
		var me = this, top, left;

		if(!me.positioned) {	
			switch(me.attributes.vpos) {
				case 'top': top = me.attributes.marginTop; break;
				case 'middle': top = (document.body.offsetHeight / 2) - (me.domEl.offsetHeight / 2); break;
				case 'bottom': top = document.body.offsetHeight - me.domEl.offsetHeight - me.attributes.marginBottom; break;
			}
			switch(me.attributes.hpos) {
				case 'left': left = me.attributes.marginLeft; break;
				case 'center': left = (document.body.offsetWidth / 2) - (me.domEl.offsetWidth / 2); break;
				case 'right': left = document.body.offsetWidth - me.domEl.offsetWidth - me.attributes.marginRight; break;
			}

			me.domEl.style.top = top + 'px';
			me.domEl.style.left = left + 'px';
			me.positioned = true;
		}
	},
    onStartDrag: function(e) {
        var me = this;
        e.preventDefault();
        me.lastX = e.clientX;
        me.lastY = e.clientY;
        me.onDragging_ = me.onDragging.bind(me);
        me.onStopDrag_ = me.onStopDrag.bind(me);
        window.addEventListener('mousemove', me.onDragging_);
        window.addEventListener('mouseup', me.onStopDrag_);
    },
    onDragging: function(e) {
        var me = this,
            newX = e.clientX,
            newY = e.clientY,
            diffY = newY - me.lastY,
            diffX = newX - me.lastX,
            left, top;

        left = parseInt(document.defaultView.getComputedStyle(me.domEl,'').getPropertyValue('left'),10);
        top = parseInt(document.defaultView.getComputedStyle(me.domEl,'').getPropertyValue('top'),10);
        me.domEl.style.left = ((left < 0) ? 0 : (left + diffX)) + 'px';
        me.domEl.style.top = ((top < 0) ? 0 : (top + diffY)) + 'px';

        me.lastX = newX;
        me.lastY = newY;
    },
    onStopDrag: function(e) {
        var me = this;
        window.removeEventListener('mousemove', me.onDragging_);
        window.removeEventListener('mouseup', me.onStopDrag_);
    }
};
itasks.ToolBar  = {
	cssCls: 'toolbar',
	attributes: {
		height: 'wrap',
		width: 'flex',
		direction: 'horizontal',
		halign: 'left',
		padding: '2 2 2 2'
	},
};

itasks.ButtonBar  = {
	cssCls: 'buttonbar',
	attributes: {
		height: 'wrap',
		width: 'flex',
		direction: 'horizontal',
		halign: 'right',
		padding: '2 2 2 0'
	}
};
itasks.List = {
	cssCls: 'list'
};
itasks.ListItem = {
	cssCls: 'listitem'
};
itasks.Debug = {
	cssCls: 'debug'
};

itasks.Menu = {
	cssCls: 'menu',
	attributes: {
		height: 'wrap',
		width: 'wrap'
	},
    initDOMEl: function() {
		var me = this;	

		if (!(me.parentCmp.type != 'Menu' && me.children.length == 1)){
			me.labelEl = document.createElement('div');
			me.labelEl.classList.add(me.cssPrefix + 'menu-label');
			me.innerLabelEl = document.createElement('span');
			me.innerLabelEl.innerHTML = me.attributes.text;

			if(me.attributes.iconCls) {
				me.icon = document.createElement('div');
				me.icon.classList.add(me.cssPrefix + 'button-icon');
				me.icon.classList.add(me.attributes.iconCls);
				me.labelEl.appendChild(me.icon);
			}

			me.labelEl.appendChild(me.innerLabelEl);

			me.domEl.appendChild(me.labelEl);

			me.containerEl = document.createElement('div');
			me.containerEl.classList.add(me.cssPrefix + 'menu-content');
			me.domEl.appendChild(me.containerEl);
		}
	},
};

itasks.MenuSep = {
	cssCls: 'menu-sep',
};
