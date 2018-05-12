itasks.Raw = {
	initDOMEl: function() {
		var me = this;
		me.headerEl = document.createElement('div');
		me.headerEl.classList.add(me.cssPrefix + 'header');
		me.headerEl.innerHTML = me.type + ": " + JSON.stringify(me.attributes);

		me.domEl.appendChild(me.headerEl);
		me.containerEl = document.createElement('div');
		me.containerEl.classList.add(me.cssPrefix + 'inner');
		me.domEl.appendChild(me.containerEl);
	},
	onAttributeChange: function(name,value) {
		me.headerEl.innerHTML = me.type + ": " + JSON.stringify(me.attributes);
	}
};
itasks.RawRecord = Object.assign({},itasks.Raw,{
	cssCls: 'raw-record'
});
itasks.RawCons = Object.assign({},itasks.Raw,{
	cssCls: 'raw-cons'
});
itasks.RawVarCons = Object.assign({},itasks.Raw,{
	cssCls: 'raw-var-cons'
});
itasks.RawInteract = Object.assign({},itasks.Raw,{
	cssCls: 'raw-interact'
});
itasks.RawStep = Object.assign({},itasks.Raw,{
	cssCls: 'raw-step',	
});
itasks.RawParallel = Object.assign({},itasks.Raw,{
	cssCls: 'raw-parallel',	
});
itasks.RawEmpty = {
	cssCls: 'raw-empty',
	initDOMEl: function() {
		this.domEl.innerHTML = '(empty)';
	}
};
itasks.RawAction = {
	cssCls: 'raw-action',	
	domTag: 'a',
	attributes: {
		width: 'wrap'
	},
	initDOMEl: function() {
		var me = this, el = me.domEl;

		el.innerHTML = me.attributes.actionId;
		el.href = '#';
		el.classList.add(this.cssPrefix + (me.attributes.enabled ? 'raw-action-enabled' : 'raw-action-disabled'));
		el.addEventListener('click',function(e) {
			me.doEditEvent(me.attributes.taskId,null,me.attributes.actionId);
			e.preventDefault();
		});
    },
	onAttributeChange: function(name,value) {
		var me = this, el = me.domEl;
		switch(name) {
			case 'enabled':
				if(value) {
					el.classList.remove(this.cssPrefix + 'raw-action-disabled');
					el.classList.add(this.cssPrefix + 'raw-action-enabled');
				} else {
					el.classList.remove(this.cssPrefix +'raw-action-enabled');
					el.classList.add(this.cssPrefix +'raw-action-disabled');
				}
				break;
		}
	}
};
