itasks.TextView = {
	cssCls: 'textview',
	container: false,
	initDOMEl: function() {
		this.domEl.innerHTML = this.attributes.value || '';
	},
	setValue: function(html) {
		this.domEl.innerHTML = html;	
	},
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
};
itasks.HtmlView = {
	cssCls: 'htmlview',
	initDOMEl: function() {
		this.domEl.innerHTML = this.attributes.value || '';
	},
	setValue: function(html) {
		this.domEl.innerHTML = html;	
	},
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': this.domEl.innerHTML = value; break;
		}
	}
};
itasks.ProgressBar = {
    domTag: 'progress',
	cssCls: 'progress',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;

        el.innerHTML = me.attributes.text;
		el.min = 0;
        el.max = 100;
        if(typeof me.attributes.value == 'number') {
            el.value = me.attributes.value;
        }
    },
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value': if (value == null) this.domEl.removeAttribute('value'); else this.domEl.value = value; break;
			case 'text': this.domEl.innerHTML = value; break;
		}
	}
};
