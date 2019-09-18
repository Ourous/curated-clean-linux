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
	domTag: 'div',
	cssCls: 'progress',
	initDOMEl: function() {
		var me = this,
			el = this.domEl;

		//https://stackoverflow.com/questions/41429906/how-to-display-data-label-inside-html5-progress-bar-cross-browser-compatible
		var child = document.createElement('span');
		child.setAttribute('class', 'value');
		el.appendChild(child);

		me.setProgress(me.attributes.value);
		me.setText(me.attributes.text);
	},
	setProgress:function(value) {
		this.domEl.children[0].style=typeof value == 'number' ? ('width:'+value+'%;') : '';
	},
	setText:function(text) {
		this.domEl.setAttribute('title', text);
	},
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'value':
				this.setProgress(value);
				break;
			case 'text':
				this.setText(value);
				break;
		}
	}
};
