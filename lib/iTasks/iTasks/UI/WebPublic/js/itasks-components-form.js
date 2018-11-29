itasks.TextField = {
	domTag: 'input',
	attributes: {
		eventTimeout: 500,
		height: 'wrap'
	},
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'text';
		el.value = me.attributes.value ? me.attributes.value : '';

		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
			el.addEventListener('keyup',function(e) {
				const rawV = e.target.value;
				var v = Just(rawV);
				if('maxlength' in me.attributes){
					if(rawV.length > me.attributes['maxlength']){
						const shortenedV = rawV.substr(0, me.attributes['maxlength']);
						el.value = shortenedV;
                                                v = Just(shortenedV);
						return;
					}
				}

				if('minlength' in me.attributes){
					if(rawV.length < me.attributes['minlength']){
						v = Nothing;
					}
				}

				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,v);
			});
		}
	},
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus() ) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.TextArea = {
    domTag: 'textarea',
	attributes: {
		height: 'flex',
		width: 'flex',
		minHeight: 150,
		minWidth: 400,
		eventTimeout: 500,
	},
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.innerHTML = me.attributes.value ? me.attributes.value : '';
		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
        	el.addEventListener('keyup',function(e) {
				const rawV = e.target.value;
				var v = Just(rawV);
				if('maxlength' in me.attributes){
					if(rawV.length > me.attributes['maxlength']){
						const shortenedVal = rawV.substr(0, me.attributes['maxlength']);
						el.value = shortenedVal;
						v = Just(shortenedVal);
						return;
					}
				}

				if('minlength' in me.attributes){
					if(rawV.length < me.attributes['minlength']){
						v = Nothing;
					}
				}

				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,v);
        	});
		}
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.PasswordField = {
	domTag: 'input',
	attributes: {
		eventTimeout: 500,
		height: 'wrap'
	},
	initDOMEl: function() {
		var me = this,
			el = this.domEl;
		el.type = 'password';
		el.value = me.attributes.value ? me.attributes.value : '';
		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
			el.addEventListener('keyup',function(e) {
				const rawV = e.target.value;
				var v = Just(e.target.value);
				if('maxlength' in me.attributes){
					if(rawV.length > me.attributes['maxlength']){
						const shortenedVal = rawV.substr(0, me.attributes['maxlength']);
						el.value = shortenedVal;
						v = Just(shortenedVal);
						return;
					}
				}

				if('minlength' in me.attributes){
					if(rawV.length < me.attributes['minlength']){
						v = Nothing;
					}
				}

				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,v);
			});
		}
	}
	,onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.NumberField = {
	domTag: 'input',
    allowDecimal: false,
	attributes: {
		width: 150,
		height: 'wrap'
	},
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'text';
		el.value = (me.attributes.value === undefined || me.attributes.value === null) ? '' : me.attributes.value;

		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
        	el.addEventListener('keyup',function(e) {
				var value = e.target.value == "" ? NaN : Number(e.target.value);
				const isFloat = value % 1 !== 0;
				value = (isNaN(value) || (!me.allowDecimal && isFloat)) ? Nothing : Just(value);
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,value);
        	});
		}
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			if(me.domEl !== document.activeElement || ! document.hasFocus()) { //Don't update the focused element...
				me.domEl.value = (value === null) ? '' : value;
			}
		}
	}
};
itasks.IntegerField = Object.assign(Object.assign({}, itasks.NumberField),{
    allowDecimal: false
});
itasks.DecimalField = Object.assign(Object.assign({}, itasks.NumberField),{
    allowDecimal: true
});
itasks.DocumentField = {
	cssCls: 'edit-document',
        initDOMEl: function() {

        var me = this,
            el = this.domEl;

        //Create a hidden file selector
        me.fileEl = document.createElement('input');
        me.fileEl.type = "file";
        me.fileEl.style.display = "none";
        me.fileEl.addEventListener('change',me.onFileSelect.bind(me));
        el.appendChild(me.fileEl);

        me.labelEl = document.createElement('span');
        el.appendChild(me.labelEl);
        me.actionEl = document.createElement('a');

	// only enable clear link if control is not disabled
	if (!(me.attributes.enabled === false)) {
		me.actionEl.href = "#";
		me.actionEl.addEventListener('click',me.onAction.bind(me));
	}

	el.appendChild(me.actionEl);

        me.xhr = null;
        me.value = me.attributes.value || null;
        me.showValue();
    },
    showUploading: function(progress) {
        this.labelEl.innerHTML = "Uploading... " + progress + "%";
        this.actionEl.innerHTML = "Cancel";
    },
    showValue: function() {
        var me = this;
        if(me.attributes.value !== null) {
            me.labelEl.innerHTML = '<a href="' + me.attributes.value[1] + '" target="_blank">' + me.attributes.value[2] + '</a>';
            me.actionEl.innerHTML = 'Clear';
        } else {
            me.labelEl.innerHTML = 'No file selected';
            me.actionEl.innerHTML = 'Select';
        }
    },
    onAction: function(e) {
        var me = this;
        e.preventDefault();

        if(me.xhr != null) { //Cancel
            me.xhr.abort();
            me.xhr = null;
            me.showValue();
            return;
        }
        if(me.attributes.value != null) { //Clear;
            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,Nothing);
            me.attributes.value = null;
            me.showValue();
        } else { //Select
            me.fileEl.click();
        }
    },
    onFileSelect: function() {
        var me = this,
            fd;

        //Create uploader
        me.xhr = new XMLHttpRequest();
        me.xhr.upload.addEventListener('progress',function(e) {
            me.showUploading(Math.round((e.loaded * 100) / e.total));
        });
        me.xhr.onreadystatechange = me.onUploadStateChange.bind(me);
        me.xhr.open('POST','/upload',true);
        //Add file to upload data
        fd = new FormData();
        fd.append('upload',me.fileEl.files[0]);
        me.xhr.send(fd);
    },
    onUploadStateChange: function(e) {
        var me = this, rsp,doc,value;

        if (me.xhr.readyState == 4 && me.xhr.status == 200) {
            //Upload ready
            rsp = JSON.parse(me.xhr.responseText);
			doc = rsp[0];	
			value = [doc.documentId,doc.contentUrl,doc.name,doc.mime,doc.size];

            //Switch to value state
            me.doEditEvent(me.attributes.taskId,me.attributes.editorId,Just(value));
            me.xhr = null;
			
            me.attributes.value = value;
            me.showValue();
        }
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
			me.setEditorValue(value);
		}
	},
    setEditorValue: function(value) {
		var me = this;

        if(me.xhr != null) {
            me.xhr.abort();
            me.xhr = null;
        }
        me.attributes.value = value; 
        me.showValue();
    }
};
itasks.Checkbox = {
	domTag: 'input',
	attributes: {
    	width: 'wrap'
	},
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'checkbox';
        el.checked = me.attributes.value;

		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
	        el.addEventListener('click',function(e) {
			var value = e.target.checked;
			me.doEditEvent(me.attributes.taskId,me.attributes.editorId,Just(value));
	        });
		}
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
        	me.domEl.checked = value;
		}
	}
};
itasks.Slider = {
	domTag: 'input',
    initDOMEl: function() {
        var me = this,
            el = this.domEl;
        el.type = 'range';
        if ('min' in me.attributes) el.min = me.attributes.min;
        if ('max' in me.attributes) el.max = me.attributes.max;
        el.value = me.attributes.value;

		if('enabled' in me.attributes && me.attributes['enabled'] === false) {
			el.disabled = true;
		} else {
        	el.addEventListener('change',function(e) {
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId, Just(Number(e.target.value)));
        	});
		}
    },
	onAttributeChange: function(name,value) {
		var me = this;
		if(name == 'value') {
        	me.domEl.value = value;
		}
	}
};
itasks.Button = {
	domTag: 'a',
	cssCls: 'button',
	attributes: {
		height: 'wrap',
		width: 'wrap',
		enabled: true
	},
	initDOMEl: function() {
		var me = this,
			el = me.domEl;

		el.href = '#';
		if(me.attributes.iconCls) {
			me.icon = document.createElement('div');
			me.icon.classList.add(me.cssPrefix + 'button-icon');
			me.icon.classList.add(me.attributes.iconCls);
			el.appendChild(me.icon);
		}
		if(!me.attributes.enabled) {
			el.classList.add(me.cssPrefix + 'button-disabled');
		}
		if(me.attributes.text) {
			me.label = document.createElement('div');
			me.label.innerHTML = me.attributes.text;
			me.label.classList.add(me.cssPrefix + 'button-label');
			el.appendChild(me.label);
		}

		el.addEventListener('click',function(e) {
			if(typeof(me.attributes.value) == 'boolean') { //Toggle edit buttons
				me.attributes.value = !me.attributes.value;
			}

			if(me.attributes.enabled) {
				const val      = me.attributes.value;
				const eventVal = typeof(me.attributes.value) == 'string' ?
				                 val :              // action event
				                 Just(val == true); // editor event
				me.doEditEvent(me.attributes.taskId,me.attributes.editorId,eventVal);
			}
			e.preventDefault();
			return false;
		});
	},

	initContainerEl: function() { //Make sure no padding is set on buttons
	},
	onAttributeChange: function(name,value) {
		var me = this;
		switch(name) {
			case 'enabled':
				if(value) {
					me.domEl.classList.remove(me.cssPrefix + 'button-disabled');
				} else {
					me.domEl.classList.add(me.cssPrefix + 'button-disabled');
				}
				break;
		}
	}

};
itasks.Label = {
    domTag: 'label',
	container: false,
	attributes: {
		width: '150px',
		paddingTop: 5,
		paddingRight: 5,
		paddingBottom: 5,
		paddingLeft: 5
	},
    initDOMEl: function() {
        var me = this,
            el = me.domEl;
        el.innerHTML = me.attributes.text;
    },
	onAttributeChange:function(name,value) {
		switch(name) {
			case 'text': this.domEl.innerHTML = value; break;
		}
	}
};
itasks.Icon = {
	attributes: {
		width: 'wrap',
		height: 'wrap'
	},
	initDOMEl: function() {
		var me = this,
			el = me.domEl;
		el.classList.add(me.cssPrefix + 'icon');
		el.classList.add(me.attributes.iconCls);
		me.currentIcon = me.attributes.iconCls;

		if(me.attributes.tooltip) {
			el.setAttribute('tooltip',me.attributes.tooltip);
		}
    },
	onAttributeChange: function(name,value) {
		var me = this,
			el = me.domEl;
		switch(name) {
			case 'iconCls':
				el.classList.remove(me.currentIcon);
				me.currentIcon = value;
				el.classList.add(me.currentIcon);
				break;
			case 'tooltip':
				el.setAttribute('tooltip',value);
				break;
		}
	}
};

