implementation module iTasks.Extensions.Form.Pikaday

import StdEnv
import iTasks, Data.Func
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.JavaScript
import iTasks.UI.Editor.Modifiers, iTasks.UI.Editor.Controls
import iTasks.Extensions.DateTime
import qualified Data.Map as DM

PIKADAY_JS_URL :== "/pikaday/pikaday.js"
PIKADAY_CSS_URL :== "/pikaday/css/pikaday.css"
MOMENT_JS_URL :== "/momentjs/moment.min.js"

pikadayField :: Editor String
pikadayField = leafEditorToEditor {LeafEditor|genUI = withClientSideInit initUI genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI attr dp mode vst=:{VSt|taskId,optional}
		# val = editModeValue mode
		# valAttr = maybe JSONNull JSONString val
		# attr = 'DM'.unions [optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr valAttr, attr]
		= (Ok (uia UITextField attr, val),vst)

	initUI me world
		//Load css
		# world      = addCSSFromUrl PIKADAY_CSS_URL world
		//Defer further action till after the field is created...
		# (cb,world) = jsWrapFun (initDOMEl me) me world
		# world      = (me .# "initDOMEl" .= cb) world
		# (cb,world) = jsWrapFun (beforeRemove me) me world
		# world      = (me .# "beforeRemove" .= cb) world
		= world

	beforeRemove me args = snd o (me .# "picker" .# "destroy" .$ ())

	initDOMEl me args world
		# (cb,world) = jsWrapFun (initDOMEl` me) me world
		# world      = addJSFromUrl MOMENT_JS_URL (Just cb) world
		= world

	initDOMEl` me args world
		# (cb,world) = jsWrapFun (initDOMEl`` me) me world
		# world      = addJSFromUrl PIKADAY_JS_URL (Just cb) world
		= world

	initDOMEl`` me args world
		//Create pikaday object
		# (value,world)     = me .# "attributes.value" .? world
		# (domEl,world)     = me .# "domEl" .? world
		# world             = (domEl .# "value" .= value) world
		//Create onselect/keyup
		# (onSelectCb,world) = jsWrapFun (onSelect me) me world
		# (onKeyupCb,world)  = jsWrapFun (onKeyup me) me world
		# (cfg,world)        = jsEmptyObject world
		# world              = (cfg .# "field" .= domEl) world
		# world              = (cfg .# "format" .= "YYYY-MM-DD") world
		# world              = (cfg .# "firstDay" .= 1) world
		# world              = (cfg .# "onSelect" .= onSelectCb) world
		# world              = (domEl .# "addEventListener" .$! ("keyup", onKeyupCb)) world
		# (picker,world)     = jsNew "Pikaday" cfg world
		# world              = (me .# "picker" .= picker) world
		//Handle attribute changes
		# (cb,world)         = jsWrapFun (onAttributeChange picker me) me world
		# world              = (me .# "onAttributeChange" .= cb) world
		//React to selects
		= world

	onAttributeChange picker me {[0]=name,[1]=value} world
		| jsValToString name <> Just "value"
			= world
		# world = (me.# "noEvents" .= True ) world
		# world = (picker .# "setDate" .$! value) world
		# world = (me.# "noEvents" .= False) world
		= world

	onSelect me args world
		# (noEvents,world)  = me .# "noEvents" .? world
		| not (jsIsUndefined noEvents) && jsValToBool noEvents == Just True
			= world
		# (value,world)     = (me .# "picker.toString" .$ "YYYY-MM-DD") world
		# value             = jsValToString value
		# (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# (_,world)         = (me .# "doEditEvent" .$ (taskId, editorId, toJSON value)) world
		= world

	onKeyup me args world
		# (taskId,world)   = me .# "attributes.taskId" .? world
		# (editorId,world) = me .# "attributes.editorId" .? world
		# (value,world)    = me .# "domEl.value" .? world
		# value            = jsValToString value
		# world            = (me .# "doEditEvent" .$! (taskId, editorId, toJSON value)) world
		= world

	onEdit dp (tp,e) _ vst = (Ok (ChangeUI [SetAttribute "value" (JSONString (fromMaybe "" e))] [], e),vst)

	onRefresh dp new st vst=:{VSt| optional}
		| st === Just new = (Ok (NoChange, st), vst)
		| otherwise       = (Ok (ChangeUI [SetAttribute "value" (JSONString new)] [], (Just new)), vst)

	valueFromState s = s

pikadayDateField :: Editor Date
pikadayDateField = selectByMode
	(bijectEditorValue toString fromString textView)
	(injectEditorValue toString parseDate (withDynamicHintAttributes "date (yyyy-mm-dd)" (withEditModeAttr pikadayField)))
	(injectEditorValue toString parseDate (withDynamicHintAttributes "date (yyyy-mm-dd)" (withEditModeAttr pikadayField)))
