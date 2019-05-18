implementation module iTasks.Extensions.Editors.Ace

import iTasks
import iTasks.UI.Editor, iTasks.UI.Editor.Modifiers, iTasks.UI.Definition
import iTasks.UI.JavaScript
import qualified Data.Map as DM
import Text, Data.Func, StdArray

ACE_JS_URL :== "/ace/src-noconflict/ace.js"
ACE_DEFAULT_THEME :== "ace/theme/chrome"
ACE_DEFAULT_MODE  :== "ace/mode/text"

derive class iTask AceState, AceRange

derive gEditor AceOptions
derive gEq AceOptions
derive gText AceOptions
gDefault{|AceOptions|} = {AceOptions|theme = ACE_DEFAULT_THEME, mode = ACE_DEFAULT_MODE}

derive JSONEncode AceOptions
derive JSONDecode AceOptions

aceTextArea :: Editor String
aceTextArea = bijectEditorValue toAce fromAce aceEditor
where
	aceState = {AceState|lines = [],cursor = (0,0), selection = Nothing, disabled=False}
	toAce s = (defaultValue, {AceState|aceState & lines = split "\n" s})
	fromAce (_,{AceState|lines}) = join "\n" lines

aceEditor :: Editor (!AceOptions,!AceState)
aceEditor = leafEditorToEditor
    { LeafEditor
    | genUI          = withClientSideInit initUI genUI
    , onEdit         = onEdit
    , onRefresh      = onRefresh
    , valueFromState = valueFromState
    }
where
	genUI attr dp mode vst=:{VSt|taskId,optional}
		# (options,state) = fromMaybe gDefault{|*|} $ editModeValue mode
		//Set both state and options as attributes
		# aceAttr = 'DM'.fromList
			[("lines",JSONArray (map JSONString state.AceState.lines))
			,("cursor", toJSON state.AceState.cursor)
			,("selection", maybe JSONNull encodeRange state.AceState.selection)
			,("disabled",JSONBool state.AceState.disabled)
			,("theme",JSONString options.AceOptions.theme)
			,("mode",JSONString options.AceOptions.mode)
			]
		# attr = 'DM'.unions [aceAttr, optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), attr]
		= (Ok (uia UIComponent attr, (options, state)),vst)

	initUI me world
		//Setup UI component
		# world      = (me .# "domTag" .= "pre") world
		# (cb,world) = jsWrapFun (onAttributeChange me) me world
		# world      = (me .# "onAttributeChange" .= cb) world
		//Load Ace javascript
		# (cb,world) = jsWrapFun (\_ -> initUI` me) me world
		# world      = addJSFromUrl ACE_JS_URL (Just cb) world
		= world

	initUI` me world
		//Create Ace editor linked to domEl
		# (domEl,world)     = me .# "domEl" .? world
		# world             = (domEl .# "style.width" .= "100%") world
		# world             = (domEl .# "style.height" .= "100%") world
		# (editor,world)    = (jsGlobal "ace.edit" .$ domEl) world
		# (session,world)   = (editor .# "getSession" .$ ()) world
		# (selection,world) = (session.# "getSelection" .$ ()) world
		# world             = (me .# "editor" .= editor) world
		//Set options
		# (_,world)         = (editor .# "setReadOnly" .$ me .# "attributes.disabled") world
		# (_,world)         = (editor .# "setTheme" .$ me .# "attributes.theme") world
		# (_,world)         = (session .# "setMode" .$ me .# "attributes.mode") world
		//Initialize state based on attributes
		# (lines,world)     = jsValToList` (me .# "attributes.lines") (jsValToString` "") world
		# value             = join "\n" lines
		# (_,world)         = (editor .# "setValue" .$ value) world
		//Set initial cursor position
		# (_,world)       	= (editor .# "navigateTo" .$ (me .# "attributes.cursor[0]", me .# "attributes.cursor[1]")) world
		//Potentially set initial selection
		# (selattr,world)     = me .# "attributes.selection" .? world
		# world = if (jsIsNull selattr) world ((selection .# "setSelectionRange" .$! value) world)
		//Add event listeners
		# (cb,world)     = jsWrapFun (\_ -> onChange editor me) me world
		# (_,world)      = (editor .# "on" .$ ("change",cb)) world
		# (cb,world)     = jsWrapFun (\_ -> onCursorChange selection me) me world
		# (_,world)      = (selection .# "on" .$ ("changeCursor",cb)) world
		# (cb,world)     = jsWrapFun (\_ -> onSelectionChange selection me) me world
		# (_,world)      = (selection .# "on" .$ ("changeSelection",cb)) world
		= world

	onAttributeChange me {[0]=name,[1]=value} world
	# (editor,world)  = me .# "editor" .? world
	= case jsValToString name of
		Just "lines"
			# world           = ((me .# "noEvents") .= True) world //Flag that no events should be sent because we just received the latest value
			# (value,world)   = (value .# "join" .$ "\n") world
			# (_,world)       = (editor .# "setValue" .$ (value,1)) world
			# world           = (me .# "noEvents" .= False) world
			= world
		Just "cursor"
			# (row,world)    = value .# 0 .? world
			# (col,world)    = value .# 1 .? world
			# (_,world)       = ((editor .# "navigateTo") .$ (row,col)) world
			= world
		Just "selection"
			# (editor,world)  = me .# "editor" .? world
			# (session,world)   = ((editor .# "getSession") .$ ()) world
			# (selection,world) = ((session .# "getSelection") .$ ()) world
			| jsIsNull value
				# (_,world) = ((selection .# "clearSelection") .$ ()) world
				= world
			| otherwise
				# (_,world) = ((selection .# "setSelectionRange") .$ value) world
				= world
		Just "disabled"
			# (_,world)       = ((editor .# "setReadOnly") .$ value) world
			= world
		= world

	onChange editor me world
		# (noEvents,world)  = me .# "noEvents" .? world
		| (not (jsIsUndefined noEvents)) && jsValToBool` True noEvents
			= world
		# (value,world)  = (editor .# "getValue" .$ ()) world
		# (Just value) = jsValToString value
		# (taskId,world)  = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,toJSON ("lines",value))) world
		= world

	onCursorChange selection me world
		# (cursor,world)  = ((selection.# "getCursor") .$ ()) world
		# (row,world) = cursor .# "row" .? world
		# (column,world) = cursor .# "column" .? world
		# (Just row) = jsValToInt row
		# (Just column) = jsValToInt column
		# (taskId,world)  = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,toJSON ("cursor",row,column))) world
		= world

	onSelectionChange selection me world
		# (taskId,world)  = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# (empty,world)  = (selection .# "isEmpty" .$ ()) world
		| jsValToBool` True empty
			# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,toJSON ("selection",JSONNull))) world
			= world
		| otherwise
			# (range,world)  = ((selection.# "getRange") .$ ()) world
			# (start,world)  = range.# "start" .? world
			# (end,world)    = range.# "end" .? world
			# (srow,world)   = start .# "row" .? world
			# (scol,world)   = start .# "column" .? world
			# (erow,world)   = end .# "row" .? world
			# (ecol,world)   = end .# "column" .? world
			# (Just srow)    = jsValToInt srow
			# (Just scol)    = jsValToInt scol
			# (Just erow)    = jsValToInt erow
			# (Just ecol)    = jsValToInt ecol
			# (_,world)      = ((me .# "doEditEvent") .$ (taskId,editorId,toJSON ("selection",(srow,scol),(erow,ecol)))) world
			= world

	onEdit dp ([], [JSONString "lines", JSONString text]) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & lines = split "\n" text})), vst)
	onEdit dp ([], [JSONString "cursor",JSONInt row,JSONInt col]) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & cursor = (row,col)})),vst)
	onEdit dp ([], [JSONString "selection",JSONNull]) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & selection = Nothing})), vst)
	onEdit dp ([],[JSONString "selection",JSONArray [JSONInt srow,JSONInt scol],JSONArray [JSONInt erow,JSONInt ecol]]) (o,s) vst
		# selection = {AceRange|start=(srow,scol),end=(erow,ecol)}
		= (Ok (NoChange, (o,{AceState|s & selection = Just selection})), vst)
	onEdit _ (_, e) _ vst = (Error $ "Invalid event for Ace editor: " +++ toString (toJSON e), vst)

	onRefresh dp r=:(_,rs) v=:(_,vs) vst
		//Check if nothing changed
		| r === v = (Ok (NoChange, r),vst)
		//Determine which parts changed
		# lineChange = if (rs.AceState.lines === vs.AceState.lines)
						[] [SetAttribute "lines" (JSONArray (map JSONString rs.AceState.lines))]
		//Cursor change
		# cursorChange = if (rs.AceState.cursor === vs.AceState.cursor) 
						[] [SetAttribute "cursor" (toJSON rs.AceState.cursor)]
		//Selection change
		# selectionChange = if (rs.AceState.selection === vs.AceState.selection) 
						[] [SetAttribute "selection" (maybe JSONNull encodeRange rs.AceState.selection)]
		//Disabled change
		# disabledChange = if (rs.AceState.disabled == vs.AceState.disabled) 
						[] [SetAttribute "disabled" (JSONBool rs.AceState.disabled)]
		= (Ok (ChangeUI (flatten [lineChange,cursorChange,selectionChange,disabledChange]) [], r),vst)

	encodeRange {AceRange|start=(srow,scol),end=(erow,ecol)}
		= JSONObject [("start",JSONObject [("row",JSONInt srow),("column",JSONInt scol)])
					 ,("end",JSONObject [("row",JSONInt erow),("column",JSONInt ecol)])]

	valueFromState st = Just st
