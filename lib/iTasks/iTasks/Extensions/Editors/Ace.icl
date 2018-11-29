implementation module iTasks.Extensions.Editors.Ace

import iTasks
import iTasks.UI.Editor, iTasks.UI.Editor.Modifiers, iTasks.UI.Definition
import iTasks.UI.JS.Interface, iTasks.UI.JS.Encoding
import qualified Data.Map as DM
import Text, Data.Func

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
	genUI dp mode vst=:{VSt|taskId,optional}
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
    	# attr = 'DM'.unions [aceAttr, optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp)]
		= (Ok (uia UIComponent attr, (options, state)),vst)

	initUI me world
		//Setup UI component
		# world      = ((me .# "domTag") .= toJSVal "pre") world
		# (cb,world) = jsWrapFun (\a w -> (jsNull,onAttributeChange me a w)) world
		# world      = ((me .# "onAttributeChange") .= cb) world
		//Load Ace javascript
		# (cb,world) = jsWrapFun (\_ w -> (jsNull,initUI` me w)) world
		# world      = addJSFromUrl ACE_JS_URL (Just cb) world
		= world

	initUI` me world
		//Create Ace editor linked to domEl
		# (domEl,world)     = .? (me .# "domEl") world
		# (editor,world)    = jsNewObject "ace.edit" [toJSArg domEl] world
        # (session,world)   = ((editor .# "getSession") .$ ()) world
        # (selection,world) = ((session.# "getSelection") .$ ()) world
		# world             = ((me .# "editor") .= editor) world
		//Set options
		# (readOnly,world)  = .? (me .# "attributes.disabled") world
        # (_,world)         = ((editor .# "setReadOnly") .$ readOnly) world
		# (theme,world)     = .? (me .# "attributes.theme") world
        # (_,world)         = ((editor .# "setTheme") .$ theme) world
		# (mode,world)      = .? (me .# "attributes.mode") world
        # (_,world)         = ((session .# "setMode") .$ mode) world
		//Initialize state based on attributes
		# (lines,world)     = .? (me .# "attributes.lines") world
        # (value,world)     = ((lines.# "join") .$ "\n") world
        # (_,world)         = ((editor .# "setValue") .$ value) world
		//Set initial cursor position
		# (cursor,world)    = .? (me .# "attributes.cursor") world
		# (row,world)       = .? (cursor .# 0) world
		# (col,world)       = .? (cursor .# 1) world
		# (_,world)       	= ((editor .# "navigateTo") .$ (row,col)) world
		//Potentially set initial selection
		# (selattr,world)     = .? (me .# "attributes.selection") world
		# world = if (jsIsNull (toJSVal selattr)) world (snd (((selection .# "setSelectionRange") .$ value) world))
		//Add event listeners
		# (cb,world)     = jsWrapFun (\a w -> (jsNull,onChange editor me w)) world
		# (_,world)      = ((editor .# "on") .$ ("change",cb)) world
		# (cb,world)     = jsWrapFun (\a w -> (jsNull,onCursorChange selection me w)) world
		# (_,world)      = ((selection .# "on") .$ ("changeCursor",cb)) world
		# (cb,world)     = jsWrapFun (\a w -> (jsNull,onSelectionChange selection me w)) world
		# (_,world)      = ((selection .# "on") .$ ("changeSelection",cb)) world
		= world

	onAttributeChange me [name,value] world
		# (editor,world)  = .? (me .# "editor") world
		| jsArgToString name == "lines" 
			# world           = ((me .# "noEvents") .= True) world //Flag that no events should be sent because we just received the latest value
        	# (value,world)   = (((toJSVal value) .# "join") .$ "\n") world
        	# (_,world)       = ((editor .# "setValue") .$ (value,1)) world
			# world           = ((me .# "noEvents") .= False) world
			= world
		| jsArgToString name == "cursor" 
			# (row,world)    = .? ((toJSVal value) .# 0) world
			# (col,world)    = .? ((toJSVal value) .# 1) world
        	# (_,world)       = ((editor .# "navigateTo") .$ (row,col)) world
			= world
		| jsArgToString name == "selection" 
			# (editor,world)  = .? (me .# "editor") world
        	# (session,world)   = ((editor .# "getSession") .$ ()) world
        	# (selection,world) = ((session .# "getSelection") .$ ()) world
			| jsIsNull (toJSVal value)
        		# (_,world) = ((selection .# "clearSelection") .$ ()) world
				= world 
			| otherwise
        		# (_,world) = ((selection .# "setSelectionRange") .$ value) world
				= world
		| jsArgToString name == "disabled" 
        	# (_,world)       = ((editor .# "setReadOnly") .$ value) world
			= world
		= world

	onChange editor me world
		# (noEvents,world)  = .? (me .# "noEvents") world
		| (not (jsIsUndefined noEvents)) && jsValToBool noEvents
			= world
        # (value,world)  = ((editor .# "getValue") .$ ()) world
		# (taskId,world)  = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,("lines",value))) world
		= world

	onCursorChange selection me world
        # (cursor,world)  = ((selection.# "getCursor") .$ ()) world
		# (row,world) = .? (cursor .# "row") world
		# (column,world) = .? (cursor .# "column") world
		# (taskId,world)  = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,("cursor",row,column))) world
		= world

	onSelectionChange selection me world
		# (taskId,world)  = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (empty,world)  = ((selection .# "isEmpty") .$ ()) world
		| jsValToBool empty
			# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,("selection",JSONNull))) world
			= world
		| otherwise
        	# (range,world)  = ((selection.# "getRange") .$ ()) world
        	# (start,world)  = .? (range.# "start") world
        	# (end,world)    = .? (range.# "end") world
			# (srow,world)   = .? (start .# "row") world
			# (scol,world)   = .? (start .# "column") world
			# (erow,world)   = .? (end .# "row") world
			# (ecol,world)   = .? (end .# "column") world
			# (_,world)      = ((me .# "doEditEvent") .$ (taskId,editorId,("selection",(srow,scol),(erow,ecol)))) world
			= world

	onEdit dp ([], [JSONString "lines", JSONString text]) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & lines = split "\n" text})), vst)
	onEdit dp ([], [JSONString "cursor",JSONInt row,JSONInt col]) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & cursor = (row,col)})),vst)
	onEdit dp ([], [JSONString "selection",JSONArray [JSONString "JSONNull"]]) (o, s) vst
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
