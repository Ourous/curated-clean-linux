implementation module iTasks.Extensions.Editors.Ace

import iTasks
import iTasks.UI.Editor, iTasks.UI.Editor.Modifiers, iTasks.UI.Definition
import iTasks.UI.JavaScript
import qualified Data.Map as DM
import Data.Func, StdArray

ACE_JS_URL :== "/ace/src-noconflict/ace.js"
ACE_DEFAULT_THEME :== "ace/theme/chrome"
ACE_DEFAULT_MODE  :== "ace/mode/text"

derive class iTask AceState, AceRange
derive gDefault AceState, AceRange

derive gEditor AceOptions
derive gEq AceOptions
derive gText AceOptions
gDefault{|AceOptions|} = {AceOptions|theme = ACE_DEFAULT_THEME, mode = ACE_DEFAULT_MODE}

derive JSONEncode AceOptions
derive JSONDecode AceOptions

:: EditEvent
	= EditValue !String
	| EditCursor !Int !Int
	| EditSelection !(Maybe ((Int,Int), (Int,Int)))

derive JSONEncode EditEvent
derive JSONDecode EditEvent

aceTextArea :: Editor String
aceTextArea = surjectEditorValue toAce fromAce aceEditor
where
	aceState = {AceState|value="",cursor=(0,0),selection=Nothing,disabled=False}
	toAce s Nothing = (defaultValue, {AceState|aceState & value=s})
	toAce s (Just (opts,state)) = (opts, {AceState|state & value=s})
	fromAce (_,{AceState|value}) _ = value

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
			[("value",JSONString state.AceState.value)
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
		# world             = (me .# "noEvents" .= False) world // Flag that no events should be sent because we just received the latest value
		//Create Ace editor linked to domEl
		# domEl             = me .# "domEl"
		# world             = (domEl .# "style.width" .= "100%") world
		# world             = (domEl .# "style.height" .= "100%") world
		# world             = (domEl .# "style.margin" .= 0) world
		# (editor,world)    = (jsGlobal "ace.edit" .$ domEl) world
		# world             = (editor .# "setAutoScrollEditorIntoView" .$! True) world // fit window after resize
		# (session,world)   = (editor .# "getSession" .$ ()) world
		# (selection,world) = (session.# "getSelection" .$ ()) world
		# world             = (me .# "editor" .= editor) world
		//Set options
		# world             = (editor .# "setReadOnly" .$! me .# "attributes.disabled") world
		# world             = (editor .# "setTheme" .$! me .# "attributes.theme") world
		# world             = (session .# "setMode" .$! me .# "attributes.mode") world
		//Initialize state based on attributes
		# world             = (editor .# "setValue" .$! me .# "attributes.value") world
		//Set initial cursor position
		# world             = (editor .# "navigateTo" .$! (me .# "attributes.cursor[0]", me .# "attributes.cursor[1]")) world
		//Potentially set initial selection
		# (selattr,world)     = me .# "attributes.selection" .? world
		# world = if (jsIsNull selattr) world ((selection .# "setSelectionRange" .$! me .# "attributes.value") world)
		//Add event listeners
		# (cb,world)     = jsWrapFun (\_ -> onChange editor me) me world
		# world          = (editor .# "on" .$! ("change",cb)) world
		# (cb,world)     = jsWrapFun (\_ -> onCursorChange selection me) me world
		# world          = (selection .# "on" .$! ("changeCursor",cb)) world
		# (cb,world)     = jsWrapFun (\_ -> onSelectionChange selection me) me world
		# world          = (selection .# "on" .$! ("changeSelection",cb)) world
		= world

	onAttributeChange me {[0]=name,[1]=value} world
	# editor = me .# "editor"
	= case jsValToString name of
		Just "value"
			# world           = (me .# "noEvents" .= True) world
			# world           = (editor .# "setValue" .$! (value,1)) world
			# world           = (me .# "noEvents" .= False) world
			= world
		Just "cursor"
			# world           = (me .# "noEvents" .= True) world
			# world           = (editor .# "navigateTo" .$! (value .# 0, value .# 1)) world
			# world           = (me .# "noEvents" .= False) world
			= world
		Just "selection"
			# world           = (me .# "noEvents" .= True) world
			# (session,world)   = (editor .# "getSession" .$ ()) world
			# (selection,world) = (session .# "getSelection" .$ ()) world
			# world = if (jsIsNull value)
				(selection .# "clearSelection" .$! ())
				(selection .# "setSelectionRange" .$! value)
				world
			# world           = (me .# "noEvents" .= False) world
			= world
		Just "disabled"
			= (editor .# "setReadOnly" .$! value) world
		= world

	onChange editor me world
		# (noEvents,world)  = me .# "noEvents" .? world
		| jsValToBool` True noEvents
			= world
		# (value,world)  = (editor .# "getValue" .$ ()) world
		# (Just value) = jsValToString value
		# world = (me .# "doEditEvent" .$!
			( me .# "attributes.taskId"
			, me .# "attributes.editorId"
			, toJSON (EditValue value)
			)) world
		= world

	onCursorChange selection me world
		# (noEvents,world)  = me .# "noEvents" .? world
		| jsValToBool` True noEvents
			= world
		# (cursor,world)  = (selection .# "getCursor" .$ ()) world
		# (row,world) = cursor .# "row" .? world
		# (column,world) = cursor .# "column" .? world
		# (Just row) = jsValToInt row
		# (Just column) = jsValToInt column
		# (_,world) = (me .# "doEditEvent" .$
			( me .# "attributes.taskId"
			, me .# "attributes.editorId"
			, toJSON (EditCursor row column)
			)) world
		= world

	onSelectionChange selection me world
		# (noEvents,world)  = me .# "noEvents" .? world
		| jsValToBool` True noEvents
			= world
		# (empty,world)  = (selection .# "isEmpty" .$ ()) world
		| jsValToBool` True empty
			= (me .# "doEditEvent" .$!
				( me .# "attributes.taskId"
				, me .# "attributes.editorId"
				, toJSON (EditSelection Nothing)
				)) world
		# (range,world)  = (selection.# "getRange" .$ ()) world
		# (srow,world)   = range .# "start.row" .? world
		# (scol,world)   = range .# "start.column" .? world
		# (erow,world)   = range .# "end.row" .? world
		# (ecol,world)   = range .# "end.column" .? world
		# (Just srow)    = jsValToInt srow
		# (Just scol)    = jsValToInt scol
		# (Just erow)    = jsValToInt erow
		# (Just ecol)    = jsValToInt ecol
		= (me .# "doEditEvent" .$!
			( me .# "attributes.taskId"
			, me .# "attributes.editorId"
			, toJSON (EditSelection (Just ((srow,scol), (erow,ecol))))
			)) world

	onEdit dp ([], EditValue text) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & value = text})), vst)
	onEdit dp ([], EditCursor row col) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & cursor = (row,col)})),vst)
	onEdit dp ([], EditSelection Nothing) (o, s) vst
		= (Ok (NoChange, (o,{AceState|s & selection = Nothing})), vst)
	onEdit dp ([], EditSelection (Just ((srow,scol),(erow,ecol)))) (o,s) vst
		# selection = {AceRange|start=(srow,scol),end=(erow,ecol)}
		= (Ok (NoChange, (o,{AceState|s & selection = Just selection})), vst)
	onEdit _ (_, _) _ vst = (Error $ "Invalid event for Ace editor", vst)

	onRefresh dp r=:(_,rs) (_,vs) vst
		// Determine which parts changed
		# lineChange = if (rs.AceState.value === vs.AceState.value)
						[] [SetAttribute "value" (JSONString rs.AceState.value)]
		# cursorChange = if (rs.AceState.cursor === vs.AceState.cursor) 
						[] [SetAttribute "cursor" (toJSON rs.AceState.cursor)]
		# selectionChange = if (rs.AceState.selection === vs.AceState.selection) 
						[] [SetAttribute "selection" (maybe JSONNull encodeRange rs.AceState.selection)]
		# disabledChange = if (rs.AceState.disabled == vs.AceState.disabled) 
						[] [SetAttribute "disabled" (JSONBool rs.AceState.disabled)]
		// Build change event
		# changes = flatten [lineChange,cursorChange,selectionChange,disabledChange]
		# change = if (isEmpty changes) NoChange (ChangeUI changes [])
		= (Ok (change, r),vst)

	encodeRange {AceRange|start=(srow,scol),end=(erow,ecol)}
		= JSONObject [("start",JSONObject [("row",JSONInt srow),("column",JSONInt scol)])
					 ,("end",JSONObject [("row",JSONInt erow),("column",JSONInt ecol)])]

	valueFromState st = Just st
