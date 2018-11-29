implementation module iTasks.UI.Editor

import StdBool, StdMisc, StdList, StdTuple
import iTasks.Internal.Client.LinkerSupport, Data.Maybe, Data.Functor, Data.Tuple, Data.Func, Data.Error
import iTasks.Internal.IWorld
import iTasks.UI.Definition, iTasks.WF.Definition, iTasks.UI.JS.Encoding
import qualified Data.Map as DM
import Text, Text.GenJSON
import Data.GenEq

derive JSONEncode EditState, LeafState, EditMode
derive JSONDecode EditState, LeafState, EditMode
derive gEq        EditState, LeafState

leafEditorToEditor :: !(LeafEditor edit st a) -> Editor a | JSDecode{|*|} edit & JSONEncode{|*|}, JSONDecode{|*|} st
leafEditorToEditor leafEditor = leafEditorToEditor_ JSONEncode{|*|} JSONDecode{|*|} leafEditor

leafEditorToEditor_ :: !(Bool st -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe st, ![JSONNode])) !(LeafEditor edit st a)
                    -> Editor a | JSDecode{|*|} edit
leafEditorToEditor_ jsonEncode jsonDecode leafEditor =
	{Editor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI dp val vst = mapRes False $ leafEditor.LeafEditor.genUI dp val vst

	onEdit dp (tp, jsone) (LeafState {state}) vst = case fromJSON` state of
		Just st = case decodeOnServer jsone of
			Just e = mapRes True $ leafEditor.LeafEditor.onEdit dp (tp, e) st vst
			_      = (Error ("Invalid edit event for leaf editor: " +++ toString jsone), vst)
		_       = (Error "Corrupt internal state in leaf editor", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in leaf editor", vst)

	onRefresh dp val (LeafState leafSt) vst = case fromJSON` leafSt.state of
		Just st = mapRes leafSt.touched $ leafEditor.LeafEditor.onRefresh dp val st vst
		_       = (Error "Corrupt internal state in leaf editor", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in leaf editor", vst)

	valueFromState (LeafState {state}) = case fromJSON` state of
		Just st = case leafEditor.LeafEditor.valueFromState st of
			Just val = Just val
			_        = Nothing
		_       = Nothing
	valueFromState _ = Nothing

	mapRes touched (mbRes, vst) = ((\(ui, st) -> (ui, LeafState {touched = touched, state = toJSON` st})) <$> mbRes, vst)

	toJSON` x = case (jsonEncode False x) of
		[node] = node
		_      = JSONError
	fromJSON` node = fst (jsonDecode False [node])

compoundEditorToEditor :: !(CompoundEditor st a) -> Editor a | JSONDecode{|*|}, JSONEncode{|*|} st
compoundEditorToEditor compoundEditor =
	{Editor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI dp val vst = mapRes $ compoundEditor.CompoundEditor.genUI dp val vst

	onEdit dp e (CompoundState jsonSt childSts) vst = case fromJSON jsonSt of
		Just st = mapRes $ compoundEditor.CompoundEditor.onEdit dp e st childSts vst
		_       = (Error "Corrupt internal state in compound editor", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in compound editor", vst)

	onRefresh dp val (CompoundState jsonSt childSts) vst = case fromJSON jsonSt of
		Just st = mapRes $ compoundEditor.CompoundEditor.onRefresh dp val st childSts vst
		_       = (Error "Corrupt internal state in compound editor", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in compound", vst)

	valueFromState (CompoundState jsonSt childSts) = case fromJSON jsonSt of
		Just st = case compoundEditor.CompoundEditor.valueFromState st childSts of
			Just val = Just val
			_        = Nothing
		_ = Nothing
	valueFromState _ = Nothing

	mapRes :: !(!MaybeErrorString (!ui, !st, ![EditState]), !*VSt) -> (!MaybeErrorString (!ui, !EditState), !*VSt)
	        | JSONEncode{|*|} st
	mapRes (mbRes, vst) = ((\(ui, st, childSts) -> (ui, CompoundState (toJSON st) childSts)) <$> mbRes, vst)

editorModifierWithStateToEditor :: !(EditorModifierWithState st a) -> Editor a | JSONDecode{|*|}, JSONEncode{|*|} st
editorModifierWithStateToEditor modifier =
	{Editor| genUI = genUI, onEdit = onEdit, onRefresh = onRefresh, valueFromState = valueFromState}
where
	genUI dp val vst = mapRes $ modifier.EditorModifierWithState.genUI dp val vst

	onEdit dp e (AnnotatedState jsonSt childSt) vst = case fromJSON jsonSt of
		Just st = mapRes $ modifier.EditorModifierWithState.onEdit dp e st childSt vst
		_       = (Error "Corrupt internal state in editor modifier", vst)
	onEdit _ _ _ vst = (Error "Corrupt editor state in editor modifier", vst)

	onRefresh dp val (AnnotatedState jsonSt childSt) vst = case fromJSON jsonSt of
		Just st = mapRes $ modifier.EditorModifierWithState.onRefresh dp val st childSt vst
		_       = (Error "Corrupt internal state in editor modifier", vst)
	onRefresh _ _ _ vst = (Error "Corrupt editor state in editor modifier", vst)

	valueFromState (AnnotatedState jsonSt childSt) = case fromJSON jsonSt of
		Just st = case modifier.EditorModifierWithState.valueFromState st childSt of
			Just val = Just val
			_        = Nothing
		_ = Nothing
	valueFromState _ = Nothing

	mapRes :: !(!MaybeErrorString (!ui, !st, !EditState), !*VSt) -> (!MaybeErrorString (!ui, !EditState), !*VSt)
	        | JSONEncode{|*|} st
	mapRes (mbRes, vst) = ((\(ui, st, childSt) -> (ui, AnnotatedState (toJSON st) childSt)) <$> mbRes, vst)

editModeValue :: !(EditMode a) -> Maybe a
editModeValue Enter        = Nothing
editModeValue (Update val) = Just val
editModeValue (View   val) = Just val

mapEditMode :: .(.x -> .y) !(EditMode .x) -> EditMode .y
mapEditMode _ Enter      = Enter
mapEditMode f (Update x) = Update $ f x
mapEditMode f (View x)   = View   $ f x

derive bimap EditMode

withVSt :: !TaskId !.(*VSt -> (!a, !*VSt)) !*IWorld -> (!a, !*IWorld)
withVSt taskId f iworld
	# (x, vst) = f { VSt
	               | taskId            = toString taskId
	               , optional          = False
	               , selectedConsIndex = -1
	               , pathInEditMode    = abort "VSt.dataPathInEditMode should be set by OBJECT instance of gEditor"
	               , iworld            = iworld
	               }
	= (x, vst.iworld)

newLeafState :: EditState
newLeafState = LeafState {LeafState|touched=False,state=JSONNull}

editorId :: !DataPath -> String
editorId dp = "v" + join "-" (map toString dp)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

isTouched :: !EditState -> Bool
isTouched (LeafState      {LeafState|touched}) = touched
isTouched (CompoundState  _ childSts)          = or (map isTouched childSts)
isTouched (AnnotatedState _ childSt)           = isTouched childSt

isCompound :: !EditState -> Bool
isCompound (LeafState _)              = False
isCompound (AnnotatedState _ childSt) = isCompound childSt
isCompound (CompoundState _ _)        = True

withClientSideInit ::
	((JSObj ()) *JSWorld -> *JSWorld)
	(DataPath a *VSt -> *(!MaybeErrorString (!UI, !st), !*VSt))
	DataPath a *VSt -> *(!MaybeErrorString (!UI, !st), !*VSt)
withClientSideInit initUI genUI dp val vst=:{VSt|taskId} = case genUI dp val vst of
    (Ok (UI type attr items,mask),vst=:{VSt|iworld}) = case editorLinker initUI iworld of
        (Ok (saplDeps, saplInit),iworld)
			# extraAttr = 'DM'.fromList [("taskId",  JSONString taskId)
                                        ,("editorId",JSONString (editorId dp))
                                        ,("saplDeps",JSONString saplDeps)
                                        ,("saplInit",JSONString saplInit)
                                        ]
            = (Ok (UI type ('DM'.union extraAttr attr) items,mask), {VSt|vst & iworld = iworld})
        (Error e,iworld)
            = (Error e, {VSt|vst & iworld = iworld})
    (Error e,vst) = (Error e,vst)

