definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from ABC.Interpreter      import :: PrelinkedInterpretationEnvironment
from iTasks.UI.Definition import :: UI, :: UIAttributes, :: UIChange, :: UIAttributeChange, :: TaskId
from iTasks.UI.JavaScript import :: JSWorld, :: JSVal

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Defaults import generic gDefault
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Error import :: MaybeError, :: MaybeErrorString
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq
from StdOverloaded import class toString
from Control.GenBimap import generic bimap, :: Bimap

/**
* Definition of an editor.
* EditMode and Maybe arguments/results are unique to allow deriving bimap for Editor type.
*/
:: Editor a =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath *(EditMode a) *VSt ->
		*(!MaybeErrorString (!UI, !EditState), !*VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) EditState *VSt ->
		*(!MaybeErrorString (!UIChange, !EditState), !*VSt)
	//React to a new model value
	, onRefresh :: !DataPath a EditState *VSt ->
		*(!MaybeErrorString (!UIChange, !EditState), !*VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !EditState -> *Maybe a
	}

/*
*	Definition of a leaf editor using a typed state and edit event.
*	This is an auxiliary type to define an `Editor` with an untyped state and edit events.
*/
:: LeafEditor edit st a =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode a) *VSt ->
		*(!MaybeErrorString (!UI, !st), !*VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !edit) st *VSt ->
		*(!MaybeErrorString (!UIChange, !st), !*VSt)
	//React to a new model value
	, onRefresh :: !DataPath a st *VSt ->
		*(!MaybeErrorString (!UIChange, !st), !*VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st -> Maybe a
	}

leafEditorToEditor :: !(LeafEditor edit st a) -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} st & JSONDecode{|*|} edit

//Version without overloading, for use in generic case
//The first two argument should be JSONEncode{|*|} and JSONDecode{|*|} which cannot be used by overloading within generic functions
leafEditorToEditor_ :: !(Bool st -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe st, ![JSONNode])) !(LeafEditor edit st a)
                    -> Editor a | JSONDecode{|*|} edit

/*
*	Definition of a compound editor using an additional typed state, next to the children's states.
*	This is an auxiliary type to define an `Editor` with an untyped state.
*	The function work on the typed additional state and the untyped children's states.
*/
:: CompoundEditor st a =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode a) *VSt ->
		*(!MaybeErrorString (!UI, !st, ![EditState]), !*VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) st [EditState] *VSt ->
		*(!MaybeErrorString (!UIChange, !st, ![EditState]), !*VSt)
	//React to a new model value
	, onRefresh :: !DataPath a st [EditState] *VSt ->
		*(!MaybeErrorString (!UIChange, !st, ![EditState]), !*VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st [EditState] -> Maybe a
	}

compoundEditorToEditor :: !(CompoundEditor st a) -> Editor a | JSONDecode{|*|}, JSONEncode{|*|} st

/*
*	Definition of an editor modifier using an additional typed state, next to the child state.
*	Modifiers without additional state can be directly defined in terms of the `Editor` type.
*	This is an auxiliary type to define an `Editor` with an untyped state.
*	The function work on the typed additional state and the untyped child state.
*/
:: EditorModifierWithState st a =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode a) *VSt ->
		*(!MaybeErrorString (!UI, !st, !EditState), !*VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) st EditState *VSt ->
		*(!MaybeErrorString (!UIChange, !st, !EditState), !*VSt)
	//React to a new model value
	, onRefresh :: !DataPath a st EditState *VSt ->
		*(!MaybeErrorString (!UIChange, !st, !EditState), !*VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st EditState -> Maybe a
	}

editorModifierWithStateToEditor :: !(EditorModifierWithState st a) -> Editor a | JSONDecode{|*|}, JSONEncode{|*|} st

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

:: EditMode a = Enter | Update !a | View !a

editModeValue :: !(EditMode a) -> Maybe a
mapEditMode :: .(.x -> .y) !(EditMode .x) -> EditMode .y
derive bimap EditMode

/** Edit masks contain information about a value as it is being edited in an interactive task.
*   During editing, values can be in an inconsistent, or even untypable state
*/  
:: EditState
	= LeafState      !LeafState             //* Edit state of single fields/controls
	| CompoundState  !JSONNode ![EditState] //* Compound structure of multiple editors with additional extra state
	| AnnotatedState !JSONNode !EditState   //* Edit state annotated with additional information, used for modifiers

:: LeafState =
	{ touched :: !Bool
	, state   :: !JSONNode //Usually contains the (serialized) value
	}

:: *VSt =
	{ taskId			:: !String   //* The id of the task the visualisation belongs to
	, optional			:: !Bool     //* Create optional form fields
	, selectedConsIndex	:: !Int      //* Index of the selected constructor in an OBJECT
	, pathInEditMode    :: [Bool]    //* Path of LEFT/RIGHT choices used when UI is generated in edit mode
	, abcInterpreterEnv :: !PrelinkedInterpretationEnvironment //* Used to serialize expressions for the client
	}

withVSt :: !TaskId !.(*VSt -> (!a, !*VSt)) !*IWorld -> (!a, !*IWorld)

derive JSONEncode EditState, LeafState, EditMode
derive JSONDecode EditState, LeafState, EditMode
derive gEq        EditState, LeafState

newLeafState :: EditState

//Generate the editorId string for a given datapath
editorId :: !DataPath -> String
s2dp     :: !String -> DataPath

isTouched :: !EditState -> Bool
isCompound :: !EditState -> Bool

//Add client-side initialization to the generation of an initial UI
withClientSideInit ::
	!(JSVal *JSWorld -> *JSWorld)
	!(UIAttributes DataPath a *VSt -> *(!MaybeErrorString (!UI, !st), !*VSt))
	!UIAttributes !DataPath !a !*VSt ->
		*(!MaybeErrorString (!UI, !st), !*VSt)

