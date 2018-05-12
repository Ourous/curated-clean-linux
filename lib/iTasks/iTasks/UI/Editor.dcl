definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from iTasks.UI.Definition import :: UI, :: UIAttributes, :: UIChange, :: UIAttributeChange
from iTasks.UI.JS.Interface import :: JSWorld, :: JSObj, :: JSVal, :: JSObject

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Defaults import generic gDefault
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Error import :: MaybeError, :: MaybeErrorString
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq
from StdOverloaded import class toString

/*
*	Definition of an editor editor
*/
:: Editor a = 
	{ genUI     :: DataPath                     a          *VSt -> *(!MaybeErrorString (!UI, !EditMask),           !*VSt) //Generating the initial UI
	, onEdit    :: DataPath (DataPath,JSONNode) a EditMask *VSt -> *(!MaybeErrorString (!UIChange, !EditMask), !a, !*VSt) //React to edit events
	, onRefresh :: DataPath a                   a EditMask *VSt -> *(!MaybeErrorString (!UIChange, !EditMask), !a, !*VSt) //React to a new model value 
	}

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

:: EditMode = Enter | Update | View

/** Edit masks contain information about a value as it is being edited in an interactive task.
*   During editing, values can be in an inconsistent, or even untypable state
*/  
:: EditMask
	= FieldMask !FieldMask                    //Edit state of single fields/controls
	| CompoundMask ![EditMask]                //Compound structure of multiple controls
	| StateMask !EditMask !JSONNode           //Add arbitrary editing state to an edit mask

:: FieldMask = 
	{ touched :: !Bool
	, valid   :: !Bool
	, state   :: !JSONNode //Usually contains the (serialized) value
	}

:: Masked a :== (a,EditMask)

:: *VSt =
	{ taskId			:: !String           // The id of the task the visualisation belongs to
	, mode              :: !EditMode         // If we are entering, updating or viewing the data
	, optional			:: !Bool             // Create optional form fields
	, selectedConsIndex	:: !Int              // Index of the selected constructor in an OBJECT
	, iworld			:: !*IWorld	         // The iworld, used for example if external tools are needed to create editors
	}

derive JSONEncode EditMask, FieldMask
derive JSONDecode EditMask, FieldMask
derive gEq        EditMask, FieldMask

instance toString EditMode

newFieldMask :: EditMask
newCompoundMask :: EditMask

//Generate the editorId string for a given datapath
editorId 				:: !DataPath 		-> String
s2dp					:: !String			-> DataPath

subMasks	:: !Int EditMask -> [EditMask]
isTouched	:: !EditMask -> Bool

containsInvalidFields :: !EditMask -> Bool

//Add client-side initialization to the generation of an initial UI
withClientSideInit ::
	((JSObj ()) *JSWorld -> *JSWorld)
	(DataPath a *VSt -> *(!MaybeErrorString (!UI, !EditMask), !*VSt))
	DataPath a *VSt -> *(!MaybeErrorString (!UI, !EditMask), !*VSt)

