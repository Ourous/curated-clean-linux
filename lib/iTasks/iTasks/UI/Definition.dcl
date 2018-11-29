definition module iTasks.UI.Definition
/**
* This module provides an abstract representation of user interfaces.
*
* It is the interface between UI's as specified by tasks and the Web-based UI framework that
* renders the task UI's in a web browser.
* 
* This representation seeks a middle ground between being fine grained enough
* to describe rich user interfaces and being abstract enough to leave rendering details to the client framework.
*/
from Text.GenJSON import :: JSONNode
from Data.Maybe import :: Maybe
from Data.Functor import class Functor
from iTasks.Internal.Task	import :: TaskId
from Text.HTML			import :: HtmlTag
from Data.Map			import :: Map
from iTasks.WF.Combinators.Core import :: Action

from iTasks.WF.Definition import class iTask
from iTasks.Internal.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks.Internal.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditState

from iTasks.UI.Editor.Generic import generic gEditor
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq

//Provide generic instances for all UI definitions
derive class iTask UI, UIType
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UITreeNode

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChange
	= NoChange		                                       //No changes are needed
	| ReplaceUI !UI                                        //Replace the entire UI with a new version
	| ChangeUI [UIAttributeChange] [(!Int,!UIChildChange)] //Change the current UI and/or its children

:: UIAttributeChange = SetAttribute !String !JSONNode  //A change to a user interface attribute
					 | DelAttribute !String            //Remove an attribute
:: UIChildChange 	 = ChangeChild !UIChange           //Select a sub-component and apply the change definition there
					 | RemoveChild                     //Remove the child at the given index (next children 'move down')
					 | InsertChild !UI                 //Insert a new child at the given index (next children 'move up')
                     | MoveChild !Int                  //Move an existing child a given index to a new index
                                                       //(i,MoveChild j) == [(i,RemoveChild),(j,InsertChild _)]

derive class iTask UIChange, UIAttributeChange, UIChildChange

/**
* Rendering a user interface for a composition of is a staged process in which
* the raw UI material provided by basic tasks is grouped by layout policies to reach
* a final UI definition consisting of a set of controls and a window title for the top-level application window.
*
* The UI type has contstructors for the various types of partial UI definitions.
*/
:: UI = UI !UIType !UIAttributes ![UI]

:: UIType
	// --- Intermediate nodes: (implemented in itasks-components-raw.js) ---
    = UIEmpty
	| UIAction 
	| UIPair
	| UIRecord
	| UICons
	| UIVarCons
	| UIInteract
	| UIStep
	| UIParallel
	// --- Client components: ---
	// Core framework components (implemented in itasks-core.js)
	| UIComponent                                                                   // - Component (the client-side base class)
    | UIViewport                                                                    // - Viewport for embedding another task instance's UI (like an iframe for tasks)
    | UILoader                                                                      // - Viewport for embedding another task instance's UI (like an iframe for tasks)
	// Form components (implemented in itasks-components-form.js):
	| UITextField                                                                   // - Textfield (single line text field)
	| UITextArea                                                                    // - Textarea (multi-line text field)
	| UIPasswordField                                                               // - Password (single line text field that hides the text)
	| UIIntegerField                                                                // - Integerfield (integer number field)
	| UIDecimalField	                                                            // - Decimalfield (decimal number field)
	| UIDocumentField                                                               // - Document (info + upload possibility)
	| UICheckbox                                                                    // - Checkbox (editable checkbox)
	| UISlider                                                                      // - Slider (editable slider)
	| UIButton                                                                      // - Button that sends edit events on click
	| UILabel                                                                       // - Label (non-wrapping text label, clicks focus next component)
	| UIIcon                                                                        // - Icon (information icon with tooltip text)
	// Display components (implemented in itasks-components-display.js)
	| UITextView                                                                    // - String (non-wrapping single line text with automatic escaping)
	| UIHtmlView                                                                    // - Html (formatted multi line text)
	| UIProgressBar                                                                 // - Progress (non editable progress bar)
	// Selection components (implemented in itasks-components-selection.js)
	| UIDropdown                                                                    // - Dropdown (choice from a list of alternatives)
	| UICheckGroup                                                                  // - A group of radio buttons or checkboxes (depends on multiple attribute)
	| UIChoiceList                                                                  // - A mutually exclusive set of radio buttons 
	| UIGrid                                                                        // - Grid (selecting an item in a table)
	| UITree                                                                        // - Tree (selecting a node in a tree structure)
	// Containers (implemented in itasks-components-container.js)
	| UIContainer
	| UIPanel
	| UITabSet
	| UIWindow
	| UIMenu
	| UIMenuSep
	| UIToolBar
	| UIButtonBar
	| UIList
	| UIListItem
	| UIDebug
	// Data elements (implemented in itasks-core.js)
	| UIData 

:: UIAttributes 		:== Map String JSONNode
:: UIAttribute          :== (!String,!JSONNode)
:: UIAttributeKey       :== String

// Floating window
:: UIWindowType
    = FloatingWindow        //Normal movable window
    | NotificationBubble    //Fixed position info

//Most components can be resized in two dimensions
:: UISize
	= ExactSize !Int
	| WrapSize
	| FlexSize

:: UIBound
	= ExactBound !Int
	| WrapBound
	
:: UIHAlign
	= AlignLeft
	| AlignCenter
	| AlignRight

:: UIVAlign
	= AlignTop
	| AlignMiddle
	| AlignBottom
	
:: UIDirection
	= Horizontal
	| Vertical
	
:: UISide
	= TopSide
	| RightSide
	| BottomSide
	| LeftSide

:: UISideSizes =
	{ top		:: !Int
	, right		:: !Int
	, bottom	:: !Int
	, left		:: !Int
	}	

:: UITreeNode =
	{ text		:: !String
    , iconCls   :: !Maybe String
	, children	:: !Maybe [UITreeNode]
	, leaf		:: !Bool
	, expanded	:: !Bool
	, value		:: !Int
	}

//Predefined attribute names
TITLE_ATTRIBUTE			:== "title"
HINT_ATTRIBUTE			:== "hint"
HINT_TYPE_ATTRIBUTE		:== "hint-type"
HINT_TYPE_INFO 			:== "info"
HINT_TYPE_VALID 		:== "valid"
HINT_TYPE_WARNING 		:== "warning"
HINT_TYPE_INVALID 		:== "invalid"
LABEL_ATTRIBUTE			:== "label"
PREFIX_ATTRIBUTE		:== "prefix"
POSTFIX_ATTRIBUTE		:== "postfix"
ICON_ATTRIBUTE			:== "icon"
STEPPED_ATTRIBUTE       :== "stepped"


//Construction functions
ui   :: UIType -> UI
uic  :: UIType [UI] -> UI
uia  :: UIType UIAttributes -> UI
uiac :: UIType UIAttributes [UI] -> UI

//Predefined attribute defintions
emptyAttr         :: UIAttributes

optionalAttr 	  :: !Bool                                -> UIAttributes
sizeAttr          :: !UISize !UISize                      -> UIAttributes
widthAttr         :: !UISize                              -> UIAttributes
heightAttr        :: !UISize                              -> UIAttributes
minSizeAttr       :: !UIBound !UIBound                    -> UIAttributes
minWidthAttr      :: !UIBound                             -> UIAttributes
minHeightAttr     :: !UIBound                             -> UIAttributes
maxSizeAttr       :: !UIBound !UIBound                    -> UIAttributes
maxWidthAttr      :: !UIBound                             -> UIAttributes
maxHeightAttr     :: !UIBound                             -> UIAttributes
marginsAttr       :: !Int !Int !Int !Int                  -> UIAttributes
topMarginAttr     :: !Int                                 -> UIAttributes
rightMarginAttr   :: !Int                                 -> UIAttributes
bottomMarginAttr  :: !Int                                 -> UIAttributes
leftMarginAttr    :: !Int                                 -> UIAttributes
paddingAttr       :: !Int !Int !Int !Int                  -> UIAttributes
topPaddingAttr    :: !Int                                 -> UIAttributes
rightPaddingAttr  :: !Int                                 -> UIAttributes
bottomPaddingAttr :: !Int                                 -> UIAttributes
leftPaddingAttr   :: !Int                                 -> UIAttributes
titleAttr         :: !String                              -> UIAttributes
frameAttr         :: !Bool                                -> UIAttributes
iconClsAttr       :: !String                              -> UIAttributes
tooltipAttr       :: !String                              -> UIAttributes
directionAttr     :: !UIDirection                         -> UIAttributes
halignAttr        :: !UIHAlign                            -> UIAttributes
valignAttr        :: !UIVAlign                            -> UIAttributes
hposAttr          :: !UIHAlign                            -> UIAttributes
vposAttr          :: !UIVAlign                            -> UIAttributes
windowTypeAttr    :: !UIWindowType                        -> UIAttributes
focusTaskIdAttr   :: !String                              -> UIAttributes
closeTaskIdAttr   :: !String                              -> UIAttributes
activeTabAttr     :: !Int                                 -> UIAttributes
valueAttr         :: !JSONNode                            -> UIAttributes
minAttr           :: !Int                                 -> UIAttributes
maxAttr           :: !Int                                 -> UIAttributes
textAttr          :: !String                              -> UIAttributes
enabledAttr       :: !Bool                                -> UIAttributes
multipleAttr      :: !Bool                                -> UIAttributes
instanceNoAttr    :: !Int                                 -> UIAttributes
instanceKeyAttr   :: !String                              -> UIAttributes
columnsAttr       :: ![String]                            -> UIAttributes
doubleClickAttr   :: !String !String                      -> UIAttributes
actionIdAttr      :: !String                              -> UIAttributes
editorIdAttr      :: !String                              -> UIAttributes
taskIdAttr        :: !String                              -> UIAttributes
labelAttr         :: !String                              -> UIAttributes
styleAttr         :: !String                              -> UIAttributes
classAttr         :: !String                              -> UIAttributes
resizableAttr     :: ![UISide]                            -> UIAttributes
maxlengthAttr     :: !Int                                 -> UIAttributes
minlengthAttr     :: !Int                                 -> UIAttributes
boundedlengthAttr :: !Int !Int                            -> UIAttributes
eventTimeoutAttr  :: !(Maybe Int)                         -> UIAttributes
steppedAttr       :: !Bool                                -> UIAttributes

editAttrs         :: !String !String !(Maybe JSONNode)    -> UIAttributes
choiceAttrs       :: !String !String ![Int] ![JSONNode]   -> UIAttributes

//Util
isOptional :: !UI -> Bool	
stringDisplay   :: !String  -> UI

//Encoding of UI to the format sent to the client framework
class encodeUI a :: a -> JSONNode
instance encodeUI Int
instance encodeUI Real
instance encodeUI Char
instance encodeUI String
instance encodeUI Bool
instance encodeUI HtmlTag
instance encodeUI JSONNode
instance encodeUI (Maybe a) | encodeUI a
instance encodeUI [a] | encodeUI a
instance encodeUI UI
instance encodeUI UISideSizes
instance encodeUI UISize
instance encodeUI UIBound
instance encodeUI UIVAlign
instance encodeUI UIHAlign
instance encodeUI UIDirection
instance encodeUI UIWindowType

//Combine two changes that would have to be applied one after the other into a single change
mergeUIChanges :: UIChange UIChange -> UIChange

//Apply changes to a ui
applyUIChange :: !UIChange !UI -> UI
applyUIAttributeChange :: !UIAttributeChange !UIAttributes -> UIAttributes

//Remove all paths that lead to a NoChange node
compactUIChange :: UIChange -> UIChange

//Makes sure that all children ranging 0 to max(index) are in the list
completeChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]

//Reassigns indices from 0 upwarths to the changes in the list
reindexChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
//Remove all childchanges that do nothing
compactChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]

//Serialize change definitions such that they can be sent to a client
encodeUIChange :: !UIChange -> JSONNode
encodeUIChanges :: ![UIChange] -> JSONNode

