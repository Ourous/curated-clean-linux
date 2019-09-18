implementation module iTasks.UI.Definition

import StdEnv
import Text.GenJSON, Data.GenEq, Text.HTML, Text, Data.Maybe
from Data.Map import :: Map (..)
import Data.Map.GenJSON
from Data.Functor import class Functor(..)
import qualified Data.Map as DM
import qualified Data.List as DL

from iTasks.WF.Definition import class iTask(..)
from iTasks.Internal.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks.Internal.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditState
from iTasks.UI.Editor.Generic import generic gEditor
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

import Text.HTML

derive class iTask UI, UIType
derive class iTask UISize, UIBound, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UITreeNode 

ui :: UIType -> UI
ui type = UI type 'DM'.newMap []

uic :: UIType [UI] -> UI
uic type items = UI type 'DM'.newMap items

uia :: UIType UIAttributes -> UI
uia type attr = UI type attr []

uiac :: UIType UIAttributes [UI] -> UI
uiac type attr items = UI type attr items

emptyAttr :: UIAttributes
emptyAttr = 'DM'.newMap

optionalAttr :: !Bool -> UIAttributes
optionalAttr optional = 'DM'.singleton "optional" (JSONBool optional)

sizeAttr :: !UISize !UISize -> UIAttributes
sizeAttr width height = 'DM'.fromList [("width",encodeUI width),("height",encodeUI height)]

widthAttr :: !UISize -> UIAttributes
widthAttr width = 'DM'.singleton "width" (encodeUI width)

heightAttr :: !UISize -> UIAttributes
heightAttr height = 'DM'.singleton "height" (encodeUI height)

hintAttr :: !String -> UIAttributes
hintAttr hint = 'DM'.singleton "hint" (JSONString hint)

titleAttr :: !String -> UIAttributes
titleAttr title = 'DM'.singleton "title" (JSONString title)

iconClsAttr :: !String -> UIAttributes
iconClsAttr iconCls = 'DM'.singleton "iconCls" (JSONString iconCls)

tooltipAttr :: !String -> UIAttributes
tooltipAttr tooltip = 'DM'.singleton "tooltip" (JSONString tooltip)

hposAttr :: !UIHAlign -> UIAttributes
hposAttr pos = 'DM'.singleton "hpos" (encodeUI pos)

vposAttr :: !UIVAlign -> UIAttributes
vposAttr pos = 'DM'.singleton "vpos" (encodeUI pos)

windowTypeAttr :: !UIWindowType -> UIAttributes
windowTypeAttr windowType = 'DM'.singleton "windowType" (encodeUI windowType)

focusTaskIdAttr :: !String -> UIAttributes
focusTaskIdAttr taskId = 'DM'.singleton "focusTaskId" (JSONString taskId)

closeTaskIdAttr :: !String -> UIAttributes
closeTaskIdAttr taskId = 'DM'.singleton "closeTaskId" (JSONString taskId)

activeTabAttr :: !Int -> UIAttributes
activeTabAttr activeTab = 'DM'.singleton "activeTab" (JSONInt activeTab)

valueAttr :: !JSONNode -> UIAttributes
valueAttr value = 'DM'.singleton "value" value

minAttr :: !Int -> UIAttributes
minAttr min = 'DM'.singleton "min" (JSONInt min)

maxAttr :: !Int -> UIAttributes
maxAttr max = 'DM'.singleton "max" (JSONInt max)

textAttr :: !String -> UIAttributes
textAttr text = 'DM'.singleton "text" (JSONString text)

enabledAttr :: !Bool -> UIAttributes
enabledAttr enabled = 'DM'.singleton "enabled" (JSONBool enabled)

multipleAttr :: !Bool -> UIAttributes
multipleAttr multiple = 'DM'.singleton "multiple" (JSONBool multiple)

instanceNoAttr :: !Int -> UIAttributes
instanceNoAttr instanceNo = 'DM'.singleton "instanceNo" (JSONInt instanceNo)

instanceKeyAttr :: !String -> UIAttributes
instanceKeyAttr instanceKey = 'DM'.singleton "instanceKey" (JSONString instanceKey)

columnsAttr :: ![String] -> UIAttributes
columnsAttr columns = 'DM'.singleton "columns" (JSONArray (map JSONString columns))

doubleClickAttr :: !String !String -> UIAttributes
doubleClickAttr taskId actionId = 'DM'.singleton "doubleClickAction" (JSONArray [JSONString taskId,JSONString actionId])

actionIdAttr :: !String -> UIAttributes
actionIdAttr actionId = 'DM'.singleton "actionId" (JSONString actionId)

taskIdAttr :: !String -> UIAttributes
taskIdAttr taskId = 'DM'.singleton "taskId" (JSONString taskId)

editorIdAttr :: !String -> UIAttributes
editorIdAttr taskId = 'DM'.singleton "editorId" (JSONString taskId)

labelAttr :: !String -> UIAttributes
labelAttr taskId = 'DM'.singleton "label" (JSONString taskId)

styleAttr :: !String -> UIAttributes
styleAttr style = 'DM'.singleton "style" (JSONString style)

classAttr :: ![String] -> UIAttributes
classAttr classes = 'DM'.singleton "class" (JSONArray (map JSONString classes))

addClassAttr :: !String !UIAttributes -> UIAttributes
addClassAttr classname attributes = 'DM'.put "class" (JSONArray [JSONString classname:classes]) attributes
where
	classes = case 'DM'.get "class" attributes of (Just (JSONArray names)) = names ; _ = []

removeClassAttr :: !String !UIAttributes -> UIAttributes
removeClassAttr remove attributes 
	= case 'DM'.get "class" attributes of
		(Just (JSONArray items)) = 'DM'.put "class" (JSONArray [i \\ i=:(JSONString name) <- items | name <> remove]) attributes
		_ = attributes

resizableAttr :: ![UISide] -> UIAttributes
resizableAttr sides = 'DM'.singleton "resizable" (JSONArray (map encodeUI sides))

maxlengthAttr :: !Int -> UIAttributes
maxlengthAttr maxlength = 'DM'.singleton "maxlength" (JSONInt maxlength)

minlengthAttr :: !Int -> UIAttributes
minlengthAttr minlength = 'DM'.singleton "minlength" (JSONInt minlength)

boundedlengthAttr :: !Int !Int -> UIAttributes
boundedlengthAttr min max = 'DM'.unions [minlengthAttr min, maxlengthAttr max]

eventTimeoutAttr :: !(Maybe Int) -> UIAttributes
eventTimeoutAttr timeout = 'DM'.singleton "eventTimeout" (maybe JSONNull JSONInt timeout)

editAttrs :: !String !String !(Maybe JSONNode) -> UIAttributes
editAttrs taskId editorId mbValue 
	= 'DM'.fromList [("taskId",JSONString taskId),("editorId",JSONString editorId):maybe [] (\value -> [("value",value)]) mbValue]

choiceAttrs :: !String !String ![Int] ![JSONNode] -> UIAttributes
choiceAttrs taskId editorId value options
	= 'DM'.fromList [("taskId",JSONString taskId),("editorId",JSONString editorId),("value",JSONArray (map JSONInt value)),("options",JSONArray options)]

isOptional :: !UI -> Bool
isOptional (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr)

stringDisplay :: !String -> UI
stringDisplay value = uia UITextView (valueAttr (JSONString (escapeStr value)))

//Encoding of UI definitions to the JSON format expected by the client
class encodeUI a :: a -> JSONNode

instance encodeUI Int				where encodeUI v = JSONInt v
instance encodeUI Real				where encodeUI v = JSONReal v
instance encodeUI Char 				where encodeUI v = JSONString (toString v)
instance encodeUI String			where encodeUI v = JSONString v
instance encodeUI Bool				where encodeUI v = JSONBool v
instance encodeUI HtmlTag			where encodeUI v = JSONString (toString v)

instance encodeUI JSONNode
where
	encodeUI v = toJSON v

instance encodeUI (Maybe a) | encodeUI a
where
	encodeUI Nothing = JSONNull
	encodeUI (Just a) = encodeUI a

instance encodeUI [a] | encodeUI a
where
	encodeUI l = JSONArray (map encodeUI l)

instance encodeUI UI
where
	encodeUI (UI type attr items) = JSONObject (typeField ++ attrFields ++ childrenField)
	where
		typeField = [("type",JSONString (toString type))]
		attrFields = case attr of
			'DM'.Tip = []
			_        = [("attributes",JSONObject ('DM'.toList attr))]
		childrenField = case items of
			[]    = []
			_     = [("children",JSONArray (map encodeUI items))]

instance toString UIType
where
	toString UIEmpty           = "RawEmpty"
	toString UIAction          = "RawAction"

	toString UIComponent       = "Component" 
	toString UIViewport        = "Viewport"
	toString UILoader          = "Loader"

	toString UITextField       = "TextField"
	toString UITextArea        = "TextArea"
	toString UIPasswordField   = "PasswordField"
	toString UIIntegerField    = "IntegerField"
	toString UIDecimalField    = "DecimalField"
	toString UIDocumentField   = "DocumentField"
	toString UICheckbox        = "Checkbox"
	toString UISlider          = "Slider"
	toString UIButton          = "Button"
	toString UILabel           = "Label"
	toString UIIcon            = "Icon"

	toString UITextView        = "TextView"
	toString UIHtmlView        = "HtmlView"
	toString UIProgressBar     = "ProgressBar"

	toString UIDropdown        = "Dropdown"
	toString UICheckGroup      = "CheckGroup"
	toString UIChoiceList      = "ChoiceList"
	toString UIGrid            = "Grid"
	toString UITree            = "Tree"
	toString UITabBar          = "TabBar"

    toString UIContainer       = "Container"
	toString UIPanel           = "Panel"
	toString UITabSet          = "TabSet"
	toString UIWindow          = "Window"
	toString UIMenu            = "Menu"
	toString UIMenuSep         = "MenuSep"
	toString UIToolBar         = "ToolBar"
	toString UIButtonBar       = "ButtonBar"
	toString UIList            = "List"
	toString UIListItem        = "ListItem"
	toString UIDebug           = "Debug"

	toString UIData            = "Data"

instance encodeUI UISize
where
	encodeUI (ExactSize s)	= JSONInt s
	encodeUI WrapSize		= JSONString "wrap"
	encodeUI FlexSize		= JSONString "flex"

instance encodeUI UIBound
where
	encodeUI (ExactBound s)	= JSONInt s
	encodeUI WrapBound		= JSONString "wrap"

instance encodeUI UIVAlign
where
	encodeUI AlignTop		= JSONString "top"
	encodeUI AlignMiddle	= JSONString "middle"
	encodeUI AlignBottom	= JSONString "bottom"

instance encodeUI UIHAlign
where
	encodeUI AlignLeft		= JSONString "left"
	encodeUI AlignCenter	= JSONString "center"
	encodeUI AlignRight		= JSONString "right"

instance encodeUI UIDirection
where
	encodeUI Vertical		= JSONString "vertical"
	encodeUI Horizontal		= JSONString "horizontal"

instance encodeUI UIWindowType
where
	encodeUI FloatingWindow 	= JSONString "floating"
	encodeUI NotificationBubble = JSONString "bubble"

instance encodeUI UISide
where
	encodeUI TopSide    = JSONString "top"
	encodeUI BottomSide = JSONString "bottom"
	encodeUI LeftSide   = JSONString "left"
	encodeUI RightSide  = JSONString "right"

derive class iTask UIChange, UIAttributeChange, UIChildChange

mergeUIChanges :: UIChange UIChange -> UIChange
mergeUIChanges c1 NoChange = c1 
mergeUIChanges NoChange c2 = c2 
mergeUIChanges _ (ReplaceUI ui2) = ReplaceUI ui2 //Any previous change is void when it is followed by a replace
mergeUIChanges (ReplaceUI ui1) (ChangeUI ca2 ci2) = ReplaceUI (applyUIChange (ChangeUI ca2 ci2) ui1)
mergeUIChanges (ChangeUI ca1 ci1) (ChangeUI ca2 ci2) = ChangeUI (ca1 ++ ca2) (ci1 ++ ci2)

applyUIChange :: !UIChange !UI -> UI
applyUIChange NoChange ui = ui 
applyUIChange (ReplaceUI ui) _ = ui
applyUIChange (ChangeUI ca ci) (UI type attr items)
	//Change the attributes
	# attr = foldl (flip applyUIAttributeChange) attr ca
	//Adjust the children
	# items = foldl appChildChange items ci
	= UI type attr items
where
	appChildChange items (i,RemoveChild)
		| i >= 0 && i < length items = removeAt i items
									 = items
	appChildChange items (i,InsertChild ui)
		| i >= 0 && i <= length items = insertAt i ui items
									  = items
	appChildChange items (i,ChangeChild change)
		| i >= 0 && i < length items = updateAt i (applyUIChange change (items !! i)) items
		                             = items
	appChildChange items (s,MoveChild d)
		# num = length items
		| s >= 0 && d >= 0 && s < num && d < num = insertAt d (items !! s) (removeAt s items)
                                                 = items

applyUIAttributeChange :: !UIAttributeChange !UIAttributes -> UIAttributes
applyUIAttributeChange (SetAttribute k v) attr  = 'DM'.put k v attr
applyUIAttributeChange (DelAttribute k) attr = 'DM'.del k attr

//Remove unnessecary directives
compactUIChange :: UIChange -> UIChange
compactUIChange (ChangeUI local children) = case (local,compactChildren children) of
	([],[]) = NoChange
	(local,children) = ChangeUI local children
where
	compactChildren [] = [] 
	compactChildren [(idx,ChangeChild change):cs] = case (compactUIChange change) of
		NoChange = compactChildren cs
		change   = [(idx,ChangeChild change):compactChildren cs]

	compactChildren [c:cs] = [c:compactChildren cs]
compactUIChange change = change

completeChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
completeChildChanges children = complete 0 (sortBy indexCmp children)
where
	complete i [] = []
	complete i [c:cs]
		| i < fst c = [(i,ChangeChild NoChange):complete (i + 1) cs]
					= [c:complete (fst c + 1) cs]
	indexCmp x y = fst x < fst y

reindexChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
reindexChildChanges children = [(i,c) \\ (_,c) <- children & i <- [0..]]

compactChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
compactChildChanges children = [c \\ c <- children | not (noChangeChild c)]
where
	noChangeChild (_,ChangeChild NoChange) = True
	noChangeChild _ = False

encodeUIChanges:: ![UIChange] -> JSONNode
encodeUIChanges defs = JSONArray (map encodeUIChange defs)

encodeUIChange :: !UIChange -> JSONNode
encodeUIChange NoChange = JSONNull
encodeUIChange (ReplaceUI def)
	= JSONObject
		[("type",JSONString "replace")
		,("definition",encodeUI def)
		]
encodeUIChange (ChangeUI attributes children)
	= JSONObject fields
where
	fields = [("type",JSONString "change"):attributesField]
	attributesField = case attributes of
		[] -> childrenField
		_  -> [("attributes",JSONArray [encodeAttrChange a \\ a <- attributes]):childrenField]
	childrenField = case children of
		[] -> []
		_  -> [("children",JSONArray [encodeChildChange c \\ c <- children])]

	encodeAttrChange (SetAttribute name value) = JSONObject [("name",JSONString name),("value",value)]
	encodeAttrChange (DelAttribute name) = JSONObject [("name",JSONString name),("value",JSONNull)]

	encodeChildChange (i,ChangeChild child) = JSONArray [JSONInt i,JSONString "change",encodeUIChange child]
	encodeChildChange (i,RemoveChild) 		= JSONArray [JSONInt i,JSONString "remove"]
	encodeChildChange (i,InsertChild child) = JSONArray [JSONInt i,JSONString "insert",encodeUI child]
	encodeChildChange (i,MoveChild ni)      = JSONArray [JSONInt i,JSONString "move",JSONInt ni]
