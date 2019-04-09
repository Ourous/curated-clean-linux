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
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UITreeNode 

//SHOULD BE IN Text.GenJSON
jsonObjectPut :: String JSONNode JSONNode -> JSONNode
jsonObjectPut k v (JSONObject fields) = JSONObject (put k v fields)
where
	put k v [] = [(k,v)]
	put k v [(fk,fv):fs] = if (k == fk) [(fk,v):fs] [(fk,fv):put k v fs]
jsonObjectPut k v node = node

jsonObjectGet :: String JSONNode -> Maybe JSONNode
jsonObjectGet k (JSONObject fields) = get k fields
where
	get k [] = Nothing
	get k [(fk,fv):fs] = if (k == fk) (Just fv) (get k fs)
jsonObjectGet k node = Nothing

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
optionalAttr optional = 'DM'.fromList [("optional",JSONBool optional)]

sizeAttr :: !UISize !UISize -> UIAttributes
sizeAttr width height = 'DM'.fromList [("width",encodeUI width),("height",encodeUI height)]

widthAttr :: !UISize -> UIAttributes
widthAttr width = 'DM'.fromList [("width",encodeUI width)]

heightAttr :: !UISize -> UIAttributes
heightAttr height = 'DM'.fromList [("height",encodeUI height)]

minSizeAttr :: !UIBound !UIBound -> UIAttributes
minSizeAttr minWidth minHeight = 'DM'.fromList [("minWidth",encodeUI minWidth),("minHeight",encodeUI minHeight)]

minWidthAttr :: !UIBound -> UIAttributes
minWidthAttr minWidth = 'DM'.fromList [("minWidth",encodeUI minWidth)]

minHeightAttr :: !UIBound -> UIAttributes
minHeightAttr minHeight = 'DM'.fromList [("minHeight",encodeUI minHeight)]

maxSizeAttr :: !UIBound !UIBound -> UIAttributes
maxSizeAttr maxWidth maxHeight = 'DM'.fromList [("maxWidth",encodeUI maxWidth),("maxHeight",encodeUI maxHeight)]

maxWidthAttr :: !UIBound -> UIAttributes
maxWidthAttr maxWidth = 'DM'.fromList [("maxWidth",encodeUI maxWidth)]

maxHeightAttr :: !UIBound -> UIAttributes
maxHeightAttr maxHeight = 'DM'.fromList [("maxHeight",encodeUI maxHeight)]

marginsAttr :: !Int !Int !Int !Int -> UIAttributes
marginsAttr top right bottom left = 'DM'.fromList [("marginTop",JSONInt top),("marginRight",JSONInt right),("marginBottom",JSONInt bottom),("marginLeft",JSONInt left)]

topMarginAttr :: !Int -> UIAttributes
topMarginAttr top = 'DM'.fromList [("marginTop",JSONInt top)]

rightMarginAttr :: !Int -> UIAttributes
rightMarginAttr right = 'DM'.fromList [("marginRight",JSONInt right)]

bottomMarginAttr :: !Int -> UIAttributes
bottomMarginAttr bottom = 'DM'.fromList [("marginBottom",JSONInt bottom)]

leftMarginAttr :: !Int -> UIAttributes
leftMarginAttr left = 'DM'.fromList [("marginLeft",JSONInt left)]

paddingAttr :: !Int !Int !Int !Int -> UIAttributes
paddingAttr top right bottom left = 'DM'.fromList [("paddingTop",JSONInt top),("paddingRight",JSONInt right),("paddingBottom",JSONInt bottom),("paddingLeft",JSONInt left)]

topPaddingAttr :: !Int -> UIAttributes
topPaddingAttr top = 'DM'.fromList [("paddingTop",JSONInt top)]

rightPaddingAttr :: !Int -> UIAttributes
rightPaddingAttr right = 'DM'.fromList [("paddingRight",JSONInt right)]

bottomPaddingAttr :: !Int -> UIAttributes
bottomPaddingAttr bottom = 'DM'.fromList [("paddingBottom",JSONInt bottom)]

leftPaddingAttr :: !Int -> UIAttributes
leftPaddingAttr left = 'DM'.fromList [("paddingLeft",JSONInt left)]

titleAttr :: !String -> UIAttributes
titleAttr title = 'DM'.fromList [("title",JSONString title)]

frameAttr :: !Bool -> UIAttributes
frameAttr frame = 'DM'.fromList [("frame",JSONBool frame)]

iconClsAttr :: !String -> UIAttributes
iconClsAttr iconCls = 'DM'.fromList [("iconCls",JSONString iconCls)]

tooltipAttr :: !String -> UIAttributes
tooltipAttr tooltip = 'DM'.fromList [("tooltip",JSONString tooltip)]

directionAttr :: !UIDirection -> UIAttributes
directionAttr direction = 'DM'.fromList [("direction",encodeUI direction)]

halignAttr :: !UIHAlign -> UIAttributes
halignAttr align = 'DM'.fromList [("halign",encodeUI align)]

valignAttr :: !UIVAlign -> UIAttributes
valignAttr align = 'DM'.fromList [("valign",encodeUI align)]

hposAttr :: !UIHAlign -> UIAttributes
hposAttr pos = 'DM'.fromList [("hpos",encodeUI pos)]

vposAttr :: !UIVAlign -> UIAttributes
vposAttr pos = 'DM'.fromList [("vpos",encodeUI pos)]

windowTypeAttr :: !UIWindowType -> UIAttributes
windowTypeAttr windowType = 'DM'.fromList [("windowType",encodeUI windowType)]

focusTaskIdAttr :: !String -> UIAttributes
focusTaskIdAttr taskId = 'DM'.fromList [("focusTaskId",JSONString taskId)]

closeTaskIdAttr :: !String -> UIAttributes
closeTaskIdAttr taskId = 'DM'.fromList [("closeTaskId",JSONString taskId)]

activeTabAttr :: !Int -> UIAttributes
activeTabAttr activeTab = 'DM'.fromList [("activeTab",JSONInt activeTab)]

valueAttr :: !JSONNode -> UIAttributes
valueAttr value = 'DM'.fromList [("value",value)]

minAttr :: !Int -> UIAttributes
minAttr min = 'DM'.fromList [("min",JSONInt min)]

maxAttr :: !Int -> UIAttributes
maxAttr max = 'DM'.fromList [("max",JSONInt max)]

textAttr :: !String -> UIAttributes
textAttr text = 'DM'.fromList [("text",JSONString text)]

enabledAttr :: !Bool -> UIAttributes
enabledAttr enabled = 'DM'.fromList [("enabled",JSONBool enabled)]

multipleAttr :: !Bool -> UIAttributes
multipleAttr multiple = 'DM'.fromList [("multiple",JSONBool multiple)]

instanceNoAttr :: !Int -> UIAttributes
instanceNoAttr instanceNo = 'DM'.fromList [("instanceNo",JSONInt instanceNo)]

instanceKeyAttr :: !String -> UIAttributes
instanceKeyAttr instanceKey = 'DM'.fromList [("instanceKey",JSONString instanceKey)]

columnsAttr :: ![String] -> UIAttributes
columnsAttr columns = 'DM'.fromList [("columns",JSONArray (map JSONString columns))]

doubleClickAttr :: !String !String -> UIAttributes
doubleClickAttr taskId actionId = 'DM'.fromList [("doubleClickAction",JSONArray [JSONString taskId,JSONString actionId])]

actionIdAttr :: !String -> UIAttributes
actionIdAttr actionId = 'DM'.fromList [("actionId",JSONString actionId)]

taskIdAttr :: !String -> UIAttributes
taskIdAttr taskId = 'DM'.fromList [("taskId",JSONString taskId)]

editorIdAttr :: !String -> UIAttributes
editorIdAttr taskId = 'DM'.fromList [("editorId",JSONString taskId)]

labelAttr :: !String -> UIAttributes
labelAttr taskId = 'DM'.fromList [(LABEL_ATTRIBUTE,JSONString taskId)]

styleAttr :: !String -> UIAttributes
styleAttr style = 'DM'.fromList [("style",JSONString style)]

classAttr :: !String -> UIAttributes
classAttr cls = 'DM'.fromList [("class",JSONString cls)]

resizableAttr :: ![UISide] -> UIAttributes
resizableAttr sides = 'DM'.fromList [("resizable",JSONArray (map encodeUI sides))]

maxlengthAttr :: !Int -> UIAttributes
maxlengthAttr l = 'DM'.fromList [("maxlength", JSONInt l)]

minlengthAttr :: !Int -> UIAttributes
minlengthAttr l = 'DM'.fromList [("minlength", JSONInt l)]

boundedlengthAttr :: !Int !Int -> UIAttributes
boundedlengthAttr min max = 'DM'.unions [minlengthAttr min, maxlengthAttr max]

eventTimeoutAttr :: !(Maybe Int) -> UIAttributes
eventTimeoutAttr to = 'DM'.fromList [("eventTimeout", maybe JSONNull JSONInt to)]

steppedAttr :: !Bool -> UIAttributes
steppedAttr stepped = 'DM'.fromList [(STEPPED_ATTRIBUTE, JSONBool stepped)]

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
		typeField     = [("type",JSONString (toString type))]
		attrFields    = [("attributes",JSONObject ('DM'.toList attr))]
		childrenField = case items of
			[]    = []
			_     = [("children",JSONArray (map encodeUI items))]

instance toString UIType
where
	toString UIEmpty           = "RawEmpty"
	toString UIAction          = "RawAction"
	toString UIPair            = "RawPair"
	toString UIRecord          = "RawRecord"
	toString UICons            = "RawCons"
	toString UIVarCons         = "RawVarCons"
	toString UIInteract        = "RawInteract"
	toString UIStep            = "RawStep"
	toString UIParallel        = "RawParallel"

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

instance encodeUI UISideSizes 
where
	encodeUI {top,right,bottom,left}
		= JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)

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
	= JSONObject
		[("type",JSONString "change")
		,("attributes", JSONArray (map encodeAttrChange attributes))
		,("children",JSONArray (map encodeChildChange children))
		]
where
	encodeAttrChange (SetAttribute name value) = JSONObject [("name",JSONString name),("value",value)]
	encodeAttrChange (DelAttribute name) = JSONObject [("name",JSONString name),("value",JSONNull)]

	encodeChildChange (i,ChangeChild child) = JSONArray [JSONInt i,JSONString "change",encodeUIChange child]
	encodeChildChange (i,RemoveChild) 		= JSONArray [JSONInt i,JSONString "remove"]
	encodeChildChange (i,InsertChild child) = JSONArray [JSONInt i,JSONString "insert",encodeUI child]
	encodeChildChange (i,MoveChild ni)      = JSONArray [JSONInt i,JSONString "move",JSONInt ni]

