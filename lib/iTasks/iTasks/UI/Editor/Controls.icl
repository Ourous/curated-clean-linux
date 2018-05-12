implementation module iTasks.UI.Editor.Controls

import iTasks.UI.Definition, iTasks.UI.Editor
import StdFunc, StdBool, Data.GenEq, StdList
import Data.Error, Text.GenJSON, Text.HTML
import qualified Data.Map as DM

import iTasks.UI.Definition
import iTasks.UI.Editor.Modifiers

disableOnView e = selectByMode (withAttributes (enabledAttr False) e) e e

textField :: Editor String
textField = fieldComponent UITextField

textArea :: Editor String
textArea = fieldComponent UITextArea

passwordField :: Editor String
passwordField = fieldComponent UIPasswordField

integerField :: Editor Int
integerField = fieldComponent UIIntegerField

decimalField :: Editor Real
decimalField = fieldComponent UIDecimalField

documentField :: Editor (!String,!String,!String,!String,!Int)
documentField = fieldComponent UIDocumentField

checkBox :: Editor Bool
checkBox = fieldComponent UICheckbox

slider :: Editor Int
slider = fieldComponent UISlider

button :: Editor Bool
button = fieldComponent UIButton

label :: Editor String
label = viewComponent (\text -> (textAttr text)) UILabel

icon :: Editor (!String,!Maybe String)
icon = viewComponent (\(iconCls,tooltip) -> 'DM'.unions [iconClsAttr iconCls,maybe 'DM'.newMap tooltipAttr tooltip]) UIIcon

textView :: Editor String
textView = viewComponent (\text -> valueAttr (JSONString (escapeStr text))) UITextView

htmlView :: Editor HtmlTag
htmlView = viewComponent (\html -> valueAttr (JSONString (toString html))) UIHtmlView

progressBar :: Editor (Maybe Int, Maybe String)
progressBar = viewComponent combine UIProgressBar
where
	combine (amount,text) = 'DM'.unions ((maybe [] (\t -> [textAttr t]) text) ++ (maybe [] (\v -> [valueAttr (JSONInt v)]) amount))
						
dropdown :: Editor ([ChoiceText], [Int])
dropdown = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIDropdown

checkGroup :: Editor ([ChoiceText], [Int])
checkGroup = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UICheckGroup

choiceList :: Editor ([ChoiceText], [Int])
choiceList = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIChoiceList

toOptionText {ChoiceText|id,text}= JSONObject [("id",JSONInt id),("text",JSONString text)]
checkBoundsText options idx = or [id == idx \\ {ChoiceText|id} <- options]

grid :: Editor (ChoiceGrid, [Int])
grid = choiceComponent (\{ChoiceGrid|header} -> columnsAttr header) (\{ChoiceGrid|rows} -> rows) toOption checkBounds UIGrid
where
	toOption {ChoiceRow|id,cells}= JSONObject [("id",JSONInt id),("cells",JSONArray (map (JSONString o toString) cells))]
	checkBounds options idx = or [id == idx \\ {ChoiceRow|id} <- options]

tree :: Editor ([ChoiceNode], [Int])
tree = choiceComponent (const 'DM'.newMap) id toOption checkBounds UITree
where
	toOption {ChoiceNode|id,label,icon,expanded,children}
		= JSONObject [("text",JSONString label)
					 ,("iconCls",maybe JSONNull (\i -> JSONString ("icon-"+++i)) icon)
					 ,("id",JSONInt id)
					 ,("expanded",JSONBool expanded)
					 ,("children",JSONArray (map toOption children))
					]

	checkBounds options idx 
		= or (map (checkNode idx) options)
	checkNode idx {ChoiceNode|id,children}
		| idx == id = True
		| otherwise = or (map (checkNode idx) children)

//Field like components for which simply knowing the UI type is sufficient
fieldComponent :: UIType -> Editor a | JSONDecode{|*|}, JSONEncode{|*|}, gEq{|*|} a
fieldComponent type = disableOnView {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where 
	genUI dp val vst=:{VSt|taskId,mode,optional}
		# val = if (mode =: Enter) JSONNull (toJSON val) 
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = val}
		# attr = 'DM'.unions [optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr val]
		= (Ok (uia type attr,mask),vst)

	onEdit dp (tp,e) val mask vst=:{VSt|optional}
		= case e of
			JSONNull = (Ok (ChangeUI [SetAttribute "value" JSONNull] [],FieldMask {touched=True,valid=optional,state=JSONNull}),val,vst)
			json = case fromJSON e of
				Nothing  = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),val,vst)
				Just val = (Ok (ChangeUI [SetAttribute "value" (toJSON val)] [],FieldMask {touched=True,valid=True,state=toJSON val}),val,vst)

	onRefresh dp new old mask vst=:{VSt|mode,optional}
		| old === new = (Ok (NoChange,mask),new,vst)
		| otherwise   = (Ok (ChangeUI [SetAttribute "value" (toJSON new)] [],mask),new,vst)

//Components which cannot be edited 
viewComponent toAttributes type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst
		= (Ok (uia type (toAttributes val), FieldMask {touched = False, valid = True, state = JSONNull}),vst)

	onEdit dp (tp,e) val mask vst
		= (Error "Edit event for view component",val,vst)

	onRefresh dp new old mask vst
		= case [SetAttribute nk nv \\ ((ok,ov),(nk,nv)) <- zip ('DM'.toList (toAttributes old),'DM'.toList (toAttributes new)) | ok == nk && ov =!= nv] of
			[] 		= (Ok (NoChange,mask),new,vst)
			changes = (Ok (ChangeUI changes [],mask),new,vst)

//Choice components that have a set of options
choiceComponent attr getOptions toOption checkBounds type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val,sel) vst=:{VSt|taskId,mode,optional}
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = JSONNull}
		# sel = if (mode =: Enter) [] sel //When entering, the selection is initially empty
		# attr = 'DM'.unions [attr val,choiceAttrs taskId (editorId dp) sel (map toOption (getOptions val))]
		= (Ok (uia type attr,mask), vst)

	onEdit dp (tp,e) (val,sel) mask vst=:{VSt|optional}
		# options = getOptions val
		= case e of
			JSONNull
				= (Ok (NoChange,FieldMask {touched=True,valid=optional,state=JSONNull}),(val,[]),vst)
			(JSONArray ids)
				# selection = [i \\ JSONInt i <- ids]
				| all (checkBounds options) selection
					# multiple = maybe False (\(JSONBool b) -> b) ('DM'.get "multiple" (attr val))
					# valid = if (selection =: []) multiple True //The selection is only allowed to be empty when multiselect is enabled
					= (Ok (NoChange,FieldMask {touched=True,valid=valid,state=JSONArray ids}),(val,selection),vst)
				| otherwise
					= (Error ("Choice event out of bounds: " +++ toString (JSONArray ids)),(val,sel),vst)
			_ 
				= (Error ("Invalid choice event: " +++ toString e), (val,sel),vst)

	onRefresh dp (new,nsel) (old,osel) mask vst
		//Check options
		# oOpts = map toOption (getOptions old)
		# nOpts = map toOption (getOptions new)
		# cOptions= if (nOpts =!= oOpts) (ChangeUI [SetAttribute "options" (JSONArray nOpts)] []) NoChange
		# cSel = if (nsel =!= osel) (ChangeUI [SetAttribute "value" (toJSON nsel)] []) NoChange
		//Check selection
		= (Ok (mergeUIChanges cOptions cSel, mask),(new,nsel),vst)
