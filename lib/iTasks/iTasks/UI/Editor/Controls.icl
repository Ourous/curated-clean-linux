implementation module iTasks.UI.Editor.Controls

import StdEnv
import iTasks.UI.Definition, iTasks.UI.Editor
import Data.GenEq, Data.Error, Text.GenJSON, Text.HTML, Data.Func, Data.Functor, Data.Tuple, Data.List, Data.Maybe, Data.Map.GenJSON
import qualified Data.Map as DM

import iTasks.UI.Definition
import iTasks.UI.Editor.Modifiers

disableOnView e = selectByMode (e <<@ enabledAttr False) e e

textField :: Editor String
textField = fieldComponent UITextField (Just "") isValidString

textArea :: Editor String
textArea = fieldComponent UITextArea (Just "") isValidString

passwordField :: Editor String
passwordField = fieldComponent UIPasswordField (Just "") isValidString

isValidString :: !UIAttributes !String -> Bool
isValidString attrs str
	= lStr >= getLengthAttr 0 "minlength" && lStr <= getLengthAttr lStr "maxlength"
where
	getLengthAttr :: !Int !String -> Int
	getLengthAttr default attr = case 'DM'.get attr attrs of
		Just (JSONInt l) = l
		_                = default

	lStr = size str

integerField :: Editor Int
integerField = fieldComponent UIIntegerField Nothing (\_ _ -> True)

decimalField :: Editor Real
decimalField = fieldComponent UIDecimalField Nothing (\_ _ -> True)

documentField :: Editor (!String,!String,!String,!String,!Int)
documentField = fieldComponent UIDocumentField Nothing (\_ _ -> True)

checkBox :: Editor Bool
checkBox = fieldComponent UICheckbox (Just False) (\_ _ -> True)

slider :: Editor Int
slider = fieldComponent UISlider Nothing (\_ _ -> True)

button :: Editor Bool
button = fieldComponent UIButton Nothing (\_ _ -> True)

label :: Editor String
label = viewComponent textAttr UILabel

icon :: Editor (!String,!Maybe String)
icon = viewComponent (\(iconCls,tooltip) -> 'DM'.unions [iconClsAttr iconCls,maybe 'DM'.newMap tooltipAttr tooltip])
                     UIIcon

textView :: Editor String
textView = viewComponent (valueAttr o JSONString o escapeStr) UITextView

htmlView :: Editor HtmlTag
htmlView = viewComponent (valueAttr o JSONString o toString) UIHtmlView

progressBar :: Editor (Maybe Int, Maybe String)
progressBar = viewComponent combine UIProgressBar
where
	combine (amount,text) =
		'DM'.unions ((maybe [] (\t -> [textAttr t]) text) ++ (maybe [] (\v -> [valueAttr (JSONInt v)]) amount))
						
dropdown :: Editor ([ChoiceText], [Int])
dropdown = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIDropdown

checkGroup :: Editor ([ChoiceText], [Int])
checkGroup = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UICheckGroup

choiceList :: Editor ([ChoiceText], [Int])
choiceList = choiceComponent (const 'DM'.newMap) id toOptionText checkBoundsText UIChoiceList

toOptionText {ChoiceText|id,text}= JSONObject [("id",JSONInt id),("text",JSONString text)]
checkBoundsText options idx = or [id == idx \\ {ChoiceText|id} <- options]

derive JSONEncode ChoiceText
derive JSONDecode ChoiceText

grid :: Editor (ChoiceGrid, [Int])
grid = choiceComponent (\{ChoiceGrid|header} -> columnsAttr header) (\{ChoiceGrid|rows} -> rows) toOption checkBounds UIGrid
where
	toOption {ChoiceRow|id,cells}= JSONObject [("id",JSONInt id),("cells",JSONArray (map (JSONString o toString) cells))]
	checkBounds options idx = or [id == idx \\ {ChoiceRow|id} <- options]

derive JSONEncode ChoiceGrid, ChoiceRow
derive JSONDecode ChoiceGrid, ChoiceRow

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

derive JSONEncode ChoiceNode
derive JSONDecode ChoiceNode

withConstantChoices :: !choices !(Editor (!choices, ![Int])) -> Editor [Int]
withConstantChoices choices editor = bijectEditorValue (\sel -> (choices, sel)) snd
                                     (withChangedEditMode editModeFor editor)
where
	// enter mode has to be changed to update mode to pass the choices to the editor
	editModeFor Enter = Update (choices, [])
	editModeFor other = other

//Field like components for which simply knowing the UI type is sufficient
fieldComponent
	:: !UIType !(Maybe a) !(UIAttributes a -> Bool) -> Editor a
	| JSONDecode{|*|}, JSONEncode{|*|}, gEq{|*|} a
fieldComponent type mbEditModeInitValue isValid = disableOnView $ editorWithJSONEncode (leafEditorToEditor o leafEditor)
where 
	leafEditor toJSON =
		{LeafEditor|genUI=genUI toJSON,onEdit=onEdit,onRefresh=onRefresh toJSON,valueFromState=valueFromState}

	genUI toJSON attr dp mode vst=:{VSt|taskId,optional}
		# mbVal   = maybe mbEditModeInitValue Just $ editModeValue mode
		# mbVal   = maybe Nothing (\val -> if (isValid attr val) (Just val) Nothing) mbVal
		# attr    = 'DM'.unions [ optionalAttr optional
		                        , taskIdAttr taskId
		                        , editorIdAttr $ editorId dp
		                        , valueAttr $ maybe JSONNull toJSON mbVal
		                        , attr
		                        ]
		= (Ok (uia type attr, (mbVal, attr)), vst)

	onEdit _ (_, mbVal) (_, attrs) vst = (Ok (NoChange, (mbVal`, attrs)), vst)
	where
		mbVal` = case mbVal of
			Just val | isValid attrs val = Just val
			_                            = Nothing

	onRefresh toJSON dp new (mbOld, attrs) vst
		| mbOld === Just new = (Ok (NoChange, (mbOld, attrs)), vst)
		| otherwise          = (Ok (ChangeUI [SetAttribute "value" (toJSON new)] [], (if (isValid attrs new) (Just new) Nothing, attrs)), vst)

	valueFromState (mbVal, _) = mbVal

	editorWithJSONEncode :: !((a -> JSONNode) -> Editor a) -> Editor a | JSONEncode{|*|} a
	editorWithJSONEncode genFunc = genFunc toJSON

//Components which cannot be edited 
viewComponent :: !(a -> UIAttributes) !UIType -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a
viewComponent toAttributes type = leafEditorToEditor leafEditor
where
	leafEditor = {LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}

	genUI attr dp mode vst = case editModeValue mode of
		Just val = (Ok (uia type ('DM'.union attr $ toAttributes val), val),                vst)
		_        = (Error "View components cannot be used in enter mode", vst)

	onEdit _ (_, ()) _ vst = (Error "Edit event for view component",vst)

	onRefresh dp new val vst = (Ok (changes, new), vst)
	where
        changes = case setChanges ++ delChanges of
			[]      = NoChange
			changes = ChangeUI changes []

		setChanges = [ SetAttribute key val
		             \\ (key, val) <- 'DM'.toList $ toAttributes new
		             | 'DM'.get key oldAttrs <> Just val
		             ]
		delChanges = [DelAttribute key \\ (key, _) <- 'DM'.toList $ 'DM'.difference oldAttrs newAttrs]

		oldAttrs = toAttributes val
		newAttrs = toAttributes new

	valueFromState val = Just val

//Choice components that have a set of options
choiceComponent :: !(a -> UIAttributes) !(a -> [o]) !(o -> JSONNode) !([o] Int -> Bool) !UIType -> Editor (!a, ![Int])
                 | JSONEncode{|*|}, JSONDecode{|*|} a
choiceComponent attr getOptions toOption checkBounds type = disableOnView $
	leafEditorToEditor {LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attrs dp mode vst=:{VSt|taskId}
		# (mbVal, sel) = maybe (Nothing, []) (appFst Just) $ editModeValue mode
		# attr = 'DM'.unions [attrs, maybe 'DM'.newMap attr mbVal, choiceAttrs taskId (editorId dp) sel $ mbValToOptions mbVal]

		# multiple = maybe False (\(JSONBool b) -> b) ('DM'.get "multiple" attr)
		= (Ok (uia type attr, (mbVal, sel, multiple)), vst)

	onEdit dp (tp, selection) (mbVal, sel, multiple) vst=:{VSt|optional}
		# options = maybe [] getOptions mbVal
		| all (checkBounds options) selection
			= (Ok (NoChange, (mbVal, selection, multiple)),vst)
		| otherwise
			= (Error ("Choice event out of bounds: " +++ toString (toJSON selection)), vst)

	onRefresh dp (newVal, newSel) (mbOldVal, oldSel, multiple) vst
		//Check options
		# oldOpts            = mbValToOptions mbOldVal
		# newOpts            = mbValToOptions $ Just newVal
		# cOptions           = if (newOpts =!= oldOpts)
		                          (ChangeUI [SetAttribute "options" (JSONArray newOpts)] [])
		                          NoChange
		//Check selection
		# cSel               = if (newSel =!= oldSel) (ChangeUI [SetAttribute "value" (toJSON newSel)] []) NoChange
		= (Ok (mergeUIChanges cOptions cSel, (Just newVal, newSel, multiple)),vst)

	valueFromState (Just val, sel, multiple)
		//The selection is only allowed to be empty when multiselect is enabled
		| not multiple && isEmpty sel = Nothing
		| otherwise                   = Just (val, sel)
	valueFromState _               = Nothing

	mbValToOptions mbVal = toOption <$> maybe [] getOptions mbVal
