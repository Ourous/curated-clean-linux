implementation module iTasks.UI.Editor.Modifiers

from StdFunc import o, const, flip, id
import StdBool, StdString, StdList
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.WF.Combinators.Tune
import Data.Error, Text.GenJSON, Data.Tuple, Data.Functor, Data.Maybe
import Data.GenEq, Data.Func
import qualified Data.Map as DM

instance tune UIAttributes Editor
where
	tune extra editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
	where
		genUI dp mode vst=:{VSt|taskId,optional} = case editorGenUI dp (mapEditMode id mode) vst of
			(Ok (UI type attr items, st),vst) = (Ok (UI type ('DM'.union attr extra) items, st),vst)
			(e,vst)                           = (e,vst)

withEditModeAttr :: !(Editor a) -> Editor a
withEditModeAttr editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
where
	genUI dp mode vst=:{VSt|taskId} = case editorGenUI dp (mapEditMode id mode) vst of
		(Ok (UI type attr items,mask),vst) = (Ok (UI type ('DM'.put "mode" (JSONString (modeString mode)) attr) items, mask),vst)
		(e,vst) = (e,vst)
	where
		modeString Enter      = "enter"
		modeString (Update _) = "update"
		modeString (View _)   = "view"

withDynamicHintAttributes :: !String !(Editor a) -> Editor a
withDynamicHintAttributes typeDesc editor=:{Editor|genUI=editorGenUI,onEdit=editorOnEdit,onRefresh=editorOnRefresh, valueFromState}
	= {Editor| editor & genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp mode vst=:{VSt|taskId,optional} = case editorGenUI dp (mapEditMode id mode) vst of
		(Ok (UI type attr items,mask),vst)
			//Add hint attributes
			# attr = 'DM'.union (stdAttributes typeDesc (isJust $ valueFromState mask) optional mask) attr
			= (Ok (UI type attr items,mask),vst)
		(e,vst) = (e,vst)

	onEdit dp e omask vst=:{VSt|optional} = addHintAttrChanges omask (editorOnEdit dp e omask vst)
	onRefresh dp e omask vst=:{VSt|optional} = addHintAttrChanges omask (editorOnRefresh dp e omask vst)

	addHintAttrChanges omask (Ok (change,nmask),vst=:{VSt|optional})
		# attrChange = case stdAttributeChanges typeDesc optional omask (isJust $ valueFromState nmask) nmask of
			[] = NoChange
			cs = ChangeUI cs []
		# change = mergeUIChanges change attrChange
		= (Ok (change,nmask),vst)
	addHintAttrChanges omask (e,vst) = (e,vst)

/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: !String !Bool !Bool !EditState -> UIAttributes
stdAttributes typename valid optional mask
	| valid
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_VALID)
						,(HINT_ATTRIBUTE, JSONString ("You have correctly entered a " +++ typename))]
	| otherwise
		| not $ isTouched mask = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INFO)
			, ( HINT_ATTRIBUTE
			  , JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)")
			  )
			]
		| isCompound mask = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INVALID)
			, (HINT_ATTRIBUTE, JSONString ("You need to enter a "+++ typename +++ " (this value is required)"))
			]
		| otherwise = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INVALID)
			, (HINT_ATTRIBUTE, JSONString ("This value not in the required format of a " +++ typename))
			]

stdAttributeChanges :: !String !Bool !EditState !Bool !EditState -> [UIAttributeChange]
stdAttributeChanges typename optional om nvalid nm
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename nvalid optional nm)]

:: StoredMode = StoredEnter | StoredUpdate | StoredView

derive JSONEncode StoredMode
derive JSONDecode StoredMode

selectByMode :: !(Editor a) !(Editor a) !(Editor a) -> Editor a
selectByMode
		viewEditor  =:{Editor|genUI=viewGenUI,  onEdit=viewOnEdit,  onRefresh=viewOnRefresh,   valueFromState=viewValueFromState}
		enterEditor =:{Editor|genUI=enterGenUI, onEdit=enterOnEdit, onRefresh=enterOnRefresh,  valueFromState=enterValueFromState}
		updateEditor=:{Editor|genUI=updateGenUI,onEdit=updateOnEdit,onRefresh=updateOnRefresh, valueFromState=updateValueFromState}
	= editorModifierWithStateToEditor
		{EditorModifierWithState|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst = attachMode storedMode $ case mode of
		View _   = viewGenUI   dp mode` vst
		Enter    = enterGenUI  dp mode` vst
		Update _ = updateGenUI dp mode` vst
	where
		mode` = mapEditMode id mode

		storedMode = case mode of
			Enter    = StoredEnter
			Update _ = StoredUpdate
			View _   = StoredView

	onEdit dp e mode st vst = attachMode mode $ case mode of
		StoredView   = viewOnEdit dp e st vst
		StoredEnter  = enterOnEdit dp e st vst
		StoredUpdate = updateOnEdit dp e st vst

	onRefresh dp new mode st vst = attachMode mode $ case mode of
		StoredView   = viewOnRefresh dp new st vst
		StoredEnter  = enterOnRefresh dp new st vst
		StoredUpdate = updateOnRefresh dp new st vst

	valueFromState mode st = case mode of
		StoredView   = viewValueFromState st
		StoredEnter  = enterValueFromState st
		StoredUpdate = updateValueFromState st

	attachMode mode (res, vst) = ((\(x, st) -> (x, mode, st)) <$> res, vst)

withChangedEditMode :: !((EditMode a) -> EditMode a) !(Editor a) -> Editor a
withChangedEditMode toNewMode editor=:{Editor| genUI=editorGenUI} = {Editor| editor & genUI = genUI}
where
	genUI dp mode vst = editorGenUI dp (mapEditMode id $ toNewMode mode) vst

viewConstantValue :: !a !(Editor a) -> Editor ()
viewConstantValue val e = bijectEditorValue (const val) (const ()) $ withChangedEditMode (const $ View val) e

bijectEditorValue :: !(a -> b) !(b -> a) !(Editor b) -> Editor a
bijectEditorValue tof fromf editor=:{Editor|genUI=editorGenUI,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor| editor & genUI=genUI,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst       = editorGenUI dp (mapEditMode tof mode) vst
	onRefresh dp new st vst = editorOnRefresh dp (tof new) st vst

	valueFromState st = case editorValueFromState st of
		Just val = Just $ fromf val
		_        = Nothing

injectEditorValue :: !(a -> b) !(b -> MaybeErrorString a) !(Editor b) -> Editor a
injectEditorValue tof fromf {Editor|genUI=editorGenUI,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst        = editorGenUI dp (mapEditMode tof mode) vst
	onEdit dp e st vst       = mbMapFromF $ editorOnEdit dp e st vst
	onRefresh dp newa st vst = mbMapFromF $ editorOnRefresh dp (tof newa) st vst

	valueFromState st = case editorValueFromState st of
		Just valb = case fromf valb of
			Ok vala = Just vala
			_       = Nothing
		_ = Nothing

	// TODO: store and clear error
    mbMapFromF (Error e, vst) = (Error e, vst)
	mbMapFromF (Ok (change, st), vst) = case editorValueFromState st of
		Just newb = case fromf newb of
			Ok _ = (Ok (change, st), vst)
			Error e
				# attrChange = ChangeUI [SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_INVALID)
					,SetAttribute HINT_ATTRIBUTE (JSONString e)] []
				= (Ok (mergeUIChanges change attrChange, st), vst)
		_ = (Ok (change, st), vst)

surjectEditorValue :: !(a -> b) !(b (Maybe a) -> a) !(Editor b) -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a
surjectEditorValue tof fromf {Editor|genUI=editorGenUI,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState} = editorModifierWithStateToEditor
	{EditorModifierWithState|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst = case editorGenUI dp (mapEditMode tof mode) vst of
		(Error e,vst)     = (Error e,vst)
		//Track value of the 'outer' editor
		(Ok (ui, st),vst) = (Ok (ui, editModeValue mode, st), vst)

	onEdit dp e mbOldA st vst = case editorOnEdit dp e st vst of
		(Error e,        vst) = (Error e, vst)
		(Ok (change, st),vst) = (Ok (change, updatedState mbOldA st, st), vst)

	onRefresh dp newA _ st vst = case editorOnRefresh dp (tof newA) st vst of
		(Error e,         vst) = (Error e, vst)
		(Ok (change, st), vst) = (Ok (change, updatedState (Just newA) st, st), vst)

	// only give value if inner editor is in valid state
	valueFromState val innerSt | isJust $ editorValueFromState innerSt = val
	valueFromState _   _                                               = Nothing

	updatedState mbOldA innerSt = maybe mbOldA (\newB -> Just $ fromf newB mbOldA) mbNewB
	where
		mbNewB = editorValueFromState innerSt

comapEditorValue :: !(b -> a) !(Editor a) -> Editor b | JSONEncode{|*|}, JSONDecode{|*|} b
comapEditorValue tof {Editor|genUI=editorGenUI,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= editorModifierWithStateToEditor
		{EditorModifierWithState|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst          = appFst (fmap (\(ui, st) -> (ui, editModeValue mode, st))) $
	                             editorGenUI dp (mapEditMode tof mode) vst
	onEdit dp _ mbB st vst     = (Ok (NoChange, mbB, st), vst) //Ignore edits
	onRefresh dp newB _ st vst = appFst (fmap (\(ui, st) -> (ui, Just newB, st))) $
	                             editorOnRefresh dp (tof newB) st vst
	valueFromState mbB _       = mbB
