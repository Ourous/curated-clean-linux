implementation module iTasks.UI.Editor.Common

import StdBool, StdEnum, StdOrdList, StdList, Data.Maybe, StdList, StdString, StdFunc
import Text.GenJSON, Data.GenEq, Data.List

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Editor.Containers, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Data.Tuple, Data.Error, Text, Text.GenJSON, Data.Func, Data.Functor
import qualified Data.Map as DM

emptyEditor :: Editor a | JSONEncode{|*|}, JSONDecode{|*|} a
emptyEditor = leafEditorToEditor {LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	// store initial value in state
	genUI _ mode vst           = (Ok (ui UIEmpty, editModeValue mode),vst)
	onEdit _ (_, ()) mbVal vst = (Ok (NoChange, mbVal),vst)   // ignore edit events
	onRefresh _ val _ vst      = (Ok (NoChange, Just val),vst)   // just use new value
	valueFromState mbVal       = mbVal

emptyEditorWithDefaultInEnterMode :: !a -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a
emptyEditorWithDefaultInEnterMode defaultValue = emptyEditorWithDefaultInEnterMode_ JSONEncode{|*|} JSONDecode{|*|} defaultValue

emptyEditorWithDefaultInEnterMode_ :: !(Bool a -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe a, ![JSONNode])) !a -> Editor a
emptyEditorWithDefaultInEnterMode_ jsonEncode jsonDecode defaultValue = leafEditorToEditor_
	jsonEncode jsonDecode
	{LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	// store initial value in state
	genUI _ mode vst         = (Ok (ui UIEmpty, fromMaybe defaultValue $ editModeValue mode),vst)
	onEdit _ (_, ()) val vst = (Ok (NoChange, val),vst)   // ignore edit events
	onRefresh _ val _ vst    = (Ok (NoChange, val),vst)   // just use new value
	valueFromState val       = Just val

emptyEditorWithErrorInEnterMode :: !String -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a
emptyEditorWithErrorInEnterMode error = emptyEditorWithErrorInEnterMode_ JSONEncode{|*|} JSONDecode{|*|} error

emptyEditorWithErrorInEnterMode_ :: !(Bool a -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe a, ![JSONNode])) !String
                                 -> Editor a
emptyEditorWithErrorInEnterMode_ jsonEncode jsonDecode error = leafEditorToEditor_ jsonEncode jsonDecode
	{LeafEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	// store initial value in state
	genUI _ mode vst         = case editModeValue mode of
        Nothing  = (Error error, vst)
		Just val = (Ok (ui UIEmpty, val),vst)
	onEdit _ (_, ()) val vst = (Ok (NoChange, val),vst)   // ignore edit events
	onRefresh _ val _ vst    = (Ok (NoChange, val),vst)   // just use new value
	valueFromState val       = Just val

diffChildren :: ![a] ![a] !(a -> UI) -> [(!Int, !UIChildChange)] | gEq{|*|} a
diffChildren old new toUI = diffChildren` 0 old new
where
    // only children from old list are left -> remove them all
    diffChildren` idx old [] = removeRemaining idx old
    // only new children are left -> insert them all
    diffChildren` idx [] new = addNew idx new
    diffChildren` idx [nextOld : old] [nextNew : new]
        // children are equal -> no change required
        | nextOld === nextNew = diffChildren` (inc idx) old new
        // old item cannot be reused, as it does not occur in remaining new children -> remove it
        | not (isMemberGen nextOld new) = [(idx, RemoveChild) : diffChildren` idx old [nextNew : new]]
        | otherwise
            # (change, old`) = moveFromOldOrInsert (inc idx) old
            = [change : diffChildren` (inc idx) [nextOld : old`] new]
    where
        // next new child not found in old children list -> insert it
        moveFromOldOrInsert _ [] = ((idx, InsertChild (toUI nextNew)), [])
        moveFromOldOrInsert idxOld [nextOld : oldRest]
            // next new child found in old children list -> reuse it, i.e. move it to new index
            | nextNew === nextOld = ((idxOld, MoveChild idx), oldRest)
            // look for child to reuse in remaining old children elements
            | otherwise           = appSnd (\old` -> [nextOld : old`]) (moveFromOldOrInsert (inc idxOld) oldRest)

    removeRemaining idx rem = [(idx, RemoveChild) \\ _ <- rem]
    addNew          idx new = [(i, InsertChild (toUI x)) \\ i <- [idx..] & x <- new]

chooseWithDropdown :: [String] -> Editor Int
chooseWithDropdown labels = bijectEditorValue (\i -> [i]) selection
                            (withConstantChoices options dropdown <<@ multipleAttr False)
where
	selection [x] = x
	selection _   = 0

	options = [{ChoiceText|id=i,text=t} \\ t <- labels & i <- [0..]]

listEditor :: (Maybe ([Maybe a] -> Maybe a)) Bool Bool (Maybe ([Maybe a] -> String)) (Editor a) -> Editor [a]
            | JSONEncode{|*|} a
listEditor add remove reorder count itemEditor = listEditor_ JSONEncode{|*|} add remove reorder count itemEditor

listEditor_ :: (Bool a -> [JSONNode]) (Maybe ([Maybe a] -> Maybe a)) Bool Bool (Maybe ([Maybe a] -> String)) (Editor a)
            -> Editor [a]
listEditor_ jsonenc add remove reorder count itemEditor = compoundEditorToEditor
	{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst=:{VSt|taskId} = case genChildUIs dp 0 val [] vst of
		(Ok (items, childSts),vst)
			//Add list structure editing buttons
			# items = if (not viewMode && (remove || reorder)) [listItemUI taskId dp (length val) idx idx dx \\ dx <- items & idx <- [0..]] items
			//Add the add button
			# items = if (not viewMode && add =: Just _) (items ++ [addItemControl val]) items
			//All item UI's have a unique id that is used in the data-paths of that UI
			= (Ok (uic UIList items, (viewMode, indexList val), childSts), vst)
		(Error e,vst)  = (Error e,vst)
	where
		viewMode = mode =: View _
        val = fromMaybe [] $ editModeValue mode

		genChildUIs dp _ [] us vst = (Ok (unzip (reverse us)), vst)
		genChildUIs dp i [c:cs] us vst = case itemEditor.Editor.genUI (dp++[i]) (if viewMode View Update $ c) vst of
			(Ok (u,m),vst) = genChildUIs dp (i+1) cs [(u,m):us] vst
			(Error e,vst)  = (Error e,vst)

		addItemControl val
			# val       = Just <$> val
			# counter  	= maybe [] (\f -> [uia UITextView ('DM'.unions [widthAttr FlexSize, valueAttr (JSONString (f val))])]) count
			# button	= if (isJust add) [uia UIButton ('DM'.unions [iconClsAttr "icon-add",editAttrs taskId (editorId dp) (Just (JSONString "add"))])] []
			# attr      = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIToolBar attr (counter ++ button)

	listItemUI taskId dp numItems idx id item
		# buttons	= (if reorder
			[uia UIButton ('DM'.unions [iconClsAttr "icon-up", enabledAttr (idx <> 0), editAttrs taskId (editorId dp) (Just (JSONString ("mup_" +++ toString id)))])
							  ,uia UIButton ('DM'.unions [iconClsAttr "icon-down", enabledAttr (idx <> numItems - 1), editAttrs taskId (editorId dp) (Just (JSONString ("mdn_" +++ toString id)))])
							  ] []) ++
							  (if remove
							  [uia UIButton ('DM'.unions [iconClsAttr "icon-remove",editAttrs taskId (editorId dp) (Just (JSONString ("rem_" +++ toString id)))])
							  ] [])
		# attr = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
		= uiac UIListItem attr (if (reorder || remove) ([flexWidth item] ++ buttons) [flexWidth item])
	where
		flexWidth (UI type attr content) = UI type ('DM'.union (widthAttr FlexSize) attr) content

	//Structural edits on the list
	onEdit dp ([],JSONString e) (viewMode, ids) childSts vst=:{VSt|taskId}
		# [op,id:_] = split "_" e
		# id = toInt id 
		# index = itemIndex id ids
		# num = length childSts
		| op == "mup" && reorder
			| index < 1 || index >= num = (Error "List move-up out of bounds",vst)
				# changes =  if (index == 1) [(index,toggle 1 False),(index - 1,toggle 1 True)] [] //Update 'move-up' buttons
						  ++ if (index == num - 1) [(index,toggle 2 True),(index - 1,toggle 2 False)] [] //Update 'move-down' buttons
						  ++ [(index,MoveChild (index - 1))] //Actually move the item
				= (Ok (ChangeUI [] changes, (viewMode, swap ids index), swap childSts index), vst)
		| op == "mdn" && reorder
			| index < 0 || index > (num - 2) = (Error "List move-down out of bounds",vst)
				# changes =  if (index == 0) [(index,toggle 1 True),(index + 1,toggle 1 False)] [] //Update 'move-up' buttons
                          ++ if (index == num - 2) [(index,toggle 2 False),(index + 1,toggle 2 True)] [] //Update 'move-down' buttons
                          ++ [(index,MoveChild (index + 1))]
			    = (Ok (ChangeUI [] changes, (viewMode, swap ids (index + 1)), swap childSts (index + 1)), vst)
		| op == "rem" && remove
			| index < 0 || index >= num = (Error "List remove out of bounds",vst)
				# childSts   = removeAt index childSts
				# internalSt = (viewMode, removeAt index ids)
				# nitems = itemEditor.Editor.valueFromState <$> childSts
				# counter = maybe [] (\f -> [(length nitems, ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONString (f nitems))] []))]))]) count
				# changes =  if (index == 0 && num > 1) [(index + 1, toggle 1 False)] []
						  ++ if (index == num - 1 && index > 0) [(index - 1, toggle 2 False)] []
						  ++ [(index,RemoveChild)] ++ counter
			= (Ok (ChangeUI [] changes, internalSt, childSts), vst)
		| op == "add" && add =: (Just _)
			# f = fromJust add
			# items = itemEditor.Editor.valueFromState <$> childSts
			# mbNx = f items
			# ni = num 
			# nid = nextId ids
            // use enter mode if no value for new item is given; otherwise use update mode
			= case itemEditor.Editor.genUI (dp++[nid]) (maybe Enter Update mbNx) vst of
				(Error e,vst) = (Error e, vst)
				(Ok (ui,nm),vst)
					# nChildSts = childSts ++ [nm]
					# nitems = itemEditor.Editor.valueFromState <$> childSts
					# nids = ids ++ [nid]
					# insert = [(ni,InsertChild (listItemUI taskId dp (ni + 1) ni nid ui))]
					# counter = maybe [] (\f -> [(ni + 1, ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONString (f nitems))] []))]))]) count
					# prevdown = if (ni > 0) [(ni - 1,toggle 2 True)] []
					# change = ChangeUI [] (insert ++ counter ++ prevdown)
					= (Ok (change, (viewMode, nids), nChildSts), vst)
		= (Ok (NoChange, (viewMode, ids), childSts), vst)
	where
		swap []	  _		= []
		swap list index
			| index == 0 			= list //prevent move first element up
			| index >= length list 	= list //prevent move last element down
			| otherwise				
				# f = list !! (index-1)
				# l = list !! (index)
				= updateAt (index-1) l (updateAt index f list)
		toggle idx value = ChangeChild (ChangeUI [] [(idx,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool value)] []))])
	//Edits inside the list
	onEdit dp ([id:tp],e) (viewMode, ids) childSts vst
		# index = itemIndex id ids
		| index < 0 || index >= length childSts = (Error ("List edit out of bounds (index:" +++ toString index +++", list length: "+++toString (length childSts)+++")"),vst)
		| otherwise
			= case itemEditor.Editor.onEdit (dp ++ [id]) (tp,e) (childSts !! index) vst of
				(Error e,vst)
					= (Error e, vst)
				(Ok (change,nm),vst)
					= (Ok (childChange index change, (viewMode, ids), updateAt index nm childSts), vst)
	where
		childChange i NoChange = NoChange
		childChange i change = ChangeUI [] [(i,ChangeChild (ChangeUI [] [(0,ChangeChild change)]))]

	//Very crude full replacement
	onRefresh dp new (viewMode, ids) childSts vst
		| Just ((JSONEncode{|*->*|} jsonenc) False new) ===
		       ((JSONEncode{|*->*|} jsonenc) False <$> valueFromState (viewMode, ids) childSts)
			= (Ok (NoChange, (viewMode, ids), childSts), vst)
		//TODO: Determine small UI change
		| otherwise
			= case genUI dp (if viewMode View Update $ new) vst of
				(Ok (ui, internalSt, childSts),vst) = (Ok (ReplaceUI ui, internalSt, childSts), vst)
				(Error e,vst)                       = (Error e, vst)

    valueFromState _ childSts = valuesFromState childSts []
	where
		valuesFromState [] acc = Just $ reverse acc
		valuesFromState [st: sts] acc = case itemEditor.Editor.valueFromState st of
			Just val = valuesFromState sts [val: acc]
			_        = Nothing

	nextId [] = 0
	nextId ids = maxList ids + 1

	itemIndex id ids = itemIndex` 0 id ids
	where
		itemIndex` _ _ [] = -1
		itemIndex` i id [x:xs] = if (id == x) i (itemIndex` (i + 1) id xs)
