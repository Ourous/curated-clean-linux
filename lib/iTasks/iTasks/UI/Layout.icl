implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc, StdString
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil, iTasks.UI.Definition
import iTasks.Internal.Generic.Defaults 
import StdEnum
from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, alter, union, keys, unions, singleton
from Data.Tuple import appSnd
import Text.GenJSON

from StdFunc import o, const, id, flip
from iTasks.Internal.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
from iTasks.Internal.TaskEval import :: TaskTime
from iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.WF.Definition
import Data.GenEq

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LayoutState, LayoutTree, MvUI, MvUIChild
derive JSONDecode LayoutState, LayoutTree, MvUI, MvUIChild

//Test if a specific UI at a path is in the selection
inUISelection :: UISelection UIPath UI -> Bool
inUISelection (SelectByPath p) path _ = p === path
inUISelection (SelectByDepth n) p _ = length p == n
inUISelection (SelectDescendents) [_:_] _ = True
inUISelection (SelectDescendents) _ _ = False
inUISelection (SelectByType t) _ (UI type _ _) = t === type
inUISelection (SelectByHasAttribute k) _ (UI _ attr _) = isJust ('DM'.get k attr)
inUISelection (SelectByAttribute k p) _ (UI _ attr _) = maybe False p ('DM'.get k attr)
inUISelection (SelectByNumChildren num) _ (UI _ _  items) = length items == num
inUISelection (SelectByContains selection) path ui=:(UI _ _ items)
	| inUISelection selection path ui = True 
			  						  = or [inUISelection (SelectByContains selection) (path ++ [i]) item \\ item <- items & i <- [0..]]
inUISelection (SelectRelative prefix sel) absolutePath ui 
	= maybe False (\relativePath -> inUISelection sel relativePath ui) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing
inUISelection (SelectNone) _ _ = False
inUISelection (SelectAND sell selr) path ui = inUISelection sell path ui && inUISelection selr path ui 
inUISelection (SelectOR sell selr) path ui = inUISelection sell path ui || inUISelection selr path ui 
inUISelection (SelectNOT sel) path ui = not (inUISelection sel path ui)

inUISelectionAfterChange :: UISelection UIPath UI UIChange -> Bool
inUISelectionAfterChange selection path ui change //TODO: This needs a more efficient implemenation that does not apply the full change if it is not necessary
	= inUISelection selection path (applyUIChange change ui)

//A layout that has no effect at all
idLayout :: Layout 
idLayout = {Layout|apply=const (NoChange,LSNone),adjust=id,restore=const NoChange}

setUIType :: UIType -> Layout
setUIType type = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui=:(UI _ attr items) = (ReplaceUI (UI type attr items), LSType ui) //Crude replacement (no instruction possible)

	adjust (NoChange,s)   = (NoChange,s)
	adjust (ReplaceUI ui,_) = apply ui 
	adjust (change,LSType ui) = (change, LSType (applyUIChange change ui))

	//Crude restore...
	restore (LSType ui) = ReplaceUI ui 

setUITypeRef_ :: UIType -> Layout
setUITypeRef_ type = referenceLayout ref
where 
	ref (UI _ attr items) = UI type attr items

setUIAttributes :: UIAttributes -> Layout
setUIAttributes extraAttr = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply (UI _ attr _)
		= (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList extraAttr] [],LSAttributes attr)

	adjust (ChangeUI attrChanges itemChanges,LSAttributes attr)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Filter out updates for the attributes that this layout has overwritten setting here
		# attrChanges = filter (not o matchChange) attrChanges
		= (ChangeUI attrChanges itemChanges, LSAttributes attr)
	where
		matchChange (SetAttribute k _) = isMember k ('DM'.keys extraAttr)
		matchChange (DelAttribute k) = isMember k ('DM'.keys extraAttr)

	adjust (ReplaceUI ui,LSAttributes attr)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)
	adjust (change,s) = (change,s)

	restore (LSAttributes attr) //Restore or delete the extra attributes
		= ChangeUI [maybe (DelAttribute k) (\v -> SetAttribute k v) ('DM'.get k attr) \\ k <- 'DM'.keys extraAttr] []

setUIAttributesRef_ :: UIAttributes -> Layout
setUIAttributesRef_ extraAttr = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union extraAttr attr) items

delUIAttributes :: UIAttributeSelection -> Layout 
delUIAttributes selection = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	apply (UI _ attr _) 
		= (ChangeUI [DelAttribute k \\ k <- 'DM'.keys attr | matchKey selection k] [],LSAttributes attr)

	adjust (ChangeUI attrChanges itemChanges,LSAttributes attr)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Remove changes that affect the deleted attributes
		# attrChanges = filter (not o (matchChange selection)) attrChanges
		= (ChangeUI attrChanges itemChanges,LSAttributes attr)

	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)

	adjust (change,s) = (change,s)
	
	restore (LSAttributes attr) //Restore the attributes
		= ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList attr | matchKey selection k] []

delUIAttributesRef_ :: UIAttributeSelection -> Layout 
delUIAttributesRef_ selection = referenceLayout ref
where
	ref (UI type attr items) = UI type (foldl (\a k -> if (matchKey selection k) ('DM'.del k a) a) attr ('DM'.keys attr)) items

modifyUIAttributes :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributes selection modifier = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply (UI type attr items)
		# mods = modifier (selectAttributes selection attr)
		= (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList mods] [],LSModifyAttributes attr mods)

	adjust (ChangeUI attrChanges childChanges, LSModifyAttributes attr mods)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Recompute the modifications
		# newMods = modifier (selectAttributes selection attr)
		# modChanges = diffAttributes mods newMods
		//Remove changes that affect the modified attributes
		# attrChanges = filter (not o (matchMods ('DM'.keys newMods))) attrChanges
		= (ChangeUI (attrChanges ++ modChanges) childChanges, LSModifyAttributes attr newMods)
	where
		matchMods modKeys (SetAttribute k _) = isMember k modKeys
		matchMods modKeys (DelAttribute k)   = isMember k modKeys
	
	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)
	adjust (change,s) = (change,s)

	restore (LSModifyAttributes attr mods) //Restore the attributes
		= ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList attr] []

modifyUIAttributesRef_ :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributesRef_ selection modifier = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union (modifier selected) attr) items
	where
		selected = selectAttributes selection attr

//Known use:
//-	copySubUIAttributes SelectAll [0] [] 
//- copySubUIAttributes (SelectKeys ["title"]) [0] []	
//- copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
//- copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [1] [2]
//- copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [1] [1]
copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> Layout
copySubUIAttributes selection src dst = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	apply ui 
		# srcAttr = getAttrAtPath src ui //Find the attributes in the source
		# change  = changeAtPath dst ui (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList srcAttr | matchKey selection k] [])
		= (change, LSCopyAttributes ui)

	//THIS IS A TEMPORARY VERSION:
	//It is incorrect in general, but works for the current concrete use-cases we have in the default layouts
	adjust (change, LSCopyAttributes ui)
		//Update the ui
		# (copyChange,state) = apply (applyUIChange change ui)
		= (mergeUIChanges change copyChange, state)
			
	restore (LSCopyAttributes ui) = ReplaceUI ui

	//Find the attributes of a sub ui
	getAttrAtPath [] (UI type attr items) = attr
	getAttrAtPath [s:ss] (UI type attr items) 
		| s >= 0 && s < length items = getAttrAtPath ss (items !! s)
							         = 'DM'.newMap

	changeAtPath [] _ change = change
	changeAtPath [s:ss] (UI _ _ items) change
		| s >= 0 && s < length items = ChangeUI [] [(s,ChangeChild (changeAtPath ss (items !! s) change))]
									 = NoChange


/*
	adjust (change, LSCopyAttributes ui)
		//Check if the change affects the position of the destination
		# dstAfterChange = pathAfterChange dst change
		# ui = applyUIChange change ui
		//Apply the change to the ui
		= case dstAfterChange of
			Nothing  //The old destination node no longer exist, we have to copy all attributes again
				# change  = changeAtPath dst ui (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList (getAttrAtPath src ui) | matchKey selection k] [])
				= (change, LSCopyAttributes ui)
			Just (dstAfterChange)
				| dstAfterChange === dst //Destination is still in the same place -> only look at changes in the source
					# srcChanges = getAttrChangesAtPath src change
					# change = changeAtPath dst ui (ChangeUI [c \\ c <- srcChanges | matchChange selection c] [])
					= (change,LSCopyAttributes ui)
						
				| otherwise //The old destination is no longer the destination -> restore old node and insert in new place
					# correctionChange = changeAtPath dstAfterChange ui 
						(ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList (getAttrAtPath dstAfterChange ui) | matchKey selection k] [])
					# dstChange = changeAtPath dst ui
						(ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList (getAttrAtPath src ui) | matchKey selection k] [])
					= (mergeUIChanges correctionChange dstChange, LSCopyAttributes ui)

	restore (LSCopyAttributes ui) = ReplaceUI ui

	//Find the attributes of a sub ui
	getAttrAtPath [] (UI type attr items) = attr
	getAttrAtPath [s:ss] (UI type attr items) 
		| s >= 0 && s < length items = getAttrAtPath ss (items !! s)
							         = 'DM'.newMap

	changeAtPath [] _ change = change
	changeAtPath [s:ss] (UI _ _ items) change
		| s >= 0 && s < length items = ChangeUI [] [(s,ChangeChild (changeAtPath ss (items !! s) change))]
									 = NoChange

	//Determine where a node addressed by a path ends up after a change is applied
	pathAfterChange path NoChange = Just path //Nothing happened
	pathAfterChange path (ReplaceUI _) = Nothing //The ui indicated by the path longer exists
	pathAfterChange [] (ChangeUI _ _) = Just []
	pathAfterChange [s:ss] (ChangeUI _ childChanges) = adjust s ss childChanges
	where
		adjust s ss [] = Just [s:ss]
		adjust s ss [(i,ChangeChild c):cs]
			| i == s = case pathAfterChange ss c of 
				(Just ss) = adjust s ss cs
				Nothing = Nothing
			| otherwise
				= adjust s ss cs
		adjust s ss [(i,InsertChild _):cs]
			= if (i <= s) (adjust (s + 1) ss cs) (adjust s ss cs)
		adjust s ss [(i,RemoveChild):cs]
			| i == s = Nothing
					 = if (i < s) (adjust (s - 1) ss cs) (adjust s ss cs)
		adjust s ss [(i,MoveChild d):cs]
			| i == s = adjust d ss cs
					 = adjust (s - (if (i < s) 1 0) + (if (d <= s) 1 0)) ss cs

	getAttrChangesAtPath path change = fst (get path change) //FIXME: ChangeUI  n, Remove (n - 1), ChangeUI n
	where
		get path NoChange = ([],Just path)
		get path (ReplaceUI ui) = ([SetAttribute k v \\ (k,v) <- 'DM'.toList (getAttrAtPath path ui)],Just path)
		get []    (ChangeUI attrChanges _) = (attrChanges,Just [])
	    get [s:ss] (ChangeUI _ childChanges) = adjust s ss childChanges

		adjust s ss [] = ([],Just [s:ss])
		adjust s ss [(i,ChangeChild c):cs]
			| i == s = case get ss c of 
				(as,Just ss) = adjust s ss cs
				(_,Nothing) = ([],Nothing)
			         = adjust s ss cs
		adjust s ss [(i,InsertChild _):cs]
			= if (i <= s) (adjust (s + 1) ss cs) (adjust s ss cs)
		adjust s ss [(i,RemoveChild):cs]
			| i == s = ([],Nothing)
					 = if (i < s) (adjust (s - 1) ss cs) (adjust s ss cs)
		adjust s ss [(i,MoveChild d):cs]
			| i == s = adjust d ss cs
					 = adjust (s - (if (i < s) 1 0) + (if (d <= s) 1 0)) ss cs
*/
matchKey (SelectAll) _ = True
matchKey (SelectKeys keys) k = isMember k keys

matchChange selection (SetAttribute k _) = matchKey selection k
matchChange selection (DelAttribute k) = matchKey selection k 

selectAttributes SelectAll attr = attr
selectAttributes (SelectKeys keys) attr = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

copySubUIAttributesRef_ :: UIAttributeSelection UIPath UIPath -> Layout
copySubUIAttributesRef_ selection src dst = referenceLayout ref
where
	ref ui = updDst dst (selAttr src ui) ui

	selAttr [] (UI _ attr _) = [a \\ a=:(k,_) <- 'DM'.toList attr | matchKey selection k]
	selAttr [s:ss] (UI _ _ items) = if (s >= 0 && s < length items) (selAttr ss (items !! s)) []

	updDst [] selected (UI type attr items) = UI type (foldl (\a (k,v) -> 'DM'.put k v a) attr selected) items
	updDst [s:ss] selected ui=:(UI type attr items) = if (s >= 0 && s < length items) (UI type attr (updateAt s (updDst ss selected (items !! s)) items)) ui

//Set attributes in 'new' if they are different than, or not found in 'old'
//Remove attributes that were in old, but are no longer in new
diffAttributes old new = [SetAttribute k v \\ (k,v) <- 'DM'.toList new | maybe True (\ov -> ov <> v) ('DM'.get k old)] 
					  ++ [DelAttribute k \\ k <- 'DM'.keys old | isNothing ('DM'.get k new)]

wrapUI :: UIType -> Layout
wrapUI type = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (ReplaceUI (uic type [ui]), LSWrap ui)

	adjust (ReplaceUI ui,_) = apply ui 

	adjust (NoChange,s)   = (NoChange,s)
	adjust (change,LSWrap ui) 
		= (ChangeUI [] [(0,ChangeChild change)],LSWrap (applyUIChange change ui))

	//Crude restore...
	//As long as the UIChange type does not support moving elements up and down the tree we cannot do better
	restore (LSWrap ui) = ReplaceUI ui 

wrapUIRef_ :: UIType -> Layout
wrapUIRef_ type = referenceLayout ref
where
	ref ui = uic type [ui]

unwrapUI :: Layout
unwrapUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui=:(UI type attr [i:is]) = (ReplaceUI i, LSUnwrap ui)
	apply ui 					    = (NoChange, LSUnwrap ui)	

	adjust (NoChange,state) = (NoChange,state)
	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui), state)

	adjust (ChangeUI attrChanges childChanges, LSUnwrap	ui)
		//First update attributes
		# ui = applyUIChange (ChangeUI attrChanges []) ui
		//Process the child changes
		# (change, ui) = foldl adjust` (NoChange, ui) childChanges
		= (change, LSUnwrap ui)
	where
		adjust` (change, ui) c=:(n, ChangeChild cc)
			= (if (n == 0) (mergeUIChanges cc change) change, applyUIChange (ChangeUI [] [c]) ui)

		//When the first element (the one that was unwrapped) is removed, the first sibling is now the unwrapped element.
		//If there is no first sibling, we undo the unwrapping by replacing with the stored ui
		adjust` (change, UI type attr [_,i:is]) (0, RemoveChild) = (ReplaceUI i, UI type attr [i:is])
		adjust` (change, UI type attr [_]) (0, RemoveChild) = (ReplaceUI (UI type attr []), UI type attr [])
		adjust` (change, ui) (n, RemoveChild) = (change, applyUIChange (ChangeUI [] [(n,RemoveChild)]) ui)

		//When a new element is inserted at position 0, it should now be the shown element
		adjust` (change, ui) c=:(n, InsertChild i)
			= (if (n == 0) (ReplaceUI i) change, applyUIChange (ChangeUI [] [c]) ui)

		//When a move affects the first position, we need to update the shown element
		adjust` (change, ui) c=:(nfrom, MoveChild nto)
			# ui = applyUIChange (ChangeUI [] [c]) ui
			| nfrom == 0 || nto == 0
				= case ui of (UI _ _ [i:_]) = (ReplaceUI i, ui) ; _ = (change, ui)
			| otherwise 
				= (change, ui)


	//Crude restore...
	//As long as the UIChange type does not support moving elements up and down the tree we cannot do better
	restore (LSUnwrap ui) = ReplaceUI ui 

unwrapUIRef_ :: Layout
unwrapUIRef_ = referenceLayout ref
where
	ref (UI _ _ [ui:_]) = ui
	ref ui = ui

//Insert the element at the specified index.
//Only insert if there are at least as many children as the specified index
insertChildUI :: Int UI -> Layout
insertChildUI idx insert
	| idx >= 0  = {Layout|apply=apply,adjust=adjust,restore=restore}
				= idLayout
where
	apply (UI _ _ items) 
		# num = length items
		| idx >= 0 && idx <= num = (ChangeUI [] [(idx,InsertChild insert)],LSInsert num)
					             = (NoChange, LSInsert num)

	adjust (NoChange,state) = (NoChange,state)

	adjust (ReplaceUI ui, _) //We are replacing everything, so don't keep state
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui), state)

	//Adjust the child changes to account for the additional insert
	adjust (ChangeUI attrChanges childChanges, LSInsert num) 
		# (childChanges,num) = adjustChildChanges childChanges num
		= (ChangeUI attrChanges childChanges, LSInsert num)

	adjustChildChanges [] num = ([],num)
	adjustChildChanges [(i,ChangeChild change):cs] num
		| i >= 0 && i < num //The child is in a valid range
			//Adjust everything 'after' the inserted element
			# (cs,num) = adjustChildChanges cs num
			= ([(if (i < idx) i (i + 1),ChangeChild change):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 
	adjustChildChanges [(i,RemoveChild):cs] num
		| i >= 0 && i < num //The child is in a valid range
			//This removal means we also have to remove the inserted element
			| num == idx 
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(idx,RemoveChild),(i,RemoveChild):cs], num)
			//The element has not been inserted because there are too few children, there is no effect 
			| num < idx
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i,RemoveChild):cs], num)
			//The extra element was inserted:
			// If we remove an element 'after' the inserted index we have to offset the index of the removal
			| i >= idx
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i + 1, RemoveChild):cs], num)
			// If we remove an element 'before' the inserted element
			// it affects its position, so we need to move its sibling to adjust for that.
			| otherwise
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i,RemoveChild),(idx - 1,MoveChild idx):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 

	adjustChildChanges [(i,InsertChild ui):cs] num
		| i >= 0 && i <= num //The child is in a valid range
			//This addition means we have to insert the extra element now
			| num == (idx - 1)
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui),(idx,InsertChild insert):cs], num)
			//The element has not been inserted because (even with this insert) there are too few children, there is no effect 
			| num < (idx - 1)
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui):cs], num)
			//The extra element was inserted:
			// If we insert an element 'after' the inserted index we have to offset the index of the insert
			| i >= idx
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i + 1,InsertChild ui):cs], num)
			// If we insert an element 'before' the inserted element
			// it affects its position, so we need to move its sibling to adjust for that.
			| otherwise
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui),(idx + 1,MoveChild idx):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 
	adjustChildChanges [(s,MoveChild d):cs] num
		| s >= 0 && s < num && d >= 0 && d < num //Both source and destination are in a valid range
			//The element has not been inserted, there is no effect
			| num < idx 
				# (cs,num) = adjustChildChanges cs num 
				= ([(s,MoveChild d):cs],num)
			//Both are 'before' the inserted element, there is no effect
			| s < idx && d < idx
				# (cs,num) = adjustChildChanges cs num 
				= ([(s,MoveChild d):cs],num)
			//Only the source is 'before' the inserted element. We need to offset the destination and adjust
			| s < idx 
				# (cs,num) = adjustChildChanges cs num
				= ([(s,MoveChild (d + 1)),(idx - 1,MoveChild idx):cs],num)
			//Only the destination is 'before' the in
			| d < idx 	
				# (cs,num) = adjustChildChanges cs num
				= ([(s + 1, MoveChild d),(idx + 1,MoveChild idx):cs],num)
			//Both are 'after' the inserted element, offset the indices
			| otherwise
				# (cs,num) = adjustChildChanges cs num 
				= ([(s + 1,MoveChild (d + 1)):cs],num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 

	//Check in the state if the extra element was inserted or not
	restore (LSInsert num)
		| idx >= 0 && idx <= num = ChangeUI [] [(idx,RemoveChild)]
					             = NoChange

insertChildUIRef_ :: Int UI -> Layout
insertChildUIRef_ idx insert = referenceLayout ref
where
	ref ui=:(UI type attr items)
		| idx >= 0 && idx <= length items = UI type attr (insertAt idx insert items)
										  = ui

removeSubUIs :: UISelection -> Layout
//removeSubUIs selection = removeSubUIsRef_ selection
removeSubUIs selection = moveSubUIs` selection Nothing

removeSubUIsRef_ :: UISelection -> Layout
removeSubUIsRef_ selection = referenceLayout ref
where
	ref ui=:(UI type attr items) //Special case for the root node
	  | inUISelection selection [] ui = UI UIEmpty 'DM'.newMap []
							          = UI type attr (flatten [rem [i] x \\ x <- items & i <- [0..]])

	rem path ui=:(UI type attr items)
	  | inUISelection selection path ui = []
						      = [UI type attr (flatten [rem (path ++ [i]) x \\ x <- items & i <- [0..]])]

moveSubUIs :: UISelection UIPath Int -> Layout 
//moveSubUIs selection path pos = moveSubUIsRef_ selection path pos
moveSubUIs selection path pos = moveSubUIs` selection (Just (path,pos)) 

moveSubUIsRef_ :: UISelection UIPath Int -> Layout 
moveSubUIsRef_ selection dst pos = referenceLayout ref
where
	ref ui
		# (selected,Just ui`) = collect [] ui   //Find and remove all matching nodes
		# (dst`,pos`)   = adjust [] dst pos ui  //Adjust the path and position for the removals
		= (insert selected dst` pos` ui`)       //Insert the selected nodes at the destination

	collect path ui=:(UI type attr items)
		| not (startsWith path dst) && inUISelection selection path ui //Match
			= ([ui],Nothing)
		| otherwise //Check all children
			# (collects,items) = unzip [collect (path ++ [i]) x \\ x <- items & i <- [0..]]
			= (flatten collects, Just (UI type attr [x \\ Just x <- items]))

	startsWith [] dst = True
	startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
	startsWith _ _ = False

	adjust cur [] pos (UI _ _ items) = ([],index cur pos items)
	adjust cur [s:ss] pos (UI _ _ items)
		| s >= 0 && s < length items
			# (ss`,pos`) = adjust (cur ++ [s]) ss pos (items !! s)
			= ([index cur s items:ss`],pos`)
		| otherwise
			= ([s:ss],pos)

	index cur n items = length [x \\ x <- take n items & i <- [0..] | not (inUISelection selection (cur ++ [i]) x)]

	insert selected [] i (UI type attr items)
		| i >= 0 && i <= length items = UI type attr (take i items ++ selected  ++ drop i items)
									  = UI type attr items
	insert selected [s:ss] i (UI type attr items)
		| s >= 0 && s < length items
			= UI type attr (updateAt s (insert selected ss i (items !! s)) items)
			= UI type attr items

moveSubUIs` :: UISelection (Maybe (!UIPath,!Int)) -> Layout
moveSubUIs` selection mbDst = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		//First construct the state and determine the change that removes all matching nodes
		# (rchange, state) = case applyRem selection [] ui of
			//We can't remove the root-level UI, therefore if it matches the selection we replace it with UIEmpty
			(RemoveChild,state)         = (ReplaceUI (UI UIEmpty 'DM'.newMap []),state) 
			(ChangeChild change, state) = (change,state)
		//If a destination was specified and it exists
		# (ichange, state) = applyIns mbDst [] state
		= (mergeUIChanges rchange ichange, LSRemoveSubUIs state)

	applyRem selection path ui=:(UI type attr items)
		| not (onDestinationPath path) && inUISelection selection path ui //Match
			= (RemoveChild, {toMvUI ui & matched = True})
		| otherwise
			# (change,children) = case applyRemChildren path 0 0 items of
				([],children) = (NoChange,children)
				(changes,children) = (ChangeUI [] changes, children)
			# state = {MvUI|type=type,attr=attr,matched=False,moved=False,deleted=False,dstChange=NoChange,children=children}
			= (ChangeChild change, state)
	where
		applyRemChildren path i n [] = ([],[])
		applyRemChildren path i n [item:items]
			# (change,item) = applyRem selection (path ++ [i]) item 
			# (changes,items) = applyRemChildren path (i + 1) (if item.MvUI.matched n (n + 1)) items
			= ([(n,change):changes], [MvUIItem item:items])

	applyIns Nothing path mvui = (NoChange,mvui)
	applyIns (Just dst) path mvui
		| destinationExists dst mvui
			# (changes,mvui) = collectDestinationChanges True [mvui]
			# (change,mvui)  = applyAtDestinationPath dst changes mvui
			= (change,mvui)
		| otherwise = (NoChange,mvui)

	adjust (change, LSRemoveSubUIs mvui)
		//Adjusting changes happens in two passes 
		//In the first pass we adjust the upstream change and remove or restore items that match, or no longer match the selection
		# (rchange, mvuis) = case adjustRem selection [] change mvui of
			(ChangeChild change,mvuis) = (change,mvuis)
			(RemoveChild,mvuis)        = (ReplaceUI (UI UIEmpty 'DM'.newMap []),mvuis) //If the root-level UI needs to be removed, replace it with UIEmpty
			(InsertChild ui,mvuis)     = (ReplaceUI ui,mvuis)           			   //If the root-level UI is going to be restored, replace it
		//In the second pass we update the nodes that were moved
		# (ichange, mvui) = adjustIns mbDst [] mvuis
		= (mergeUIChanges rchange ichange, LSRemoveSubUIs mvui)

	adjustRem selection path NoChange mvui=:{MvUI|deleted,matched,moved,children} 
		| deleted = (ChangeChild NoChange,[mvui])
		//Check if the node should have been removed by the layout
		# ui = fromMvUI mvui
		| not (onDestinationPath path) && inUISelection selection path ui
			| matched   = (ChangeChild NoChange, [mvui]) //Already removed, nothing to do
			| otherwise = (RemoveChild, [{MvUI|mvui & matched = True}]) //Remove it now
		| otherwise
			| matched //It should not be removed, restore it 
				# ui = fromMvUI mvui
				# (ChangeChild change, mvui`) = applyRem selection path ui //Replace the state, it is now possible that subnodes are matched
				= (InsertChild (applyUIChange change ui), [{MvUI|mvui & deleted = True}, mvui`])
			| otherwise 
				//EXPERIMENT: Recursively check children (may not be neccesary)
				# (schanges, children) = adjustRemSiblings selection path (\x -> True) children
				# change = case schanges of
					[] = NoChange
					_  = ChangeUI [] schanges
				= (ChangeChild change, [{MvUI|mvui & children = children}]) //Wasn't moved, nothing to do

	adjustRem selection path (ReplaceUI ui) mvui=:{MvUI|matched,moved,deleted}
		= case applyRem selection path ui of
			(ChangeChild change, mvui`) //The new ui does not match the selection
				| matched //This node was previously removed, we need to insert it
					= (InsertChild (applyUIChange change ui), [{MvUI|mvui & deleted = True}, mvui`])
				| otherwise //The ui existed, so we replace it with the adjusted version
					= (ChangeChild (ReplaceUI (applyUIChange change ui)), [{MvUI|mvui & deleted = True}, mvui`])
			(RemoveChild, mvui`) //The new ui matches the selection 
				| matched //The previous ui was also removed, so we don't have to do anything here, but we may have to update the target location
					 | moved
						= (ChangeChild NoChange, [{mvui & dstChange = ReplaceUI ui}]) //We don't apply the replace, but defer it to the target location
					 | otherwise
						= (ChangeChild NoChange, [{MvUI|mvui & deleted = True}, mvui`])
				| otherwise //The previous ui was not removed, so we remove it now
					= (RemoveChild, [{MvUI|mvui & deleted = True}, mvui`])

	adjustRem selection path change=:(ChangeUI attrChanges childChanges) mvui=:{MvUI|attr,matched,moved,dstChange,children}
		//Check if the change, when applied, would make this ui match the selection
		| not (onDestinationPath path) && inUISelection selection path (applyUIChange change (fromMvUI mvui))
			//Apply the changes to the state, but ignore the downstream changes because the node is hidden
			# attr         = foldl (flip applyUIAttributeChange) attr attrChanges
			//By passing SelectNone we make sure that all moved descendents are no longer removed
			# (_,children) = adjustRemChildChanges SelectNone path childChanges children 
			//If this item was already moved before, we need to apply the changge
			# dstChange = if moved (mergeUIChanges dstChange change) dstChange
			= (if matched (ChangeChild NoChange) RemoveChild, [{MvUI|mvui & attr = attr, matched = True, dstChange = dstChange, children = children}])
		| otherwise
			//Apply the changes to the state
			# attr                    = foldl (flip applyUIAttributeChange) attr attrChanges
			# (childChanges,children) = adjustRemChildChanges selection path childChanges children
			# mvui                    = {MvUI|mvui & attr = attr, children = children}
			| matched //If this ui was previously removed, we need to restore it
				= (InsertChild (fromMvUI mvui), [mvui])
			| otherwise
				= (ChangeChild (ChangeUI attrChanges childChanges), [mvui])

	adjustRemChildChanges selection path [] items = ([], items)
	adjustRemChildChanges selection path [(i,c):cs] items
		# (c, items) = adjustRemChildChange selection path i c items 
		# (cs, items) = adjustRemChildChanges selection path cs items
		= (c ++ cs, items)

	adjustRemChildChange selection path i (ChangeChild change) items
		| i >= 0 && i < numItems items //Check bounds
			//Recursively apply change 
			# (cchange, item) = adjustRem selection (path ++ [i]) change (getItem i items)
			# items           = setItem i item items
			# change = case cchange of 
				(ChangeChild NoChange)         = []
				(ChangeChild (ChangeUI [] [])) = []	
				_                              = [(adjustIndex i items, cchange)]
			= (change, items)
		| otherwise
			= ([],items)
	adjustRemChildChange selection path i (InsertChild ui) items
		| i >= 0 && i <= numItems items 
			# (cchange,item) = applyRem selection (path ++ [i]) ui
			# change = case cchange of 
				RemoveChild = []
				ChangeChild change = [(adjustIndex i items, InsertChild (applyUIChange change ui))]
			# items = insertItem i item items
			# (schanges, items) = adjustRemSiblings selection path (\x -> x > i) items
			= (change ++ schanges, items)
		| otherwise
			= ([],items)
	
	adjustRemChildChange selection path i (RemoveChild) items
		| i >= 0 && i < numItems items 
			# item=:{MvUI|matched} = getItem i items
			# change = if matched [] [(adjustIndex i items,RemoveChild)]
			# items = setItem i [{MvUI|item & deleted = True}] items
			# (schanges, items) = adjustRemSiblings selection path (\x -> x >= i) items
			= (change ++ schanges, items)
		| otherwise
			= ([],items)

	adjustRemChildChange selection path i (MoveChild d) items
		| i >= 0 && i < numItems items &&  d >= 0 && d < numItems items
			# item=:{MvUI|matched} = getItem i items //If the item was removed by the layout, we don't need to move it downstream
			# change = if matched [] [(adjustIndex i items,MoveChild (adjustIndex d items))]
			# items = moveItem i d items
			# (schanges, items) = adjustRemSiblings selection path (\x -> True) items //TODO: Improve the predicate, we don't have to check all...
			= (change ++ schanges, items) 
		| otherwise
			= ([],items)

	adjustRemSiblings selection path whichSiblings children = (changes, reverse items)
	where
        (changes, items) = adjust 0 [] children


        adjust :: !Int ![MvUIChild] ![MvUIChild] -> (![(!Int,!UIChildChange)],![MvUIChild])
		adjust i before [] = ([],before)
		adjust i before [MvUIItem item:children]
			| item.MvUI.deleted
				= adjust i [MvUIItem item : before] children //Ignore deleted branches
			| whichSiblings i
				# (cchange, items) = adjustRem selection (path ++ [i]) NoChange item
				# change = case cchange of
					(ChangeChild NoChange)         = []
					(ChangeChild (ChangeUI [] [])) = []
					(ChangeChild change)           = [(adjustIndex i (reverse before),cchange)]
				# (changes, children) = adjust (i + 1) (map MvUIItem (reverse items) ++ before) children
				= (change ++ changes, children)
			| otherwise
				= adjust (i + 1) [MvUIItem item : before] children
		adjust i before [child:children]
			= adjust i [child : before] children
			
	adjustIns Nothing path mvuis = (NoChange, removeDeleted mvuis)
	adjustIns (Just dst) path mvuis
		//First check if the previously inserted nodes are still where they were last time
		//If they are not, remove them if necessary
		# (dstExists,dstNew,correctionChange,mvuis) = adjustDestination (Just dst) mvuis
		//Then collect the changes to the destination and apply them in the right place
		| dstExists
			# (changes,mvui)      = collectDestinationChanges dstNew mvuis
			# (updateChange,mvui) = applyAtDestinationPath dst changes mvui
			= (mergeUIChanges correctionChange updateChange, mvui)
		| otherwise
			= (correctionChange, removeDeleted mvuis)

	//Check if a MvUIMoveDestination node is found where we expect it.
	//If it is not, we remove it from the state and create a change to correct the downstream UI 
	//We also return if we had to insert the exists and if we inserted the destination node for the first time
	adjustDestination dst [item=:{MvUI|deleted}:items] //Ignore the deleted items, we haven't removed them yet
		| deleted
			# (dstExist,dstNew,correctionChange,items) = adjustDestination dst items
			= (dstExist,dstNew,correctionChange,[item:items])
		| otherwise
			# (dstExist,dstNew,correctionChange,item) = adjustDestinationItem dst item
			= (dstExist,dstNew,correctionChange,[item:items])
		
	//We found the correct container, start looking for MvUIMoveDestination node
	adjustDestinationItem (Just ([], pos)) mvui=:{MvUI|children} //We found the correct container, start looking for MvUIMoveDestination node
		| pos < 0 = (False,False,NoChange,mvui) //Out of bounds...
		# dstExists = numItems children >= pos
		= case find 0 0 children of
			Nothing //Not found, check if there are enough items for the position
				= (dstExists, True, NoChange, mvui)
			Just (i,ai,n) //Found
				| i == pos //The inserted segment is where it should be!
				    = (True, False, NoChange, mvui)
				| i < pos  //Elements have been removed before the inserted segment
					| dstExists //If there are 'enough' items we can move elements from after the inserted segment to just before it
						# difference = pos - i
						# change = ChangeUI [] [(ai + n + j, MoveChild (ai + j)) \\ j <- [0 .. (difference - 1)]]
						# children = moveMoveDestinationTowardsEnd difference children
						= (True, False,change, {MvUI|mvui & children = children})
					| otherwise //If there are not enough elements, then the destination should not exist
						# change = ChangeUI [] (repeatn n (ai,RemoveChild))
						# children = [x \\ x <- children | not (x =:(MvUIMoveDestination _))]
					    = (False,False,change, {MvUI|mvui & children = children})
				| i > pos  //Elements have been inserted before the inserted segment
					# difference = i - pos
					# change = ChangeUI [] [(ai - 1 - j, MoveChild (ai + n - 1 - j)) \\ j <- [0 .. (difference - 1)]]
					# children = moveMoveDestinationTowardsBegin difference children
					= (True,False,change, {MvUI|mvui & children = children})
	where
		find i ai [] = Nothing
		find i ai [MvUIMoveDestination n:_] = Just (i,ai,n)
		find i ai [MvUIItem x:xs] = find (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
		find i ai [_:xs] = find i ai xs

	//We are still searching for the destination
	adjustDestinationItem (Just ([s:ss], pos)) mvui=:{MvUI|children} 
		# (dstExists, dstNew, changes, children) = adjust 0 0 children
		# change = case changes of
			[] = NoChange
			_  = ChangeUI [] changes
		= (dstExists,dstNew,change, {MvUI|mvui & children = children})
	where
		adjust i ai [] = (False,False,[],[])
		adjust i ai [MvUIItem x:xs]
			| x.MvUI.deleted //Ignore deleted branches
				# (de,dn,cs,xs)   = adjust i ai xs
				= (de,dn,cs,[MvUIItem x:xs])
			| otherwise
				# (de,dn,c,x)     = adjustDestinationItem (if (i == s) (Just (ss,pos)) Nothing) x
				# (des,dns,cs,xs) = adjust (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
				= case c of 
					NoChange = (de || des, dn || dns, cs, [MvUIItem x:xs])
					_        = (de || des, dn || dns, [(ai,ChangeChild c):cs], [MvUIItem x:xs])
		adjust i ai [MvUIMoveDestination n :xs] //This should not be here, create remove instructions
			# (de,dn,cs,xs)   = adjust i ai xs
			= (de,dn,repeatn n (ai,RemoveChild) ++ cs, xs)
		adjust i ai [x:xs] //Ignore other nodes
			# (de,dn,cs,xs)   = adjust i ai xs
			= (de,dn,cs,[x:xs])

	adjustDestinationItem Nothing mvui=:{MvUI|children}
		# (changes, children) = adjust 0 0 children //There should not be a destination node here, process children and remove it when we find one
		# change = case changes of
			[] = NoChange
			_  = ChangeUI [] changes
		= (False,False,change,{MvUI|mvui & children = children})
	where
		adjust i ai [] = ([],[])
		adjust i ai [MvUIItem x:xs]
			# (_,_,c,x) = adjustDestinationItem Nothing x
			# (cs,xs)   = adjust (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
			= case c of 
				NoChange = (cs,[MvUIItem x:xs])
				_        = ([(ai,ChangeChild c):cs], [MvUIItem x:xs])
		adjust i ai [MvUIMoveDestination n :xs] //This should not be here, create remove instructions
			# (cs,xs)   = adjust i ai xs
			= (repeatn n (ai,RemoveChild) ++ cs, xs)
		adjust i ai [x:xs] //Ignore other nodes
			# (cs,xs)   = adjust i ai xs
			= (cs,[x:xs])

	removeDeleted mvuis = hd [{MvUI|x & children = removeDeletedItems children} \\ x=:{MvUI|deleted,children} <- mvuis | not deleted]
	where
		removeDeletedItems items = [app x \\ x <- items | not (del x)]

		app (MvUIItem mvui=:{MvUI|children}) = MvUIItem {MvUI|mvui & children = removeDeletedItems children}
		app x = x

		del (MvUIItem {MvUI|deleted}) = deleted
		del _ = False

	collectDestinationChanges dstNew mvuis
		# (_,changes,items) = collectc 0 (map MvUIItem mvuis)
		= case items of
			[MvUIItem mvui] = (changes,mvui)
			_               = abort "Broken algorithm, not all removed items have been collected :("
	where
		collect i mvui=:{MvUI|matched,moved,dstChange,children}
			| matched 
				| moved && not dstNew //Already moved, only apply the local changes
					= case dstChange of
						NoChange = (i + 1, [], mvui)
						_        = (i + 1, [(i,ChangeChild dstChange)],{MvUI|mvui & dstChange = NoChange})
				| otherwise //Newly removed node
					= (i + 1, [(i,InsertChild (fromMvUI mvui))],{MvUI|mvui & moved=True})
			| otherwise
				# (i,changes,children) = collectc i children
				= (i,changes,{MvUI|mvui & children=children})

		collectc i [] = (i,[],[])
		collectc i [MvUIItem x=:{MvUI|deleted}:xs]
			| deleted 
				# c = if dstNew [] (repeatn (countMoved x) (i,RemoveChild)) //If the destination still exist remove deleted children
				# (i,cs,xs) = collectc i xs
				= (i, c ++ cs, xs)
			| otherwise
				# (i,c,x) = collect i x
				# (i,cs,xs) = collectc i xs
				= (i, c ++ cs, [MvUIItem x:xs])
		collectc i [x:xs]
			# (i,cs,xs) = collectc i xs
			= (i,cs,[x:xs])

	applyAtDestinationPath ([],pos) changes mvui=:{MvUI|children}
		//Adjust the indices of the changes
		# numRemovedBeforePos = length [matched \\ MvUIItem {MvUI|matched} <- take pos children | matched]
		# changes = [(pos - numRemovedBeforePos + i,c) \\ (i,c) <- changes]
		//Determine how much the number of moved items changes in the destination
		# insertedAmountChange = length [c \\ c=:(_,InsertChild _) <- changes] - length  [c \\ c=:(_,RemoveChild) <- changes]
		# children = updateMoveDestination pos ((+) insertedAmountChange) children
		= (ChangeUI [] changes, {MvUI|mvui & children = children})
	applyAtDestinationPath ([s:ss],pos) changes mvui=:{MvUI|children}
		| s >= 0 && s < numItems children
			# (change,item) = applyAtDestinationPath (ss,pos) changes (getItem s children)
			= (ChangeUI [] [(adjustIndex s children,ChangeChild change)], {MvUI|mvui & children = setItem s [item] children})
		| otherwise
			= (NoChange,mvui)

	restore (LSRemoveSubUIs mvui)
		= ReplaceUI (fromMvUI mvui) //VERY CRUDE RESTORE..

	//UTIL FUNCTIONS 
	toMvUI :: UI -> MvUI 
	toMvUI (UI type attr items) = {MvUI|type=type,attr=attr,matched=False,moved=False,deleted=False,dstChange=NoChange,children=map (MvUIItem o toMvUI) items}

	fromMvUI :: MvUI -> UI
	fromMvUI {MvUI|type,attr,children} = UI type attr [fromMvUI mvui \\ MvUIItem mvui <- children]
	
	//Check if the destination for moving elements is this node or one of its descendents
	onDestinationPath path = maybe False (startsWith path o fst) mbDst
	where
		startsWith [] dst = True
		startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
		startsWith _ _ = False

	destinationExists ([],pos) {MvUI|children}
		= pos >= 0 && pos <= numItems children
	destinationExists ([s:ss],pos) {MvUI|children}
		| s >= 0 && s < numItems children = destinationExists (ss,pos) (getItem s children)
										  = False

	numItems xs = length [x \\ x=:(MvUIItem {MvUI|deleted}) <- xs | not deleted]

	getItem i [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = getItem i xs
		| i == 0  = x
				  = getItem (i - 1) xs
	getItem i [_:xs] = getItem i xs
		
	setItem i item [] = []
	setItem i item [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:setItem i item xs]
		| i < 0  = [MvUIItem x:xs]
		| i == 0 = map MvUIItem item ++ xs
				 = [MvUIItem x:setItem (i - 1) item xs]
	setItem i item [x:xs] = [x:setItem i item xs]

	insertItem i item [] 
		| i == 0 = [MvUIItem item]
				 = []
	insertItem i item [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:insertItem i item xs]
		| i < 0  = [MvUIItem x:xs]
		| i == 0 = [MvUIItem item, MvUIItem x:xs]
				 = [MvUIItem x:insertItem (i - 1) item xs]
	insertItem i item [x:xs] = [x:insertItem i item xs]
	
	moveItem i d xs = insertItem d (getItem i xs) (removeItem i xs)

	removeItem i [] = []
	removeItem i [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:removeItem i xs]
	 	| i < 0   = [MvUIItem x:xs]
		| i == 0  = xs
				  = [MvUIItem x:removeItem (i - 1) xs]
	removeItem i [x:xs] = [x:removeItem i xs]

	updateMoveDestination 0 f [MvUIMoveDestination n:xs] = [MvUIMoveDestination (f n):xs]
	updateMoveDestination 0 f xs                         = [MvUIMoveDestination (f 0):xs]
	updateMoveDestination i f [MvUIItem x:xs]            = [MvUIItem x:updateMoveDestination (i - 1) f xs]
	updateMoveDestination i f [x:xs]                     = [x:updateMoveDestination i f xs]
	updateMoveDestination i f [] = []

    moveMoveDestinationTowardsEnd n children = children//trace_n "A" children //FIXME
    moveMoveDestinationTowardsBegin n children = children//trace_n "B" children

	countMoved {MvUI|moved,children} = (if moved 1 0) + sum [countMoved x \\ MvUIItem x <- children]

	adjustIndex s items = s + (offset s items)
	where
		offset n [] = n
		offset n [MvUIItem {MvUI|matched,deleted}:xs]
			| deleted = offset n xs //Ignore deleted items
			| n == 0    = 0 //Stop
						= offset (n - 1) xs - (if matched 1 0)
		offset n [MvUIMoveDestination num:xs] = offset n xs + num

layoutSubUIs :: UISelection Layout -> Layout
//layoutSubUIs selection layout = layoutSubUIsRef_ selection layout 
layoutSubUIs selection layout = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	//Find all places that match the selection
	//Keep track of the original UI in the state to enable dynamic matches
	apply ui
		# (change, state) = apply` [] ui
		= (change, LSLayoutSubUIs ui state)

	apply` path ui=:(UI type attr items)
		| inUISelection selection path ui 
			# (change,state) = layout.Layout.apply ui
			= (change,UIModified state)
		| otherwise
			# (itemChanges,itemStates) = unzip [apply` (path ++ [i]) ui \\ ui <- items & i <- [0..]]
			//Cleanup item changes (only keep those that actually change something
			# itemChanges =	[(i,ChangeChild c) \\ c <- itemChanges & i <- [0..] | not (c =: NoChange || c =: (ChangeUI [] []))]
			//Also cleanup item states
			# itemStates = [(i,s) \\ s <- itemStates & i <- [0..] | not s =: (SubUIsModified _ [])]
			= (ChangeUI [] itemChanges,SubUIsModified () itemStates)

	adjust (change, LSLayoutSubUIs ui state)
		# (change,ui,state) = adjust` [] change ui state
		= (change,LSLayoutSubUIs ui state)

	//For replacements, just use the apply rule on the new ui
	adjust` path (ReplaceUI ui) _ state 
		# (change, state) = apply` path ui
		= (ReplaceUI (applyUIChange change ui), ui, state)

	//When we get a change for a previously modified ui, we need to check whether the change still holds
	adjust` path change ui (UIModified state)
		# ui = applyUIChange change ui //Keep the 'shadow' copy of the UI up-to date
		| inUISelection selection path ui 
			//The layout should still be applied
			# (change,state) = layout.Layout.adjust (change, state)
			= (change,ui,UIModified state)
		| otherwise
			//The layout should no longer be applied, use the restore function to undo the layout
			//then apply the upstream change
			# rchange = layout.Layout.restore state
			//Now that this ui no longer matches, maybe its descendents do
			# (achange, state) = apply` path ui
			//The result is, the combination of first restoring, then updating, t
			# change = mergeUIChanges rchange (mergeUIChanges change achange)
			= (change, ui, state)

	//When we get a change, we need to check which sub-uis were affected
	adjust` path change ui state=:(SubUIsModified _ states)
		//Check if the change means that the layout is now applicable to this node
		| inUISelectionAfterChange selection path ui change
			//Update the 'shadow' copy of the UI
			# ui             = applyUIChange change ui
			//If the UI now matches we need to restore all changes to sub-ui's and then apply the layout
			# restore        = restoreSubUIs state
			# (change,state) = layout.Layout.apply ui
			= (mergeUIChanges restore change, ui, UIModified state)
		//Apply the change, and modify it if necessary
		| otherwise
			= case change of
				(ChangeUI attrChanges childChanges)
					//Update the attributes of the 'shadow' ui
					# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
					# (childChanges, items, states) = adjustChildChanges childChanges items states
					= (ChangeUI attrChanges childChanges, UI type attr items, SubUIsModified () states)
				NoChange
					//Recursively check all children
					# (UI type attr items) = ui
					# (childChanges, items, states) = adjustChildChanges [(i, ChangeChild NoChange) \\ i <- [0 .. (length items - 1)]] items states
					# change = if (childChanges =: []) NoChange	(ChangeUI [] childChanges)
					= (change, UI type attr items, SubUIsModified () states)
	where
		adjustChildChanges [] items states = ([], items, states)
		adjustChildChanges [(i,c):cs] items states
			# (c, items, states)  = adjustChildChange i c items states
			# (cs, items, states) = adjustChildChanges cs items states
			= (c ++ cs, items, states)

		adjustChildChange i (ChangeChild change) items states
			//Recursively adjust the change
			| i >= 0 && i < length items
				# (change, item, state) = adjust` (path ++ [i]) change (items !! i) (ltGet i states)
				= (case change of NoChange = []; _ = [(i,ChangeChild change)], updateAt i item items, ltPut i state states)
			| otherwise
				= ([],items, states)
		adjustChildChange i (InsertChild ui) items states
			| i >= 0 && i <= length items
				//(potentially) apply the layout to the inserted item
				# (change,state) = apply` (path ++ [i]) ui
				//Check the siblings, because their path has changed
				# (schanges, items, states) = adjustSiblings path (\x -> x > i) (insertAt i ui items) (ltInsert i state states)
				= ([(i,InsertChild (applyUIChange change ui)):schanges], items, states)
			| otherwise
				= ([], items, states)
		adjustChildChange i RemoveChild items states
			| i >= 0 && i < length items
				//Check the siblings, because their path has changed
				# (schanges, items, states) = adjustSiblings path (\x -> x >= i) (removeAt i items) (ltRemove i states)
				= ([(i,RemoveChild):schanges], items, states)
			| otherwise
				= ([], items, states)
		adjustChildChange i (MoveChild d) items states
			| i >= 0 && i < length items && d >= 0 && d < length items
				//Check the siblings, because their path has changed //TODO: We can do better... don't need to check all
				# (schanges, items, states) = adjustSiblings path (const True) (listMove i d items) (ltMove i d states)
				= ([(i,MoveChild d):schanges], items, states)
			| otherwise
				= ([], items, states)

		adjustSiblings path whichSiblings items states = adjust 0 items states
		where
            adjust :: !Int ![UI] ![(!Int, !LayoutTree LayoutState ())]
                   -> (![(!Int, !UIChildChange)], ![UI], ![(!Int, !LayoutTree LayoutState ())])
			adjust i [] states = ([],[],states)
			adjust i [item:items] states
				| whichSiblings i
					//Check
					# (change,item,state) = adjust` (path ++ [i]) NoChange item (ltGet i states)
					//Check the remaining items
					# (changes, items, states) = adjust (i + 1) items (ltPut i state states)
					= case change of 
						NoChange = (changes, [item:items], states)
						_        = ([(i,ChangeChild change):changes], [item:items], states)
				| otherwise
					# (changes, items, states) = adjust (i + 1) items states
					= (changes, [item:items], states)

	restoreSubUIs (UIModified state) = layout.Layout.restore state
	restoreSubUIs (SubUIsModified _ states)
		= case [(i,ChangeChild (restoreSubUIs s)) \\ (i,s) <- states] of
			[]      = NoChange
			changes = ChangeUI [] changes

	restore (LSLayoutSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE... TODO:We can do better than this

layoutSubUIsRef_ :: UISelection Layout -> Layout
layoutSubUIsRef_ selection layout = referenceLayout ref
where
	ref ui = app [] ui
	app path ui=:(UI type attr items) 
		| inUISelection selection path ui = applyLayout layout ui
										  = UI type attr [app (path ++ [i]) x \\ x <- items & i <- [0..]]

sequenceLayouts :: Layout Layout -> Layout 
//sequenceLayouts layout1 layout2 = sequenceLayoutsRef_ layout1 layout2
sequenceLayouts layout1 layout2 = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		# (change1,s1) = layout1.Layout.apply ui
		# (change2,s2) = layout2.Layout.apply (applyUIChange change1 ui)
		= (mergeUIChanges change1 change2, LSSequence s1 s2)

	adjust (change,LSSequence s1 s2) 
		# (change,s1) = layout1.Layout.adjust (change,s1)
		# (change,s2) = layout2.Layout.adjust (change,s2)
		= (change,LSSequence s1 s2)
	adjust (change,s) = (change,s)

	restore (LSSequence s1 s2)
		//Restore in reverse order
		# change2 = layout2.Layout.restore s2
		# change1 = layout1.Layout.restore s1
		= mergeUIChanges change2 change1	

sequenceLayoutsRef_ :: Layout Layout -> Layout
sequenceLayoutsRef_ layout1 layout2 = referenceLayout ref
where
	ref ui = applyLayout layout2 (applyLayout layout1 ui)

referenceLayout :: (UI -> UI) -> Layout
referenceLayout ref = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (ReplaceUI (ref ui), LSReference ui)
		
	adjust (NoChange,state) = (NoChange,state)
	adjust (change, LSReference ui) 
		# ui = applyUIChange change ui
		= (ReplaceUI (ref ui),LSReference ui)

	restore (LSReference ui)
		= ReplaceUI ui

//Helper for sequence layouts
applyLayout :: Layout UI -> UI 
applyLayout {Layout|apply} ui = applyUIChange (fst (apply ui)) ui

//Util functions on the layout tree structure
ltGet :: Int [(Int,LayoutTree a b)] -> (LayoutTree a b) | gDefault{|*|} b
ltGet index [] = SubUIsModified defaultValue []
ltGet index [(i,tree):ts] = if (i == index) tree (ltGet index ts)

ltPut :: Int (LayoutTree a b) [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
// It is pointless to store empty trees in a sparse representation, just make sure we delete the previously stored value
ltPut index (SubUIsModified _ []) list = [x \\ x=:(i,_) <- list | i <> index] 
ltPut index item list
	= [x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [x \\ x=:(i,_) <- list | i > index]

ltInsert :: Int (LayoutTree a b) [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
ltInsert index (SubUIsModified _ []) list = [(if (i >= index) (i + 1) i, x) \\ (i,x) <- list]
ltInsert index item list 
	= [ x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [(i + 1,x) \\ (i,x) <- list | i >= index]

ltRemove :: Int [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
ltRemove index list = [(if (i > index) (i - 1) i, x) \\ (i,x) <- list | i <> index]

ltMove :: Int Int [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)] | gDefault{|*|} b
ltMove src dst list = ltInsert dst (ltGet src list) (ltRemove src list)

ltCount :: Bool (a -> Bool) [(Int,LayoutTree a b)] -> Int
ltCount recursive pred list = foldr count 0 (map snd list)
where
	count (UIModified x) n = if (pred x) (n + 1) n
	count (SubUIsModified _ mods) n = if recursive (n + ltCount recursive pred mods) n

listMove :: Int Int [a] -> [a]
listMove src dst list = insertAt dst (list !! src) (removeAt src list)

//Experiment to create an alternative, more declarative way of specifying layouts
/*
Function UI -> UI s.t. h o g o f
Where
f : UI -> TaskUITree
g : TaskUITree -> TaskUILayout
h : TaskUILayout -> UI

f transforms the original UI into a TaskUITree the same way we do now
g transforms the UILayout into a layout
h transforms the UILayout into a sparse new UI with some attributes (like direction), including an attribute origin that contains the original UIPath in the original tree

*/
:: TaskUITree
  = Ed  UIPath
  | Par UIPath [TaskUITree]

:: TaskUILayout a
  = UIBeside [TaskUILayout a]
  | UIAbove  [TaskUILayout a]
  | UINode   UIPath

uiOf :: TaskUITree -> TaskUILayout a
uiOf (Ed  path  ) = UINode path
uiOf (Par path _) = UINode path

besideT ts = UIBeside ts
aboveT ts = UIAbove ts

uiToRefs :: UI -> TaskUITree
uiToRefs ui
  = case ui of
      UI UIParallel _ subs = Par [] (recurse [] subs)
      UI _          _ subs = case recurse [] subs of
                               [x : _] -> x
                               _       -> Ed []
  where
  uiToRefs` :: UIPath (Int, UI) -> [TaskUITree]
  uiToRefs` path (i, UI UIParallel _ subs)
    # curPath = path ++ [i]
    = [Par curPath (recurse curPath subs)]
  uiToRefs` path (i, UI x _ _)
    # curPath = path ++ [i]
    = [Ed curPath]
  recurse curPath subs = flatten (map (uiToRefs` curPath) (zip2 [0..] subs))

taskUILayoutToUI :: (TaskUILayout a) -> UI
taskUILayoutToUI (UIBeside ls)
  = UI UIParallel ('DM'.singleton "direction" (encodeUI Horizontal)) (map taskUILayoutToUI ls)
taskUILayoutToUI (UIAbove ls)
  = UI UIParallel ('DM'.singleton "direction" (encodeUI Vertical)) (map taskUILayoutToUI ls)
taskUILayoutToUI (UINode path)
  = UI UIContainer ('DM'.singleton "origin" (toJSON path)) []

