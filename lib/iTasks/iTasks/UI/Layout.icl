implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc, StdString
import Data.GenLexOrd
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor, Data.Func
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil, iTasks.UI.Definition
import iTasks.Internal.Generic.Defaults 
import StdEnum

from Data.Map as DM import qualified newMap, put, get, del, toList, fromList, delList, alter, union, keys, unions, singleton, member, null
from Data.Set as DS import qualified newSet, insert, delete, toList, fromList, null
from Data.Tuple import appSnd
from Data.Map import instance Functor (Map k)

import Text.GenJSON

from StdFunc import o, const, id, flip
from iTasks.Internal.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
from iTasks.Internal.TaskEval import :: TaskTime
from iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.WF.Definition
import Data.GenEq

derive gEq LUIEffectStage, LUINo
derive gLexOrd LUIEffectStage

instance < (LUIEffectStage a) | gLexOrd{|*|} a
where
	(<) x y = (gLexOrd{|*|} x y) === LT

instance < LUINo
where
	(<) (LUINo xs) (LUINo ys) = xs < ys

instance == LUINo
where
	(==) (LUINo xs) (LUINo ys) = xs == ys

instance toString LUINo
where
	toString (LUINo steps) = join "." (map toString steps)


/*
* The first thing we need for rule-based layouts is a datastructure that can keep track
* of what changes have been made by layout rules and that can 'buffer' upstream changes 
* such that layout rules can be applied before they are passed on.
* 
* The following functions are used to initialize this datastructure
*/

noChanges :: LUIChanges
noChanges = {toBeInserted=False, toBeRemoved=False, toBeReplaced=Nothing, toBeShifted=Nothing, setAttributes='DM'.newMap, delAttributes = 'DS'.newSet}

noEffects :: LUIEffects
noEffects = {overwrittenType = ESNotApplied, overwrittenAttributes = 'DM'.newMap, hiddenAttributes = 'DM'.newMap, additional = ESNotApplied, hidden = ESNotApplied, wrapper = ESNotApplied, unwrapped = ESNotApplied}

//Initialize an LUI tree from a regular UI tree
initLUI :: UI -> LUI
initLUI (UI type attr items) = LUINode type attr (map initLUI items) noChanges noEffects

initLUIMoves :: LUIMoves
initLUIMoves = 'DM'.newMap

extractResetChange :: (LUI,LUIMoves) -> (UIChange,(LUI,LUIMoves))
extractResetChange (lui,moves)
	# mbUI = extractUIWithEffects_ (lui,moves)
	= case confirmChangesAndEffects_ (lui,moves) of
		(Just lui,moves) = (ReplaceUI (fromMaybe (UI UIEmpty 'DM'.newMap []) mbUI), (lui,moves))
		_                = abort "extractResetChange: top-level node removed accidentally"
/*
* When upstream changes 'arrive' they are tracked in the 'buffer' data structure.
* All information about what should be modified according to the upstream change is
* recorded in the tree.
* 
* The following functions implement this recording of changes in the tree
*/

//When an upstream UI change is applied to the LUI it is recorded in the LUI tree
//in such a way that it can easily be extracted as a downstream change later on
applyUpstreamChange :: UIChange (LUI,LUIMoves) -> (LUI,LUIMoves)
applyUpstreamChange change (lui=:(LUIMoveSource moveId),moves)
	# (movedStage,movedLui) = getMovedNode_ moveId moves
	# (movedLui,moves) = applyUpstreamChange change (movedLui,moves)
	# moves = putMovedNode_ moveId (movedStage,movedLui) moves
	= (lui,moves)
//If the node is a wrapper, apply the change to the wrapped child
applyUpstreamChange change (LUINode type attr items changes effects=:{LUIEffects|wrapper=ESApplied _},moves)
	# (items,moves) = mapSt (\i m -> if (isAdditional_ i) (i,m) (applyUpstreamChange change (i,m))) items moves
	= (LUINode type attr items changes effects,moves)
applyUpstreamChange NoChange (lui,moves) = (lui,moves)
applyUpstreamChange (ReplaceUI ui) (LUINode type attr items changes effects,moves)
	| changes.toBeInserted //If it is a new node, we can replace it
		= (setToBeInserted_ (initLUI ui),moves)
	= (LUINode type attr items {changes & toBeReplaced = Just (initLUI ui)} effects,moves)
applyUpstreamChange (ReplaceUI ui) (lui,moves) = abort "applyUpstreamChange: can't replace an non-LUINode constructor"
applyUpstreamChange (ChangeUI attributeChanges childChanges) (lui,moves)
	= (foldl applyUpstreamChildChange (foldl applyUpstreamAttributeChange (lui,moves) attributeChanges) childChanges)
where
	applyUpstreamAttributeChange :: (LUI,LUIMoves) UIAttributeChange -> (LUI,LUIMoves)
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects,moves) (SetAttribute key value)
		# setAttributes = 'DM'.put key value setAttributes
		# delAttributes = 'DS'.delete key delAttributes
		= (LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects, moves)
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects,moves) (DelAttribute key)
		# setAttributes = 'DM'.del key setAttributes
		# delAttributes = 'DS'.insert key delAttributes
		= (LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects, moves)
	applyUpstreamAttributeChange (lui,moves) _ = (lui,moves)

	applyUpstreamChildChange :: (LUI,LUIMoves) (Int,UIChildChange) -> (LUI,LUIMoves)
	applyUpstreamChildChange (lui,moves) (index,ChangeChild change) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			# (items,moves) = updateItem (applyUpstreamChange change) adjustedIndex items moves
			= (LUINode type attr items changes effects, moves)
		_
			= (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,RemoveChild) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			= (LUINode type attr (removeItem adjustedIndex items) changes effects,moves)
		_ = (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,InsertChild ui) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex > length items = (lui,moves)
			= (LUINode type attr (insertAt adjustedIndex (setToBeInserted_ (initLUI ui)) items) changes effects, moves)
		_ = (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,MoveChild destination) = case lui of
		(LUINode type attr items changes effects)
			# shiftId = nextShiftID_ items
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			= (LUINode type attr (shiftItem shiftId adjustedIndex destination items) changes effects, moves)
		_ = (lui,moves)

	//An index may point to the destination of a shifted child node. In that case we want to apply
	//the update to the node that will be shifted to that destination
	updateItem updateFunction index items moves = case items !! index of
		(LUIShiftDestination shiftId) = updateItem updateFunction (fst (lookupShiftSource_ shiftId items)) items moves
		lui 
			# (lui,moves) = applyUpdate (lui,moves)
			= (updateAt index lui items, moves)
	where
		applyUpdate (LUINode type attr items changes=:{toBeReplaced = Just replacement} effects,moves)
			# (replacement,moves) = applyUpdate (replacement,moves)
			= (LUINode type attr items {changes & toBeReplaced = Just replacement} effects, moves)
		applyUpdate (lui,moves) = updateFunction (lui,moves)

	//When upstream removes a shifted note, we can forget that it was shifted and mark the source node as removed instead
	removeItem index items = case items !! index of
		(LUIShiftDestination shiftId) = map (removeShiftSource shiftId) (removeAt index items)
		(LUINode type attr citems changes effects) = updateAt index (LUINode type attr citems {changes & toBeRemoved = True} effects) items
	where
		removeShiftSource shiftId lui=:(LUINode type attr items changes=:{toBeShifted = Just sourceId} effects)
			| sourceId == shiftId = LUINode type attr items {changes & toBeShifted = Nothing, toBeRemoved = True} effects
			                      = lui
		removeShiftSource _ lui = lui

	shiftItem shiftId index destination items = case items !! index of
		//A shift destination: that means upstream expects the node to be already moved
		//We set a new destination or remove the destination if we move back to the original position
		(LUIShiftDestination prevShiftId)
			//Remove the current destination
			# items = removeAt index items
			//If we move it back to the original position, we can consider the node never to be moved
			//otherwise, we create a new destination node
			= case findSamePositionShift prevShiftId destination items of
				Just (sourcePosition,LUINode type attr citems changes effects)
					//Update the source
					= updateAt sourcePosition (LUINode type attr citems {changes & toBeShifted = Nothing} effects) items
				Nothing
					//And add the new destination
					= insertAt (adjustIndex_ destination items moves) (LUIShiftDestination prevShiftId) items
		//Regular node
		(LUINode type attr citems changes effects)
			//Mark the node as a shifted node
			# items = updateAt index (LUINode type attr citems {changes & toBeShifted = Just shiftId} effects) items
			//Record the destination
			= insertAt (adjustIndex_ destination items moves) (LUIShiftDestination shiftId) items
	where
		findSamePositionShift shiftId destination items = find 0 0 items
		where
			find i ai [] = Nothing
			find i ai [x=:(LUINode _ _ _ {toBeShifted=Just sourceId} _):xs]
				| sourceId == shiftId = if (ai == destination) (Just (ai,x)) Nothing
				                      = find (i + 1) ai xs
			find i ai [x:xs]
				| nodeExists_ (LUINo []) x moves = find (i + 1) (ai + 1) xs
				                                              = find (i + 1) ai xs

	//Adjust a change index for additional nodes inserted by layout rules
	adjustIndex_ :: Int [LUI] LUIMoves -> Int
	adjustIndex_ index items moves = fst3 (scanToPosition_ (LUINo []) index items moves)

/*
* Layout rules transform the 'buffered' tree and annotate the places where layout effects
* should be applied, or should no longer be applied
*/
setUIType :: UIType -> LayoutRule
setUIType newType = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{overwrittenType},moves)
		# overwrittenType = case overwrittenType of
			(ESNotApplied) = ESToBeApplied (ruleNo,newType)
			(ESToBeApplied _) = ESToBeApplied (ruleNo,newType)
			(ESApplied (curRule,curType))
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise             = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
			(ESToBeUpdated (curRule,curType) _)
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise           = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
			(ESToBeRemoved (curRule,curType))
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise           = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
		= (LUINode type attr items changes {effects & overwrittenType = overwrittenType},moves)

setUIAttributes :: UIAttributes -> LayoutRule
setUIAttributes setAttributes = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{overwrittenAttributes},moves)
		# overwrittenAttributes = foldr (overwriteAttribute_ ruleNo) overwrittenAttributes ('DM'.toList setAttributes)
		= (LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes},moves)

delUIAttributes :: UIAttributeSelection -> LayoutRule
delUIAttributes selection = rule 
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui=:(LUINode type attr items changes effects=:{hiddenAttributes}),moves)
		# keys = 'DM'.keys (nodeAttributes_ ruleNo lui)
		# hiddenAttributes = foldr (hideAttribute_ ruleNo (matchAttributeKey_ selection)) hiddenAttributes keys
		= (LUINode type attr items changes {effects & hiddenAttributes = hiddenAttributes}, moves)

modifyUIAttributes :: UIAttributeSelection (UIAttributes -> UIAttributes) -> LayoutRule
modifyUIAttributes selection modifier = rule 
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui=:(LUINode type attr items changes effects=:{overwrittenAttributes,hiddenAttributes}),moves)
		//1. Apply the modifier function to the current of attributes that match the selection
		# selectedAttr = selectAttributes_ selection (nodeAttributes_ ruleNo lui)
		# modifiedAttr = modifier selectedAttr
		//2. Override new attributes and hide attributes that match the selection 
		# overwrittenAttributes = overrideModifiedAttributes ruleNo modifiedAttr overwrittenAttributes
		# hiddenAttributes = hideRemovedAttributes ruleNo selectedAttr modifiedAttr hiddenAttributes
		= (LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes, hiddenAttributes = hiddenAttributes}, moves)

	overrideModifiedAttributes ruleNo modified overwritten = foldr (overwriteAttribute_ ruleNo) overwritten ('DM'.toList modified)
	hideRemovedAttributes ruleNo selected modified hidden = foldr (hideAttribute_ ruleNo isRemoved) hidden ('DM'.keys selected)
	where
		isRemoved key = not ('DM'.member key modified)

copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> LayoutRule
copySubUIAttributes selection src dst = rule
where
	rule ruleNo (lui,moves)
		//Find the selected attributes in the source node... 
		//Then use the setUIAttributes layout rule to copy the changes
		= maybe (lui,moves) (withEffect (lui,moves)) (selectSource (lui,moves))
	where
		selectSource (lui,moves) = fmap (selectAttributes_ selection o nodeAttributes_ ruleNo) (selectSubNode_ ruleNo src (lui,moves))
		withEffect (lui,moves) attr = updateSubNode_ ruleNo dst ((setUIAttributes attr) ruleNo) (lui,moves)

wrapUI :: UIType -> LayoutRule
wrapUI type = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo rule` (lui,moves)
	where
		rule` (lui=:(LUINode _ _ _ _ {LUIEffects|wrapper}),moves)
			//Check if we already wrapped
			| wrapper === (ESApplied ruleNo) || wrapper === (ESToBeApplied ruleNo)
				= (lui,moves)
			//Not yet wrapped
			| otherwise
				= (LUINode type 'DM'.newMap [lui] noChanges {noEffects & wrapper = ESToBeApplied ruleNo},moves)

unwrapUI :: LayoutRule
unwrapUI = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{LUIEffects|unwrapped}, moves)
		# hasChildren = (selectChildNodes_ ruleNo (items,moves)) =: [_:_]
		# unwrapped = case unwrapped of
			ESNotApplied = if hasChildren (ESToBeApplied ruleNo) ESNotApplied
			ESToBeApplied _ = if hasChildren (ESToBeApplied ruleNo) ESNotApplied
			ESApplied prevRuleNo = if hasChildren (ESApplied ruleNo) (ESToBeRemoved prevRuleNo)
			ESToBeRemoved prevRuleNo = if hasChildren (ESApplied ruleNo) (ESToBeRemoved prevRuleNo)
		= (LUINode type attr items changes {effects & unwrapped = unwrapped}, moves)

hideUI :: LayoutRule
hideUI = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)
	rule` ruleNo (LUINode type attr items changes effects=:{hidden},moves)
		# hidden = case hidden of
			ESNotApplied = ESToBeApplied ruleNo
			ESToBeApplied _ = ESToBeApplied ruleNo
			ESApplied _ = ESApplied ruleNo
			ESToBeRemoved _ = ESApplied ruleNo
		= (LUINode type attr items changes {effects & hidden=hidden},moves)

insertChildUI :: Int UI -> LayoutRule
insertChildUI position insertion = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)
	rule` ruleNo (lui=:(LUINode type attr items changes effects),moves)
		= case scanToPosition_ ruleNo position items moves of
			(_,True,Nothing)	
				//If the index is at the end of the range, add the item
				# items = undoAdditions ruleNo items ++ [setToBeAdded_ ruleNo (initLUI insertion)]
				= (LUINode type attr items changes effects, moves)
			(index,True,Just selected)
				# additional = getAdditional selected
				//Already inserted
				| additional === ESToBeApplied ruleNo || additional === ESApplied ruleNo
					= (lui, moves)
				| otherwise
					//Not yet inserted, or at the wrong position
					# item = setToBeAdded_ ruleNo (initLUI insertion)
					# items = insertAt index item (undoAdditions ruleNo items)
					= (LUINode type attr items changes effects, moves)
			_
				# items = undoAdditions ruleNo items
				= (LUINode type attr items changes effects, moves)

	getAdditional (LUINode _ _ _ _ {additional}) = additional
	getAdditional _ = ESNotApplied

	undoAdditions ruleNo items = map undo items
	where
		undo lui=:(LUINode type attr items changes effects=:{additional})
			= LUINode type attr items changes {effects & additional = undoEffect_ ruleNo id additional}
		undo lui = lui

moveSubUIs :: UISelection UIPath Int -> LayoutRule
moveSubUIs selection path pos = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui,moves)
		# (moved,lui,moves) = checkNodes selection (destinationExists path pos (lui,moves)) lui moves
		# (lui,moves) = updateDestination moved (lui,moves)
		= (lui,moves)
	where
		//1 Check if the destination position. If it does not, we can't move anything
		destinationExists path pos (lui,moves)
			= maybe False (positionExists pos) (selectSubNode_ ruleNo path (lui,moves))
		where
			positionExists pos (LUINode _ _ items _ _)
				= snd3 (scanToPosition_ ruleNo pos items moves)

		//2 Recursively remove/restore based on the selection nodes 
		checkNodes selection destinationExists (LUINode type attr items changes effects) moves
			# nextId = nextMoveID_ moves
			# ((_,acc),items,moves) = processChildNodes_ ruleNo (check []) ((nextId,[]),items,moves)
			= (reverse acc, LUINode type attr items changes effects, moves)
		where
			check path i ((nextId,acc),lui,moves)
				//Check if the node should be moved
				| destinationExists && nodeSelected_ ruleNo selection (path ++ [i]) lui moves //Node should be moved
					= case lui of
						(LUINode type attr items changes effects)
							# moves = putMovedNode_ nextId (ESToBeApplied ruleNo,LUINode type attr items changes effects) moves
							= ((nextId + 1,[nextId:acc]), LUIMoveSource nextId, moves)
						(LUIMoveSource moveId)
							//If we match a source here, either this rule moved it or a later rule.
							//If the referenced node was moved by a later rule, we update it because this rule comes first 
							# moves = case getMovedNode_ moveId moves of
								(ESApplied movedBy, LUINode type attr items changes effects)
									| movedBy == ruleNo
										= moves //We moved it, no need to update anything
									| otherwise
										= putMovedNode_ moveId (ESToBeUpdated movedBy ruleNo,LUINode type attr items changes effects) moves
								(ESToBeUpdated _ movedBy, LUINode type attr items changes effects)
									| movedBy == ruleNo
										= moves //We Just updated it, no need to update anything
									| otherwise
										= abort "moveSubUIs: We should never reach this case"
								_	
									= abort "moveSubUIs: We should never reach this case"
							= ((nextId,[moveId:acc]), LUIMoveSource moveId, moves)
						(LUIMoveDestination moveId _)
							//If we match a move destination here, an earlier rule moved it here
							# moves = case getMovedNode_ moveId moves of
								(ESToBeApplied _, LUINode type attr items changes effects) //It is not yet moved
									= putMovedNode_ moveId (ESToBeApplied ruleNo, LUINode type attr items changes effects) moves
								(ESApplied movedBy, LUINode type attr items changes effects) //It was already moved

									= (putMovedNode_ moveId (ESToBeUpdated movedBy ruleNo,LUINode type attr items changes effects) moves)
								(ESToBeUpdated movedBy _, LUINode type attr items changes effects) //It was already moved (twice)
									= putMovedNode_ moveId (ESToBeUpdated movedBy ruleNo, LUINode type attr items changes effects) moves
							= ((nextId,[moveId:acc]),lui,moves)	
						_
							= abort "moveSubUIs: We don't expect shift destinations here (because we are called by processChildNodes_)"
				| otherwise
					= case lui of
						(LUINode type attr items changes effects)
							//Search in children
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							= ((nextId,acc),LUINode type attr items changes effects,moves)	
						(LUIMoveSource moveId)
							//If we match a source here, either this rule moved it or a later rule.
							# (moved,LUINode type attr items changes effects) = getMovedNode_ moveId moves
							//If this rule moved it here we should mark it as no longer moved
							# moved = case moved of
								(ESApplied movedBy) | movedBy == ruleNo
									= ESToBeRemoved ruleNo
								_
									= moved
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							# moves = putMovedNode_ moveId (moved,LUINode type attr items changes effects) moves
							= ((nextId,acc),lui,moves)	
						(LUIMoveDestination moveId _)
							//An earlier rule moved it here, just process the children
							# (movedStage,LUINode type attr items changes effects) = getMovedNode_ moveId moves
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							# moves = putMovedNode_ moveId (movedStage,LUINode type attr items changes effects) moves
							= ((nextId,acc),lui,moves)	
						_	
							= ((nextId,acc),lui,moves)	

		//3 Mark the destination with the moved nodes
		updateDestination moved (lui,moves) = updateSubNode_ ruleNo path update (lui,moves)
		where
			update (LUINode type attr items changes effects, moves)
				//Remove the current destination markers
				# items = filter (not o currentMove) items
				// (re)Insert the destination marker in the right place
				# destinations = [LUIMoveDestination moveId ruleNo \\ moveId <- moved]
				# items = case scanToPosition_ ruleNo pos items moves of
					(index,True,_) = ( take index items ++ destinations ++ drop index items)
					_ = items
				= (LUINode type attr items changes effects, moves)

			currentMove (LUIMoveDestination moveId _) = isMember moveId moved
			currentMove  _ = False

layoutSubUIs :: UISelection LayoutRule -> LayoutRule
layoutSubUIs selection sub = rule
where
	rule ruleNo (lui,moves) = apply [] (lui,moves)
	where
		//Check if the layout matches (without any dereferencing or selecting wrapped nodes)
		apply path (lui,moves)
			| nodeSelected_ ruleNo selection path lui moves //If the layout matches, apply the rule
				= (sub ruleNo (lui,moves))
			| otherwise
				//We first undo effects previously applied by this rule
				# (lui,moves) = undoEffects_ ruleNo (lui,moves)
				//We want to check the set of children at the time of `ruleNo`
				= updateNode_ ruleNo (applyc path) (lui,moves)

		applyc path (lui=:(LUINode type attr items changes effects),moves)
			# (items,moves) = updateChildNodes_ ruleNo (\i (item,moves) -> apply (path ++ [i]) (item,moves)) (items,moves)
			= (LUINode type attr items changes effects,moves)

sequenceLayouts :: [LayoutRule] -> LayoutRule
sequenceLayouts subs = rule
where
	rule (LUINo ruleNo) lui = snd (foldl apply (0,lui) subs)
	where
		apply (i,lui) sub = (i + 1, sub (LUINo (ruleNo ++ [i])) lui)

//Utility functions shared by the layout rules:
setToBeInserted_ :: LUI -> LUI
setToBeInserted_ (LUINode type attr items changes effects) = LUINode type attr items {noChanges & toBeInserted = True} effects

setToBeAdded_ :: LUINo LUI -> LUI
setToBeAdded_ ruleNo (LUINode type attr items changes effects) = LUINode type attr items changes {effects & additional = ESToBeApplied ruleNo}

nextShiftID_ :: [LUI] -> LUIShiftID
nextShiftID_ items = maximum [-1:map shiftID items] + 1
where
	shiftID (LUINode _ _ _ {toBeShifted=Just x} _) = x
	shiftID (LUIShiftDestination x) = x
	shiftID _ = -1

nextMoveID_ :: LUIMoves -> LUIMoveID
nextMoveID_ moves = (foldr max 0 ('DM'.keys moves)) + 1

//Test if a certain node exists at the time of rule application
nodeExists_ :: !LUINo !LUI LUIMoves -> Bool
//Upstream nodes that no longer exist (here)
nodeExists_ ruleNo (LUINode _ _ _ {toBeRemoved=True} _) moves = False
nodeExists_ ruleNo (LUINode _ _ _ {toBeShifted=Just _} _) moves = False
//Nodes that were hidden by effects
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESToBeApplied hiddenBy}) moves = hiddenBy >= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESApplied hiddenBy}) moves = hiddenBy >= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESToBeUpdated _ hiddenBy}) moves = hiddenBy >= ruleNo
//Nodes that were introduced by effects
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESToBeApplied addedBy}) moves = addedBy <= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESApplied addedBy}) moves = addedBy <= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESToBeRemoved _}) moves = False //Marked to be removed

nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESToBeApplied wrappedBy}) moves | wrappedBy > ruleNo 
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESApplied wrappedBy}) moves | wrappedBy > ruleNo 
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESToBeRemoved wrappedBy}) moves //No longer wrapped
	= case scanToPosition_ wrappedBy 0 items moves of //Consider the wrapped child (that will be restored)
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {unwrapped=ESToBeApplied unwrappedBy}) moves | unwrappedBy <= ruleNo
	= case scanToPosition_ unwrappedBy 0 items moves of
		(_,_,Just unwrapped) = nodeExists_ ruleNo unwrapped moves
		_ = False
nodeExists_ ruleNo (LUINode _ _ items _ {unwrapped=ESApplied unwrappedBy}) moves | unwrappedBy <= ruleNo
	= case scanToPosition_ unwrappedBy 0 items moves of
		(_,_,Just unwrapped) = nodeExists_ ruleNo unwrapped moves
		_ = False
//Moved nodes
nodeExists_ ruleNo (LUIMoveSource moveId) moves
	= case getMovedNode_ moveId moves of
		//Already moved (so does not exist at the source)
		(ESToBeApplied movedBy, LUINode _ _ _ _ _) | ruleNo > movedBy = False
		(ESApplied movedBy, LUINode _ _ _ _ _) | ruleNo > movedBy = False
		(ESToBeUpdated _ movedBy, LUINode _ _ _ _ _) | movedBy == ruleNo = False
		(ESToBeUpdated _ movedBy, LUINode _ _ _ _ _) | ruleNo > movedBy = False
		(_,lui) = nodeExists_ ruleNo lui moves //Check the dereference node
nodeExists_ ruleNo (LUIMoveDestination moveId moveRule) moves
	= case getMovedNode_ moveId moves of
		//Not yet moved (so does not exist at the destination)
		(ESToBeApplied movedBy, LUINode _ _ _ _ _) | ruleNo <= movedBy = False
		(ESApplied movedBy, LUINode _ _ _ _ _) | ruleNo <= movedBy = False
		(ESToBeUpdated _ movedBy, LUINode _ _ _ _ _) | movedBy == ruleNo = True 
		(ESToBeUpdated _ movedBy, LUINode _ _ _ _ _) | ruleNo < movedBy = False
		(_,lui) = nodeExists_ ruleNo lui moves //Check the dereference node
nodeExists_ _ _ _ = True

//Test if a node matches a selection UI at a path is in the selection
nodeSelected_ :: LUINo UISelection UIPath LUI LUIMoves -> Bool
nodeSelected_ ruleNo (SelectByPath p) path _ moves = p === path
nodeSelected_ ruleNo (SelectByDepth n) p _ moves = length p == n
nodeSelected_ ruleNo (SelectDescendents) [_:_] _ moves = True
nodeSelected_ ruleNo (SelectDescendents) _ _ moves = False
nodeSelected_ ruleNo (SelectByType t) _ lui moves = fromMaybe False
	(fmap (\n -> nodeType_ ruleNo n === t) (selectNode_ ruleNo True fst (lui,moves)))
nodeSelected_ ruleNo (SelectByHasAttribute k) _ lui moves = fromMaybe False
	(fmap (\n -> isJust ('DM'.get k (nodeAttributes_ ruleNo n))) (selectNode_ ruleNo True fst (lui,moves)))
nodeSelected_ ruleNo (SelectByAttribute k p) _ lui moves = fromMaybe False
	(fmap (\n -> maybe False p ('DM'.get k (nodeAttributes_ ruleNo n)))	(selectNode_ ruleNo True fst (lui,moves)))
nodeSelected_ ruleNo (SelectByNumChildren num) _ lui moves = fromMaybe False
	//TODO: selectChildNodes should also have selectable before/after effect
	(fmap (\(LUINode _ _ items _ _,m) -> length (selectChildNodes_ ruleNo (items,m)) == num)
		(selectNode_ ruleNo True id (lui,moves)))
nodeSelected_ ruleNo (SelectByContains selection) path lui moves
	= fromMaybe False (selectNode_ ruleNo True
		(\(x=:LUINode _ _ items _ _,m) = nodeSelected_ ruleNo selection path x m
	                                  || or [nodeSelected_ ruleNo (SelectByContains selection) (path ++ [i]) item m
	                                        \\ item <- selectChildNodes_ ruleNo (items,m) & i <- [0..]])
		(lui,moves))

nodeSelected_ ruleNo (SelectRelative prefix sel) absolutePath lui moves
	= maybe False (\relativePath -> nodeSelected_ ruleNo sel relativePath lui moves) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing
nodeSelected_ ruleNo (SelectNone) _ _ moves = False
nodeSelected_ ruleNo (SelectAND sell selr) path ui moves = nodeSelected_ ruleNo sell path ui moves && nodeSelected_ ruleNo selr path ui moves
nodeSelected_ ruleNo (SelectOR sell selr) path ui moves = nodeSelected_ ruleNo sell path ui moves || nodeSelected_ ruleNo selr path ui moves
nodeSelected_ ruleNo (SelectNOT sel) path ui moves = not (nodeSelected_ ruleNo sel path ui moves)
nodeSelected_ ruleNo _ _ _ moves = False

matchAttributeKey_ :: UIAttributeSelection UIAttributeKey -> Bool
matchAttributeKey_ (SelectAll) _ = True
matchAttributeKey_ (SelectKeys keys) k = isMember k keys

selectAttributes_ :: UIAttributeSelection UIAttributes -> UIAttributes
selectAttributes_ selection attr = case selection of
	SelectAll         = attr
	(SelectKeys keys) = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

//Lookup an item by position in a list set of children.
//This lookup takes into consideration that it is possible that:
//- items were removed or shifted by an upstream change
//- items have been hidden by earlier rules
//- items were added by earlier rules
//It ignores nodes that are not relevant such as:
//- items that were added by later rules

scanToPosition_ :: LUINo Int [LUI] LUIMoves -> (Int,Bool,Maybe LUI)
scanToPosition_ ruleNo position items moves = scan position 0 items
where
	scan r i [] = (i, r == 0, Nothing) //Scanned beyond the list
	scan r i [x:xs]
		| not (nodeExists_ ruleNo x moves) = scan r (i + 1) xs //Skip
		| r == 0 = (i,True,Just x)
		| otherwise = scan (r - 1) (i + 1) xs

isAdditional_ :: LUI -> Bool
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}) = True
isAdditional_ (LUIMoveDestination _ _) = True
isAdditional_ _ = False

isShifted_ :: LUI -> Bool
isShifted_ (LUINode _ _ _ {LUIChanges|toBeShifted} _) = isJust toBeShifted
isShifted_ _ = False

isUnwrapped_ :: LUI -> Bool
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESToBeApplied _}) = True
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESApplied _}) = True
isUnwrapped_ _ = False

isHidden_ :: LUI -> Bool
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESToBeApplied _}) = True
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESApplied _}) = True
isHidden_ _ = False

lookupShiftSource_ :: Int [LUI] -> (Int,LUI)
lookupShiftSource_ shiftId items = lookup 0 items 
where
	lookup _ [] = abort "lookupShiftSource_: could not find source"
	lookup i [x:xs] = if (isSource x) (i,x) (lookup (i+1) xs) 
		
	isSource (LUINode _ _ _ {toBeShifted = Just sourceId} _) = sourceId == shiftId
	isSource _ = False

selectNode_ :: LUINo Bool ((LUI,LUIMoves) -> a) (LUI,LUIMoves) -> Maybe a
selectNode_ ruleNo beforeEffect selector (lui,moves) = fst3 (processNode_ ruleNo beforeEffect fun (lui,moves))
where
	fun (lui,moves) = (selector (lui,moves),lui,moves)

selectChildNodes_ :: LUINo ([LUI],LUIMoves) -> [LUI] 
selectChildNodes_ ruleNo (items,moves)
	# (selection,_,moves) = processChildNodes_ ruleNo fun ([],items,moves)
	= map snd (sortBy fstEl selection)
where
	fun i (acc,item,moves) = ([(i,item):acc],item,moves)
	fstEl (x,_) (y,_) = x < y	

selectSubNode_ :: LUINo UIPath (LUI,LUIMoves) -> Maybe LUI 
selectSubNode_ ruleNo [] (lui,moves)= selectNode_ ruleNo False fst (lui,moves)
selectSubNode_ ruleNo [s:ss] (lui,moves) = maybe Nothing id (selectNode_ ruleNo False select (lui,moves))
where
	select (LUINode type attr items changes effects,moves) 
		# items = selectChildNodes_ ruleNo (items,moves)
		| s < 0 || s >= length items = Nothing
		| otherwise = selectSubNode_ ruleNo ss (items !! s, moves)

updateNode_ :: LUINo ((LUI,LUIMoves) -> (LUI,LUIMoves)) (LUI,LUIMoves) -> (LUI,LUIMoves)
updateNode_ ruleNo update (lui,moves)
	# (_,lui,moves) = processNode_ ruleNo False fun (lui,moves)
	= (lui,moves)
where
	fun (item,moves) 
		# (item,moves) = update (item,moves)
		= ((),item,moves)

updateChildNodes_ :: LUINo (Int (LUI,LUIMoves) -> (LUI,LUIMoves)) ([LUI],LUIMoves) -> ([LUI],LUIMoves)
updateChildNodes_ ruleNo update (items,moves)
	# (_,items,moves) = processChildNodes_ ruleNo fun ((),items,moves)
	= (items, moves)
where
	fun i (s,items,moves) 
		# (items,moves) = update i (items,moves)
		= (s,items,moves)

updateSubNode_ :: LUINo UIPath ((LUI,LUIMoves) -> (LUI,LUIMoves)) (LUI,LUIMoves) -> (LUI,LUIMoves)
updateSubNode_ ruleNo [] update (lui,moves) = updateNode_ ruleNo update (lui,moves)
updateSubNode_ ruleNo [s:ss] update (lui,moves) = updateNode_ ruleNo apply (lui,moves)
where
	apply (LUINode type attr items changes effects,moves)
		# (items,moves) = updateChildNodes_ ruleNo applyc (items,moves)
		= (LUINode type attr items changes effects,moves)

	applyc i (lui,moves) = if (i == s) (updateSubNode_ ruleNo ss update (lui,moves)) (lui,moves)

processNode_ :: LUINo Bool ((LUI,LUIMoves) -> (a,LUI,LUIMoves)) (LUI,LUIMoves) -> (Maybe a, LUI, LUIMoves)
processNode_ ruleNo beforeEffect fun (lui,moves) = case lui of
	//When a move source exists (it is moved by a later rule), we update the referenced node
	(LUIMoveSource moveId)
		# (movedStage,movedItem) = getMovedNode_ moveId moves
		= case getMovedBy_ movedStage of
			Just movedBy 
				| movedBy >= ruleNo
					# (result,movedItem,moves) = fun (movedItem,moves)
					= (Just result,lui, putMovedNode_ moveId (movedStage,movedItem) moves)
				| otherwise //Do nothing, the node has been moved somewhere else
					= (Nothing,lui,moves)
			//The node is no longer moved, it will be restored to this location
			Nothing
				# (result,movedItem,moves) = fun (movedItem,moves)
				= (Just result ,lui, putMovedNode_ moveId (movedStage,movedItem) moves)
	//When an item was moved here (by an earlier rule), we update the referenced node
	(LUIMoveDestination moveId moveRule) 
		# (movedStage,movedItem) = getMovedNode_ moveId moves
		= case getMovedBy_ movedStage of
			Just movedBy 
				| movedBy < ruleNo //The node has been moved, Update the referenced node
					# (result,movedItem,moves) = fun (movedItem,moves)
					= (Just result, lui, putMovedNode_ moveId (movedStage,movedItem) moves)
				| otherwise //No nothing, the node has not been moved yet
					= (Nothing,lui,moves)
			//The node is no longer moved, it will be removed from this destination
			Nothing = (Nothing,lui,moves)
	//When an item is scheduled to be replaced, update the replacement
	(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		# (result,replacement,moves) = processNode_ ruleNo beforeEffect fun (replacement,moves)
		= (result, LUINode type attr items {changes & toBeReplaced=Just replacement} effects, moves)
	//When an item is wrapped by a later rule, we update the wrapped child instead of the wrapper 
	(LUINode type attr items changes effects=:{wrapper=ESApplied wrappedBy})
		| wrappedBy > ruleNo || beforeEffect && isPartOf_ wrappedBy ruleNo
			= processFirstChild_ wrappedBy ruleNo beforeEffect fun (lui,moves)
	//When an item is wrapped by a later rule, we update the wrapped child instead of the wrapper
	(LUINode type attr items changes effects=:{wrapper=ESToBeApplied wrappedBy})
		| wrappedBy > ruleNo || beforeEffect && isPartOf_ wrappedBy ruleNo
			= processFirstChild_ wrappedBy ruleNo beforeEffect fun (lui,moves)
	//Similarly, when the wrapping is set to be removed, we need to update the wrapped child
	(LUINode type attr items changes effects=:{wrapper=ESToBeRemoved wrappedBy})
		= processFirstChild_ wrappedBy ruleNo beforeEffect fun (lui,moves)
	//When a node is unwrapped, we have to process the first inner node (if it exists...)
	(LUINode type attr items changes effects=:{unwrapped=ESToBeApplied unwrappedBy}) | unwrappedBy < ruleNo
		//TODO: Consider beforeEffect case..
		= processFirstChild_ ruleNo ruleNo beforeEffect fun (lui,moves)
	(LUINode type attr items changes effects=:{unwrapped=ESApplied unwrappedBy}) | unwrappedBy < ruleNo
		= processFirstChild_ ruleNo ruleNo beforeEffect fun (lui,moves)
	//Default case: Just apply the update function
	_ 
		# (result,lui,moves) = fun (lui,moves)
		= (Just result,lui,moves)
where
	processFirstChild_ atRuleNo ruleNo beforeEffect fun (lui=:(LUINode type attr items changes effects),moves)
		= case scanToPosition_ atRuleNo 0 items moves of
			(index,True,Just wrapped)
				# (result,wrapped,moves) = processNode_ ruleNo beforeEffect fun (wrapped,moves)
				= (result, LUINode type attr (updateAt index wrapped items) changes effects, moves)
			_ = (Nothing,lui,moves)

processChildNodes_ :: LUINo (Int (a,LUI,LUIMoves) -> (a,LUI,LUIMoves)) (a,[LUI],LUIMoves) -> (a,[LUI],LUIMoves) 
processChildNodes_ ruleNo fun (state,items,moves) = processItems (indexShiftDestinations items moves) state items moves
where
	indexShiftDestinations items moves = snd (foldl index (0,'DM'.newMap) items) 
	where
		index (i,positions) item
			| nodeExists_ ruleNo item moves = case item of
				(LUIShiftDestination shiftId) = (i + 1, 'DM'.put shiftId i positions)
				_                             = (i + 1, positions)
			| otherwise                                  = (i, positions)

	processItems shiftDestinations state items moves
		# (_,state,items,moves) = foldl updateItem (0,state,[],moves) items
		= (state,reverse items, moves)
	where
		updateItem (i,state,acc,moves) item 
			| nodeExists_ ruleNo item moves
				# (state,item,moves) = case item of 
					//For shifted items we update the source
					(LUIShiftDestination shiftId) = (state,item,moves)
					_                             = fun i (state,item,moves)
				= (i + 1, state, [item:acc], moves)
			| otherwise
				# (state,item,moves) = case item of
					//For shifted items we update the source (using the index of the destination)
					(LUINode _ _ _ {toBeShifted=Just shiftId} _)
						//If we did not index the destination, apparently it no longer exists
						= maybe (state,item,moves) (\index -> fun index (state,item,moves)) ('DM'.get shiftId shiftDestinations)
					_ = (state,item,moves)
				= (i, state,[item:acc], moves)

getMovedNode_ :: LUIMoveID LUIMoves -> (LUIEffectStage LUINo,LUI)
getMovedNode_ moveId moves = case 'DM'.get moveId moves of
	Nothing = abort ("Get: Unknown id " +++ toString moveId +++ " in moved items\n")
	Just item = item

putMovedNode_ :: LUIMoveID (LUIEffectStage LUINo,LUI) LUIMoves -> LUIMoves
putMovedNode_ moveId node moves = 'DM'.put moveId node moves

delMovedNode_ :: LUIMoveID LUIMoves -> LUIMoves
delMovedNode_ moveId moves = 'DM'.del moveId moves

getMovedBy_ :: (LUIEffectStage LUINo) -> Maybe LUINo
getMovedBy_ (ESApplied ruleNo) = Just ruleNo
getMovedBy_ (ESToBeApplied ruleNo) = Just ruleNo
getMovedBy_ (ESToBeUpdated _ ruleNo) = Just ruleNo
getMovedBy_ _ = Nothing

nodeType_ :: LUINo LUI -> UIType
nodeType_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESToBeApplied (appliedAt,type)}) | appliedAt < ruleNo = type
nodeType_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESApplied (appliedAt,type)}) | appliedAt < ruleNo = type
nodeType_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESToBeUpdated _ (appliedAt,type)}) | appliedAt < ruleNo = type
nodeType_ ruleNo (LUINode type _ _ _ _) = type

nodeAttributes_ :: LUINo LUI -> UIAttributes
nodeAttributes_ ruleNo (LUINode _ attr _ changes effects=:{overwrittenAttributes,hiddenAttributes})
	//Consider upstream changes
	# attr = applyAttributeChanges_ changes attr 
	//Consider overwritten attributes
	# attr = foldl overwrite attr ('DM'.toList overwrittenAttributes)
	//Consider hidden attributes
	# attr = foldl hide attr ('DM'.toList hiddenAttributes)
	= attr	
where
	overwrite attr (key,ESToBeApplied (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr (key,ESApplied (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr (key,ESToBeUpdated _ (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr _ = attr

	hide attr (key,ESToBeApplied appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr (key,ESApplied appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr (key,ESToBeUpdated _ appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr _ = attr

applyTypeEffect_ :: UIType LUIEffects -> UIType
applyTypeEffect_ ltype effects=:{overwrittenType} = case overwrittenType of
	ESToBeApplied (_,type) = type
	ESApplied (_,type) = type
	ESToBeUpdated _ (_,type) = type
	_ = ltype

applyAttributeChanges_ :: LUIChanges UIAttributes -> UIAttributes 
applyAttributeChanges_ {setAttributes,delAttributes} attr
	= 'DM'.delList ('DS'.toList delAttributes) ('DM'.union setAttributes attr)

applyAttributeEffects_ :: UIAttributes LUIEffects -> UIAttributes
applyAttributeEffects_ attr effects=:{overwrittenAttributes,hiddenAttributes} 
	# attr = foldl overwrite attr ('DM'.toList overwrittenAttributes)
	# attr = foldl hide attr ('DM'.toList hiddenAttributes)
	= attr 
where
	overwrite attr (key,ESNotApplied) = attr
	overwrite attr (key,ESToBeApplied (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESApplied (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESToBeUpdated _ (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESToBeRemoved _) = attr

	hide attr (key,ESNotApplied) = attr
	hide attr (key,ESToBeApplied ruleNo) = 'DM'.del key attr
	hide attr (key,ESApplied ruleNo) = 'DM'.del key attr
	hide attr (key,ESToBeUpdated _ ruleNo) = 'DM'.del key attr
	hide attr (key,ESToBeRemoved _) = attr

overwriteAttribute_ :: LUINo UIAttribute (Map UIAttributeKey (LUIEffectStage (LUINo,JSONNode))) -> (Map UIAttributeKey (LUIEffectStage (LUINo,JSONNode)))
overwriteAttribute_ ruleNo (key,value) overwrittenAttributes
	# override = case 'DM'.get key overwrittenAttributes of
		//Not set yet
		Nothing = ESToBeApplied (ruleNo,value)
		Just (ESNotApplied) = ESToBeApplied (ruleNo,value)
		Just (ESToBeApplied (curRule,curValue))
			| curValue == value = ESToBeApplied (curRule,curValue)
			| otherwise         = ESToBeApplied (ruleNo,value)
		//Already set,
		Just (ESApplied (curRule,curValue))
			| curValue == value = ESApplied (curRule,curValue)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value) 
		Just (ESToBeUpdated (curRule,curValue) _)
			| curValue == value = ESApplied (ruleNo, value)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value)
		Just (ESToBeRemoved (curRule,curValue))
			| curValue == value = ESApplied (ruleNo,value)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value)
	= 'DM'.put key override overwrittenAttributes

hideAttribute_ :: LUINo (UIAttributeKey -> Bool) UIAttributeKey (Map UIAttributeKey (LUIEffectStage LUINo)) -> (Map UIAttributeKey (LUIEffectStage LUINo))
hideAttribute_ ruleNo condition key hiddenAttributes
	| isAlreadyHidden key hiddenAttributes
		= if (condition key)
			('DM'.put key (ESApplied ruleNo) hiddenAttributes)
			('DM'.put key (ESToBeRemoved ruleNo) hiddenAttributes)
	| otherwise
		= if (condition key)
			('DM'.put key (ESToBeApplied ruleNo) hiddenAttributes)
			hiddenAttributes
where
	isAlreadyHidden key attr = case 'DM'.get key attr of
		Just (ESApplied _) = True
		Just (ESToBeUpdated _ _) = True
		Just (ESToBeRemoved _) = True
		_ = False

//Undo the effects of a previously applied rule for a single node
undoEffects_ :: LUINo (LUI,LUIMoves) -> (LUI,LUIMoves)
undoEffects_ ruleNo (LUINode type attr items changes effects,moves)
	= (LUINode type attr items changes (undo ruleNo effects), moves)
where 
	undo ruleNo {overwrittenType,overwrittenAttributes,hiddenAttributes,additional,hidden,wrapper,unwrapped}
		= {overwrittenType = undoEffect_ ruleNo fst overwrittenType
		  ,overwrittenAttributes = fmap (undoEffect_ ruleNo fst) overwrittenAttributes
		  ,hiddenAttributes = fmap (undoEffect_ ruleNo id) hiddenAttributes
		  ,additional = undoEffect_ ruleNo id additional
		  ,hidden = undoEffect_ ruleNo id hidden
		  ,wrapper = undoEffect_ ruleNo id wrapper
		  ,unwrapped = undoEffect_ ruleNo id unwrapped
		  }

undoEffects_ ruleNo (ref=:(LUIMoveSource moveId),moves) = case 'DM'.get moveId moves of
	Nothing = (ref,moves)
	Just (moveStage,lui)
		# moveStage = undoEffect_ ruleNo id moveStage
		# (lui,moves) = undoEffects_ ruleNo (lui,moves)
		= (ref,'DM'.put moveId (moveStage,lui) moves)

undoEffects_ ruleNo (ref=:LUIMoveDestination moveId moveRule,moves) = case 'DM'.get moveId moves of
	Nothing = (ref,moves)
	Just (moveStage,lui)
		# moveStage = undoEffect_ ruleNo id moveStage
		# (lui,moves) = undoEffects_ ruleNo (lui,moves)
		= (ref,'DM'.put moveId (moveStage,lui) moves)

undoEffects_ ruleNo (LUIShiftDestination shiftId,moves) = (LUIShiftDestination shiftId,moves)

undoEffect_ :: LUINo (a -> LUINo) (LUIEffectStage a) -> LUIEffectStage a
undoEffect_ ruleNo f (ESToBeApplied x) | isPartOf_ (f x) ruleNo = ESNotApplied
undoEffect_ ruleNo f (ESApplied x) | isPartOf_ (f x) ruleNo = ESToBeRemoved x
undoEffect_ ruleNo f (ESToBeUpdated c x) | isPartOf_ (f c) ruleNo = ESToBeRemoved c
undoEffect_ ruleNo f (ESToBeUpdated c x) | isPartOf_ (f x) ruleNo = ESApplied c
undoEffect_ _ _ es = es

//Check if x is a sub-rule of y
isPartOf_ :: LUINo LUINo -> Bool
isPartOf_ (LUINo x) (LUINo y) = check x y
where
	check _ [] = True
	check [] _ = False
	check [x:xs] [y:ys] = if (x == y) (check xs ys) False

/*
* Once layout rules have annotated their effects, the change that has to be applied downstream
* can be extracted from the buffered structure. The result of doing this is both the combination of
* the ui change that has to be applied downstream, and a new version of the buffered tree that
* in which all pending changes and effects have been applied.
*/
extractDownstreamChange :: (LUI,LUIMoves) -> (!UIChange,!(LUI,LUIMoves))
extractDownstreamChange (lui,moves) 
	# mbChildChange = extractDownstreamChildChange lui moves
	# (mbLui,moves)  = confirmChangesAndEffects_ (lui, moves)
	= case (mbChildChange,mbLui) of
		(Just (InsertChild ui), Just lui)     = (ReplaceUI ui, cleanupState_ (lui,moves))
		(Just RemoveChild, Just lui)          = (ReplaceUI (UI UIEmpty 'DM'.newMap []), cleanupState_ (lui,moves))
		(Just (ChangeChild change), Just lui) = (change, cleanupState_ (lui,moves))
		(Nothing, Just lui)                   = (NoChange, cleanupState_ (lui,moves))
		_ = abort "extractDownstreamChange: at the top-level, an lui should always be returned"


//For each node we need to extract one of the following changes:
// 1. Just (InsertChild x)        - The node did not exist client-side, but does now
// 2. Just (RemoveChild)          - The node existed previously but should not
// 3. Just (ChangeChild x)        - The node existed previously and still does
// 4. Nothing                     - The node did not exist client-side and still does not...
extractDownstreamChildChange :: LUI LUIMoves -> Maybe UIChildChange
extractDownstreamChildChange lui moves
	# (mbMoveId,mbDestinationRule,(mbMovedStage,current=:LUINode _ _ items changes effects)) = case lui of
		(LUINode _ _ _ _ _)                  = (Nothing, Nothing,(Nothing,lui))
		(LUIMoveSource moveId)               = (Just moveId, Nothing, appFst Just (getMovedNode_ moveId moves))
		(LUIMoveDestination moveId moveRule) = (Just moveId, Just moveRule, appFst Just (getMovedNode_ moveId moves))
	| isTemporaryDestination_ mbMovedStage mbDestinationRule
		= Nothing
	//Cases where a new ui should be inserted
	| needsInsert_ items changes effects mbMovedStage mbDestinationRule
		= determineInsert_ lui current moves
	//Cases where we need a full replace
	| needsRemove_ changes effects mbMovedStage mbDestinationRule
		= determineRemove_ lui current mbMovedStage moves
	//Cases where a full replace is needed
	| needsReplace_ items changes effects
		= determineReplace_ lui current mbMovedStage moves
	| otherwise
		= determineChange_ lui current mbMovedStage moves
where				
	isTemporaryDestination_ (Just (ESToBeUpdated curRule newRule)) (Just destRule) = destRule <> curRule && destRule <> newRule
	isTemporaryDestination_ (Just (ESApplied newRule)) (Just destRule) = destRule <> newRule
	isTemporaryDestination_ _ _ = False

	needsInsert_ _ {toBeInserted=True} _ _ _ = True
	needsInsert_ _ _ {additional=ESToBeApplied _} _ _ = True
	needsInsert_ _ _ {hidden=ESToBeRemoved _} _ _ = True

	//If an item is newly wrapped we check if the wrapped item needed an insert
	needsInsert_ items _ {wrapper=ESToBeApplied wrappedBy} _ _
		= case scanToPosition_ wrappedBy 0 items moves of
			(_,_,Just (LUINode _ _ items changes effects)) = needsInsert_ items changes effects Nothing Nothing
			_ = False

	needsInsert_ _ _ _ (Just (ESToBeApplied _)) (Just _) = True
	needsInsert_ _ _ _ (Just (ESToBeRemoved _)) Nothing = True
	needsInsert_ _ _ _ (Just (ESToBeUpdated _ ruleNo)) (Just destNo) = ruleNo == destNo
	needsInsert_ _ _ _ _ _ = False

	needsRemove_ {toBeRemoved=True} _ _ _ = True
	needsRemove_ _ {additional=ESToBeRemoved _} _ _ = True
	needsRemove_ _ {hidden=ESToBeApplied _} _ _ = True

	needsRemove_ _ _ (Just (ESToBeApplied _)) Nothing = True
	needsRemove_ _ _ (Just (ESToBeRemoved _)) (Just _) = True
	needsRemove_ _ _ (Just (ESToBeUpdated ruleNo _)) (Just destNo) = ruleNo == destNo
	needsRemove_ _ _ _ _ = False	

	needsReplace_ _ {toBeReplaced=Just _} _ = True
	needsReplace_ _ _ {overwrittenType=ESToBeApplied _} = True
	needsReplace_ _ _ {overwrittenType=ESToBeUpdated _ _} = True
	needsReplace_ _ _ {overwrittenType=ESToBeRemoved _} = True
	needsReplace_ _ _ {wrapper=ESToBeApplied _} = True
	needsReplace_ _ _ {wrapper=ESToBeRemoved _} = True
	needsReplace_ _ _ {unwrapped=ESToBeApplied _} = True
	needsReplace_ _ _ {unwrapped=ESToBeRemoved _} = True
	needsReplace_ items _ {unwrapped=ESApplied _} = differentFirstChild items
	needsReplace_ _ _ _ = False

	//When the parent is unwrapped, we may need to replace the ui if another child ends up at position 0
	differentFirstChild [LUINode _ _ _ {toBeInserted=True} _:_]= True
	differentFirstChild [LUINode _ _ _ {toBeRemoved=True} _:_]= True
	differentFirstChild [LUINode _ _ _ {toBeShifted=Just _} _:_]= True
	differentFirstChild [LUIShiftDestination _:_]= True
	differentFirstChild items = False

	existsDownstream_ _ (LUINode _ _ _ _ {LUIEffects|hidden=ESApplied _}) _ _ = False
	existsDownstream_ _ (LUINode _ _ _ _ {LUIEffects|hidden=ESToBeRemoved _}) _ _ = False
	existsDownstream_ (LUIMoveSource _) (LUINode _ _ _ _ _) (Just (ESApplied _)) _ = False
	existsDownstream_ (LUIMoveSource _) (LUINode _ _ _ _ _) (Just (ESToBeUpdated _ _)) _ = False
	existsDownstream_ ref (LUINode _ _ items _ {LUIEffects|unwrapped=ESApplied unwrappedBy}) mbMovedStage moves 
		= case scanToPosition_ unwrappedBy 0 items moves of
			(_,_,Just item) = existsDownstream_ ref item mbMovedStage moves
			_                    = False
	existsDownstream_ _ _ _ _ = True

 	determineInsert_ node current moves = fmap InsertChild (extractUIWithEffects_ (current,moves))

	determineRemove_ node current=:(LUINode _ _ _ changes effects) mbMovedStage moves	
		| existsDownstream_ node current mbMovedStage moves = Just RemoveChild
		| otherwise = Nothing

	determineReplace_ node current mbMovedStage moves
		= case extractUIWithEffects_ (current,moves) of
			(Just ui)
				| existsDownstream_ node current mbMovedStage moves = Just (ChangeChild (ReplaceUI ui))
				| otherwise                 = Just (InsertChild ui)
			(Nothing)
				| existsDownstream_ node current mbMovedStage moves = Just RemoveChild
				| otherwise = Nothing

	determineChange_ node current=:(LUINode type attr items changes effects=:{unwrapped}) mbMovedStage moves
		//Determine changes to attributes
		# attributeChanges = extractAttributeChanges attr changes effects
		//Determine changes to children
		# (childShifts,items) = extractChildShifts items
		# childUpdates = extractChildInsertsAndRemoves items moves
		//Combine into one change
		# change = if (unwrapped =: (ESApplied _))
			//Check if there is an update to the first child
			(case [c \\ (i,ChangeChild c) <- childUpdates] of
				[c:_] = c
				_     = NoChange
			)
			(case (attributeChanges,childShifts,childUpdates) of
				([],[],[]) = NoChange
				_          = ChangeUI attributeChanges (childShifts ++ childUpdates)
			)
		| existsDownstream_ node current mbMovedStage moves
			= Just (ChangeChild change)
		| otherwise
			= Nothing

extractAttributeChanges :: UIAttributes LUIChanges LUIEffects -> [UIAttributeChange]
extractAttributeChanges attr changes=:{setAttributes,delAttributes} effects=:{overwrittenAttributes,hiddenAttributes}
	//Apply changes to the attributes
	# changes = foldl (applySetAttribute overwrittenAttributes hiddenAttributes) [] ('DM'.toList setAttributes)
	# changes = foldl (applyDelAttribute overwrittenAttributes hiddenAttributes) changes ('DS'.toList delAttributes)
	//Apply remaining effects (these no longer affect the stored attributes)
	# changes = foldl (applyOverrideAttribute attr) changes ('DM'.toList overwrittenAttributes)
	# changes = foldl (applyHideAttribute attr) changes ('DM'.toList hiddenAttributes)
	= reverse changes
where
	applySetAttribute overwrittenAttributes hiddenAttributes changes (key,value)
		//If an attribute has an effect applied, we don't want to change it downstream
		| isOverwritten key overwrittenAttributes || isHidden key hiddenAttributes = changes
		| otherwise = [SetAttribute key value:changes]
	applyDelAttribute overwrittenAttributes hiddenAttributes changes key
		//If an attribute was overwritten, we don't want to delete it downstream
		//If an attribute is hidden we don't need to delete it downstream (it is not shown there)
		| isOverwritten key overwrittenAttributes || isHidden key hiddenAttributes = changes
		| otherwise = [DelAttribute key:changes]

	applyOverrideAttribute attr changes (key,ESNotApplied) = changes 
	applyOverrideAttribute attr changes (key,ESToBeApplied (ruleNo,value))
		= [SetAttribute key value:changes]
	applyOverrideAttribute attr changes (key,ESApplied (ruleNo,value)) = changes //Already applied
	applyOverrideAttribute attr changes (key,ESToBeUpdated _ (ruleNo,value))
		= [SetAttribute key value:changes]
	applyOverrideAttribute attr changes (key,ESToBeRemoved _) //Either restore the original, or remove the attribute 
		= maybe [DelAttribute key:changes] (\value -> [SetAttribute key value:changes]) ('DM'.get key attr)

	applyHideAttribute attr changes (key,ESNotApplied) = changes
	applyHideAttribute attr changes (key,ESToBeApplied ruleNo)
		= [DelAttribute key:changes]
	applyHideAttribute attr changes (key,ESApplied ruleNo) = changes
	applyHideAttribute attr changes (key,ESToBeUpdated _ ruleNo) = changes
	applyHideAttribute attr changes (key,ESToBeRemoved _)
		= maybe changes (\value -> [SetAttribute key value:changes]) ('DM'.get key attr)

	isHidden key hiddenAttributes = check ('DM'.toList hiddenAttributes)
	where
		check [] = False
		check [(hiddenKey,ESToBeApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESToBeUpdated _ _):_] | hiddenKey == key = True
		check [_:xs] = check xs

	isOverwritten key overwrittenAttributes = check ('DM'.toList overwrittenAttributes)
	where
		check [] = False
		check [(hiddenKey,ESToBeApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESToBeUpdated _ _):_] | hiddenKey == key = True
		check [_:xs] = check xs

//Important: Shifts are done before inserts and removes
//           so we ignore items that are not yet inserted, but still
//           count items that are to be removed
extractChildShifts :: [LUI] -> ([(Int,UIChildChange)],[LUI])
extractChildShifts items = extract 0 [] items
where
	extract i acc [] = ([],reverse acc)
	extract i acc [x=:(LUINode _ _ _ {toBeShifted = Just shiftID} _):xs]
		//First look back for the destination
		= case findAndReplaceDestination shiftID x True acc of
			(Left d, acc)
				# (changes,items) = extract (i + 1) acc xs
				= ([(i,MoveChild d):changes],items)
			(Right n, acc)
				//Look forward for the destination
				= case findAndReplaceDestination shiftID x False xs of
					(Left d, xs)
						# (changes,items) = extract i acc xs
						= ([(i,MoveChild (n + d)):changes],items)
					(Right _, xs)
						= abort "Could not find a destination for a shifted UI element"
	//Ignore not yet inserted nodes and shift destinations
	extract i acc [x=:(LUINode _ _ _ {toBeInserted=True} _):xs] = extract i [x:acc] xs
	extract i acc [x=:(LUIShiftDestination _):xs] = extract i [x:acc] xs
	//Continue
	extract i acc [x:xs] = extract (i + 1) [x:acc] xs

	findAndReplaceDestination shiftID x backwards items = find start items
	where
		numItems = adjustedLength items
		start = if backwards numItems 0

		find i [] = (Right numItems,[])
		find i [(LUIShiftDestination matchId):xs] | matchId == shiftID
			# x = resetToBeShifted x
			= (Left i,[x:xs])
		//Ignore not yet inserted nodes and shift destinations
		find i [x=:(LUIShiftDestination _):xs]
			# (mbd,xs) = find i xs
			= (mbd,[x:xs])
		find i [x=:(LUINode _ _ _ {toBeInserted=True} _):xs]
			# (mbd,xs) = find i xs
			= (mbd,[x:xs])
		//Just keep searching
		find i [x:xs]
			# (mbd,xs) = find (if backwards (i - 1) (i + 1)) xs
			= (mbd,[x:xs])

		adjustedLength items = count visible items
		where
			count pred list = foldr (\x n -> if (pred x) (n + 1) n) 0 list

			visible (LUIShiftDestination _) = False
			visible (LUINode _ _ _ {toBeInserted=True} _) = False
			visible _ = True

	resetToBeShifted (LUINode type attr items changes effects)
		= LUINode type attr items {changes & toBeShifted = Nothing} effects

extractChildInsertsAndRemoves :: [LUI] LUIMoves -> [(Int,UIChildChange)]
extractChildInsertsAndRemoves items moves = extract 0 items moves
where
	extract i [] moves = []
	extract i [x:xs] moves = case extractDownstreamChildChange x moves of
		(Just (InsertChild ui)) = [(i,InsertChild ui):extract (i + 1) xs moves]
		(Just RemoveChild) = [(i,RemoveChild):extract i xs moves]
		(Just (ChangeChild NoChange)) = extract (i + 1) xs moves
		(Just (ChangeChild c)) = [(i,ChangeChild c):extract (i + 1) xs moves]
		Nothing = extract i xs moves

extractUIWithEffects_ :: (LUI,LUIMoves) -> Maybe UI
extractUIWithEffects_ (LUINode ltype lattr litems changes=:{toBeReplaced=Just replacement} effects, moves)
	= extractUIWithEffects_ (replacement,moves)
extractUIWithEffects_ (LUINode ltype lattr litems changes effects=:{wrapper=ESToBeRemoved _}, moves)
	= case dropWhile isAdditional_ litems of
		[wrapped:_] = extractUIWithEffects_ (wrapped,moves)
		_           = abort "extractUIWithEffects_: Wrapped item is missing"
extractUIWithEffects_ (lui=:LUINode ltype lattr litems changes=:{setAttributes,delAttributes} effects=:{overwrittenType},moves)
	//Determin type
	# type = applyTypeEffect_ ltype effects
	//Determine attributes and apply attribute effects
	# attr = applyAttributeEffects_ (applyAttributeChanges_ changes lattr) effects
	//Recursively extract children with effects 
	# items = extractChildUIsWithEffects_ litems moves
	//Determine the final ui
	| isHidden_ lui = Nothing
	| isUnwrapped_ lui = case items of
		[item:_] = Just item
		_        = Nothing
	| otherwise = Just (UI type attr items)

extractUIWithEffects_ _ = abort "extractUIWithEffects_: can only extract UI from LUINodes"

extractChildUIsWithEffects_ :: [LUI] LUIMoves -> [UI]
extractChildUIsWithEffects_ litems moves = foldr extract [] litems
where
	shiftSources = foldr collect 'DM'.newMap litems
	where
		collect n=:(LUINode type attr items changes=:{toBeShifted=Just shiftId} effects) sources
			= 'DM'.put shiftId (LUINode type attr items {changes & toBeShifted = Nothing} effects) sources
		collect n sources = sources

	//Move shifted items to the right place
	extract (LUIShiftDestination shiftID) items
		= maybe items (\item -> extract item items) ('DM'.get shiftID shiftSources)
	//Skip over items that still need to be shifted
	extract litem=:(LUINode _ _ _ {toBeShifted=Just _} _) items
		= items
	//Items that need to be removed
	extract litem=:(LUINode _ _ _ {LUIChanges|toBeRemoved=True} _) items
		= items
	extract litem=:(LUINode _ _ _ {toBeReplaced=Nothing} {additional = ESToBeRemoved _}) items
		= items
	//Dealing with moved nodes
	extract litem=:(LUIMoveSource moveId) items
		= case getMovedNode_ moveId moves of
			(ESToBeRemoved ruleId, movedItem=:(LUINode _ _ _ _ _)) //Restore (destination still exists)
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			(_,movedItem)
				= items

	extract litem=:(LUIMoveDestination moveId moveRule) items
		= case getMovedNode_ moveId moves of
			(ESToBeApplied ruleId, movedItem=:(LUINode _ _ _ _ _))
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			(ESApplied ruleId,movedItem=:(LUINode _ _ _ _ _))
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			(ESToBeUpdated previousId ruleId,movedItem=:(LUINode _ _ _ _ _))
				| moveRule == previousId //Old destination, don't include it here
					= items
				| moveRule == ruleId //New destination, include it here
					# mbItem = extractUIWithEffects_ (movedItem,moves)
					= maybe items (\item -> [item:items]) mbItem
				| otherwise
					= items
			(ESToBeRemoved ruleId,movedItem=:(LUINode _ _ _ _ _)) //To be be restored
				= items
				
	extract litem items
		# mbItem = extractUIWithEffects_ (litem,moves)
		= maybe items (\item -> [item:items]) mbItem

derive JSONEncode LUI, LUINo, LUIEffects, LUIChanges, LUIEffectStage, Set

confirmChangesAndEffects_ :: (LUI,LUIMoves) -> (Maybe LUI,LUIMoves)
confirmChangesAndEffects_ (LUINode _ _ _ {toBeReplaced=Just replacement} _, moves)
	= confirmChangesAndEffects_ (replacement,moves)
confirmChangesAndEffects_ (LUINode _ _ _ {toBeRemoved=True} _, moves)
	= (Nothing,moves)
confirmChangesAndEffects_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}, moves)
	= (Nothing,moves)
confirmChangesAndEffects_ (LUINode type attr items changes effects=:{wrapper=ESToBeRemoved wrappedBy}, moves)
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,True, Just wrapped) = confirmChangesAndEffects_ (wrapped,moves)
		_           = abort "confirmChangesAndEffects: Wrapped item is missing"
confirmChangesAndEffects_ (LUINode type attr items changes effects, moves)
	# attr = applyAttributeChanges_ changes attr
	# effects = confirmEffects_ effects
	# (items,moves) = confirmChildChangesAndEffects_ items moves
	= (Just (LUINode type attr items noChanges effects), moves)
confirmChangesAndEffects_ (ref=:(LUIMoveSource moveId),moves)
	= case 'DM'.get moveId moves of
		Nothing = (Nothing, 'DM'.del moveId moves)
		Just (stage,moved) 
			# (mbMoved,moves) = confirmChangesAndEffects_ (moved,moves)
			# stage = confirmEffect_ stage
			= case (mbMoved, confirmEffect_ stage) of
				(Just item, stage=:(ESApplied _))
					= (Just ref, 'DM'.put moveId (stage,item) moves)
				_		
					= (mbMoved, 'DM'.del moveId moves)

confirmChangesAndEffects_ (ref=:(LUIMoveDestination moveId moveRule),moves)
	= case 'DM'.get moveId moves of
		Just (ESApplied movedBy,_) | movedBy === moveRule = (Just ref,moves)
		Just (ESToBeApplied movedBy,_) | movedBy === moveRule = (Just ref,moves)
		Just (ESToBeUpdated _ movedBy,_) | movedBy === moveRule = (Just ref,moves)
		_ = (Nothing,moves)

confirmChildChangesAndEffects_ :: [LUI] LUIMoves -> ([LUI],LUIMoves)
confirmChildChangesAndEffects_ items moves = foldr confirm ([],moves) items
where
	shiftSources = foldr collect 'DM'.newMap items
	where
		collect (LUINode type attr items changes=:{toBeShifted=Just shiftId} effects) sources
			= 'DM'.put shiftId (LUINode type attr items {changes & toBeShifted=Nothing} effects) sources
		collect n sources = sources

	confirm (LUINode _ _ _ {toBeShifted=Just _} _) (items,moves) = (items,moves)
	confirm (LUIShiftDestination shiftId) (items,moves)
		= maybe (items,moves) (\item -> confirm item (items,moves)) ('DM'.get shiftId shiftSources)
	confirm item (items,moves)
		# (mbitem,moves) = confirmChangesAndEffects_ (item,moves)
		= (maybe items (\i -> [i:items]) mbitem, moves)

confirmEffects_ :: LUIEffects -> LUIEffects
confirmEffects_ {overwrittenType,overwrittenAttributes,hiddenAttributes,additional,hidden,wrapper,unwrapped}
	= {overwrittenType = confirmEffect_ overwrittenType
	  ,overwrittenAttributes = confirmMap overwrittenAttributes
	  ,hiddenAttributes = confirmMap hiddenAttributes
	  ,additional = confirmEffect_ additional
	  ,hidden = confirmEffect_ hidden
	  ,wrapper = confirmEffect_ wrapper
	  ,unwrapped = confirmEffect_ unwrapped
	  }
where
	confirmMap m = ('DM'.fromList o (map (appSnd confirmEffect_)) o 'DM'.toList) m

confirmEffect_ :: (LUIEffectStage a) -> LUIEffectStage a
confirmEffect_ (ESToBeApplied x) = ESApplied x
confirmEffect_ (ESToBeUpdated _ x) = ESApplied x
confirmEffect_ (ESToBeRemoved x) = ESNotApplied
confirmEffect_ es = es

//This extra pass should not be necessary, but without it the moves table is
//leaking memory
//TODO: Figure out why some moved items are still in the table
cleanupState_ :: (LUI,LUIMoves) -> (LUI,LUIMoves) 
cleanupState_ (lui,moves) = (lui, onlyKeep usedMoveIds moves)
where
	onlyKeep keep moves = ('DM'.fromList o (filter (\(k,_) -> isMember k keep)) o 'DM'.toList) moves

	usedMoveIds = collect lui

	collect (LUINode _ _ items _ _) = flatten (map collect items)
	collect (LUIMoveSource moveId) = [moveId:maybe [] (collect o snd) ('DM'.get moveId moves)]
	collect _ = []
