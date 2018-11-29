implementation module iTasks.WF.Combinators.Tune

import StdList, StdFunctions
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Layout

import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import Data.Maybe, Data.Error, Data.Functor, Text.GenJSON, StdString
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set
derive JSONDecode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set

class addConstantAttribute f :: !String !b !(f a) -> f a | toAttribute b
instance addConstantAttribute Task
where
	addConstantAttribute attrName value task=:(Task evala) = Task eval
	where
		eval event evalOpts tree iworld
			# (result,iworld) = evala event evalOpts tree iworld
			# attrValue = toAttribute value
			# (result,iworld) = addAttributeChange attrName attrValue attrValue result iworld 
			= (result,iworld)

class addValueAttribute f :: !String ((Maybe a) -> b) !(f a) -> f a | toAttribute b
instance addValueAttribute Task
where
	addValueAttribute attrName attrValueFun task=:(Task evala) = Task eval
	where
		//Init
		eval event evalOpts tree=:(TCInit taskId ts) iworld
			= eval event evalOpts (TCAttribute taskId (toAttribute (attrValueFun Nothing)) tree) iworld

		eval event evalOpts (TCAttribute taskId curAttrValue tree) iworld
			//Evaluate inner task
			# (result,iworld) = evala event evalOpts tree iworld
			# newAttrValue = refreshAttribute result
			# (result,iworld) = addAttributeChange attrName curAttrValue newAttrValue result iworld 
			= case result of
				ValueResult value info changes tree 
					= (ValueResult value info changes (TCAttribute taskId newAttrValue tree),iworld)
				_
					= (result,iworld)
		where
			refreshAttribute (ValueResult (Value v _) _ _ _) = toAttribute (attrValueFun (Just v))
			refreshAttribute _ = toAttribute (attrValueFun Nothing)

		//Destroy
		eval event evalOpts (TCDestroy (TCAttribute _ _ tree)) iworld =
			evala event evalOpts (TCDestroy tree) iworld

class addSDSAttribute f :: !String (SDS () r w) (r -> b) !(f a) -> f a | toAttribute b & TC r
instance addSDSAttribute Task
where
	addSDSAttribute attrName sds attrValueFun task=:(Task evala) = Task eval
	where
		//Init
		eval event evalOpts tree=:(TCInit taskId ts) iworld
			# (mbr,iworld) = 'SDS'.readRegister taskId sds iworld 
			| isError mbr
				= (ExceptionResult (fromError mbr),iworld)
			= eval event evalOpts (TCAttribute taskId (toAttribute (attrValueFun (fromOk mbr))) tree) iworld

		eval event evalOpts (TCAttribute taskId curAttrValue tree) iworld
			//Evaluate inner task
			# (result,iworld) = evala event evalOpts tree iworld
			//Check if we need to refresh the share
			# (mbNewAttrValue,iworld) = refreshAttribute taskId curAttrValue event iworld
			| isError mbNewAttrValue
				= (ExceptionResult (fromError mbNewAttrValue),iworld)
			//Add/change the value 
			# (Ok newAttrValue) = mbNewAttrValue
			# (result,iworld) = addAttributeChange attrName curAttrValue newAttrValue result iworld 
			= case result of
				ValueResult value info changes tree 
					= (ValueResult value info changes (TCAttribute taskId newAttrValue tree),iworld)
				_
					= (result,iworld)
		where
			refreshAttribute taskId cur (RefreshEvent refreshSet _) iworld
				| 'DS'.member taskId refreshSet
					# (mbr,iworld) = 'SDS'.readRegister taskId sds iworld 
					= (fmap (toAttribute o attrValueFun) mbr,iworld)
			refreshAttribute taskId cur _ iworld
				= (Ok cur,iworld)
		//Destroy
		eval event evalOpts (TCDestroy (TCAttribute _ _ tree)) iworld =
			evala event evalOpts (TCDestroy tree) iworld

//Shared helper functions
addAttributeChange attrName _ new (ValueResult value info (ReplaceUI (UI type attr items)) tree) iworld
	# attr = 'DM'.put attrName (JSONString new) attr
	# info = {TaskEvalInfo|info & attributes = 'DM'.put attrName new info.TaskEvalInfo.attributes}
	= (ValueResult value info (ReplaceUI (UI type attr items)) tree, iworld)
addAttributeChange attrName cur new (ValueResult value info (ChangeUI attrChanges itemChanges) tree) iworld
	//The constant value overrules any changes to the attribute
	# attrChanges = filter (ignoreAttribute attrName) attrChanges
	//If the annotated attribute changes, we need to set it
	# attrChanges = (if (cur <> new) [SetAttribute attrName (JSONString new)] []) ++ attrChanges
	# info = {TaskEvalInfo|info & attributes = 'DM'.put attrName new info.TaskEvalInfo.attributes}
	= (ValueResult value info (ChangeUI attrChanges itemChanges) tree, iworld)
addAttributeChange attrName cur new result iworld
	= (result,iworld)

ignoreAttribute x (SetAttribute y _) = x == y
ignoreAttribute x (DelAttribute y) = x == y

class toAttribute a where toAttribute :: a -> String
instance toAttribute String where toAttribute s = s
instance toAttribute Int where toAttribute i = toString i

class tune b f :: !b !(f a) -> f a
class tunev b a f | iTask a :: !(b a) !(f a) -> f a

(<<@) infixl 2 :: !(f a) !b	-> f a | tune b f
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(f a)	-> f a | tune b f
(@>>) a t = tune a t

(<@@) infixl 2 :: !(f a) !(b a) -> f a | tunev b a f & iTask a
(<@@) t a = tunev a t

(@@>) infixr 2 :: !(b a) !(f a) -> f a | tunev b a f & iTask a
(@@>) a t = tunev a t

instance tune (ApplyAttribute a) Task | toAttribute a
where tune (ApplyAttribute k v) task = addConstantAttribute k v task

instance tune (ApplySDSAttribute a r w) Task | toAttribute a & TC r
where tune (ApplySDSAttribute k sds f) task = addSDSAttribute k sds f task

applyLayout :: LayoutRule (Task a) -> Task a
applyLayout rule task=:(Task evala) = Task eval
	where
		ruleNo = LUINo [0]

		eval event evalOpts (TCDestroy (TCLayout s tt)) iworld //Cleanup duty simply passed to inner task
			= evala event evalOpts (TCDestroy tt) iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			//On initialization, we need to do a reset to be able to apply the layout
			= eval ResetEvent evalOpts (TCLayout (initLUI (ui UIEmpty),initLUIMoves) tt) iworld 

		//On Reset events, we (re-)apply the layout
		eval ResetEvent evalOpts (TCLayout _ tt) iworld = case evala ResetEvent evalOpts tt iworld of
			(ValueResult value info (ReplaceUI ui) tt,iworld)
				# (change,state) = extractResetChange (rule ruleNo (initLUI ui, initLUIMoves))
				= (ValueResult value info change (TCLayout state tt), iworld)		
            (res,iworld) = (res,iworld)

		eval event evalOpts (TCLayout state tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				# state = applyUpstreamChange change state
				# state = rule ruleNo state
				# (change,state) = extractDownstreamChange state
				= (ValueResult value info change (TCLayout state tt), iworld)
            (res,iworld) = (res,iworld)
		
		eval event evalOpts state iworld = evala event evalOpts state iworld //Catchall

instance tune ApplyLayout Task
where tune (ApplyLayout l) task = applyLayout l task


