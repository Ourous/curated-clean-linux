implementation module iTasks.WF.Combinators.Tune

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Tune
import iTasks.UI.Layout

import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import Data.Maybe, Text.GenJSON, StdString
import qualified Data.Map as DM

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set
derive JSONDecode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set

/*
* Tuning of tasks
*/
instance tune LazyRefresh Task
where
	tune _ (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld
			= case (eval event evalOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskEvalInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)

instance tune ApplyLayout Task
where
	tune (ApplyLayout l) task = applyLayout l task

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

class toAttribute a where toAttribute :: a -> JSONNode
instance toAttribute String where toAttribute s = JSONString s

instance tune (ApplyAttribute a) Task | toAttribute a
where
	tune (ApplyAttribute k v) task = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(k,toAttribute v)]))) task

