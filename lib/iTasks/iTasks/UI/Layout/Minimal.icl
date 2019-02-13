implementation module iTasks.UI.Layout.Minimal
/**
* This module provides a minimal layout rule that only converts all intermediate UI nodes to basic elements (containers and buttons)
* It does not apply any other transformations.
*/
import StdEnv

import iTasks.UI.Definition
import iTasks.UI.Layout
import Data.List, Data.Maybe, Text.GenJSON
import qualified Data.Map as DM
import qualified Data.Foldable as DF

minimalSessionLayout :: LayoutRule
minimalSessionLayout = layoutAny

layoutAny = sequenceLayouts
	[layoutSubUIs SelectIntermediateContainers layoutAsContainer
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	,removeSubUIs (SelectByType UIEmpty)
	]
SelectIntermediateContainers = 'DF'.foldr1 SelectOR
	(map SelectByType [UIPair,UIRecord,UICons,UIVarCons,UIInteract,UIStep,UIParallel])

layoutAsContainer = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs SelectChildren layoutAny
	]
layoutAsButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) toButtonAttributes
	]
where
	toButtonAttributes attr 
		= maybe attr (\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a]) ('DM'.get "actionId" attr)
