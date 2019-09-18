implementation module iTasks.UI.Layout.BasicForms

import StdEnv

import iTasks.UI.Definition
import iTasks.UI.Layout
import iTasks.UI.Layout.Common

import Text.GenJSON
import Data.Maybe
import Data.List

import qualified Data.Map as DM
import qualified Data.Foldable as DF

basicFormsSessionLayout :: LayoutRule
basicFormsSessionLayout = layoutCombinatorContainers

layoutCombinatorContainers = sequenceLayouts
	[layoutSubUIs (SelectByClass "interact") layoutInteract
	,layoutSubUIs (SelectByClass "step-actions") layoutStep
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	,removeSubUIs (SelectByType UIEmpty)
	]

layoutStep = sequenceLayouts
	[addButtonBar
	,layoutSubUIs (SelectAND SelectDescendents (SelectByClass "step-actions")) layoutStep
	]
where
	addButtonBar = sequenceLayouts
		[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
		,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
		,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren layoutAsButton) //Transform actions to buttons
		]

layoutInteract = sequenceLayouts
	[setUIType UIPanel
	,layoutSubUIs (SelectAND SelectDescendents (SelectByHasAttribute "label" )) toFormItem
	]

layoutAsButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) toButtonAttributes
	]
where
	toButtonAttributes attr 
		= maybe attr (\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a]) ('DM'.get "actionId" attr)
