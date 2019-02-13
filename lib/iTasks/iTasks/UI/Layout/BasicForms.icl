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
	[layoutSubUIs (SelectByType UIInteract) layoutInteract
	,layoutSubUIs (SelectByType UIStep) layoutStep
	,layoutSubUIs (SelectByType UIParallel) layoutParallel
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	,removeSubUIs (SelectByType UIEmpty)
	]

layoutStep = sequenceLayouts
	[setUIType UIContainer
	,addButtonBar
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIStep)) layoutStep
	]
where
	addButtonBar = sequenceLayouts
		[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
		,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
		,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren layoutAsButton) //Transform actions to buttons
		]

layoutParallel = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIParallel)) layoutParallel
	]

layoutInteract = sequenceLayouts
	[setUIType UIPanel
	,layoutSubUIs (SelectAND SelectDescendents SelectFormElement) toFormItem
	,layoutSubUIs (SelectAND SelectDescendents SelectEditorContainers) layoutEditorContainer
	]

SelectFormElement = SelectByHasAttribute LABEL_ATTRIBUTE
SelectEditorContainers = 'DF'.foldr1 SelectOR
	(map SelectByType [UIPair,UIRecord,UICons,UIVarCons])

layoutEditorContainer = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs (SelectAND SelectDescendents SelectEditorContainers) layoutEditorContainer
	]

layoutAsButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) toButtonAttributes
	]
where
	toButtonAttributes attr 
		= maybe attr (\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a]) ('DM'.get "actionId" attr)
