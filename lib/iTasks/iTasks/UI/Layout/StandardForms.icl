implementation module iTasks.UI.Layout.StandardForms

import iTasks.UI.Definition
import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import StdBool, StdString, StdArray, Data.List, Data.Maybe, Data.Func, Text.GenJSON
import qualified Data.Map as DM

standardFormsSessionLayout :: LayoutRule
standardFormsSessionLayout = sequenceLayouts
	[layoutCombinatorContainers
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
	,removeSubUIs (SelectByType UIEmpty)
	]

layoutCombinatorContainers = sequenceLayouts
	[layoutSubUIs (SelectByType UIInteract) layoutInteract
	,layoutSubUIs (SelectByType UIStep) layoutStep
	,layoutSubUIs (SelectByType UIParallel) layoutParallel
	//There can still be buttons (e.g. when a parallel has been transformed to a tabset
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	]

layoutStep = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents (SelectByType UIStep)) layoutStep
	,layoutSubUIs SelectNestedStep removeDisabledActions
	,layoutSubUIs (SelectAND NotYetTransformed HasActions) layoutWithActions
	,layoutSubUIs NotYetTransformed layoutWithoutActions
	]
where
	SelectNestedStep =
		(SelectAND                             // (Nested)
			(SelectByType UIStep)              // Steps (are steps)
				$ SelectByContains             // having
					$ SelectAND
						(SelectByType UIStep)  // steps
						SelectDescendents)     // under them

	SelectDisabledAction =
		SelectAND 
			SelectChildren
			(SelectAND	
				(SelectByType UIAction)
				(SelectByAttribute "enabled" ((==) (JSONBool False)))
			)

	removeDisabledActions = layoutSubUIs SelectDisabledAction hideUI

	NotYetTransformed = SelectAND (SelectByPath []) (SelectByType UIStep)
	HasActions = SelectByContains (SelectAND SelectChildren (SelectByType UIAction))

	layoutWithoutActions = sequenceLayouts
		[copySubUIAttributes SelectAll [] [0]
		,unwrapUI
		]
	layoutWithActions = sequenceLayouts
		[setUIType UIPanel
		,addButtonBar
		]
		
layoutParallel = sequenceLayouts
	[layoutSubUIs (SelectAND NotYetTransformed HasActions) layoutWithActions
	,layoutSubUIs NotYetTransformed layoutWithoutActions
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIParallel)) layoutParallel
	]
where
	NotYetTransformed = SelectAND (SelectByPath []) (SelectByType UIParallel)
	HasActions = SelectByContains (SelectAND SelectChildren (SelectByType UIAction))

	layoutWithoutActions = setUIType UIContainer
	layoutWithActions = sequenceLayouts [setUIType UIPanel, addToolBar]

layoutInteract = sequenceLayouts
	[setTitle
	,layoutEditor
	,removePromptIfEmpty
	,setContainerType
	]
where
	setTitle = copySubUIAttributes (SelectKeys ["title"]) [0] []

	layoutEditor = layoutSubUIs (SelectByPath [1]) (sequenceLayouts
		[layoutSubUIs SelectFormElement layoutFormItem
		,layoutSubUIs (SelectByType UIRecord) layoutRecord
		,layoutSubUIs (SelectByType UICons) layoutCons
		,layoutSubUIs (SelectByType UIVarCons) layoutVarCons
		,layoutSubUIs (SelectByType UIList) layoutList
		,layoutSubUIs (SelectByType UIPair) layoutPair
		])
	layoutFormItem = sequenceLayouts
		[toFormItem
		,layoutSubUIs (SelectAND SelectDescendents SelectFormElement) layoutFormItem
		]

	removePromptIfEmpty = layoutSubUIs withEmptyPrompt removePrompt
	where
		withEmptyPrompt = SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))
		removePrompt = removeSubUIs (SelectByPath [0])

	setContainerType = sequenceLayouts
		[setUIType UIContainer 
		,layoutSubUIs (SelectAND (SelectByPath []) (SelectByHasAttribute "title")) (setUIType UIPanel)
		]

SelectFormElement = SelectByHasAttribute LABEL_ATTRIBUTE

//TODO: consider flattening PAIRs somehow?
layoutPair = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIPair)) layoutPair
	]

//Different types of editor containers
layoutRecord :: LayoutRule
layoutRecord = sequenceLayouts
	[setUIType UIContainer
	,setUIAttributes (heightAttr WrapSize)
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIRecord)) layoutRecord
	]

layoutCons :: LayoutRule
layoutCons = sequenceLayouts
	[setUIType UIContainer
	,setUIAttributes (directionAttr Horizontal)
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UICons)) layoutCons
	]

layoutVarCons :: LayoutRule
layoutVarCons = sequenceLayouts
	[setUIType UIContainer
	,setUIAttributes (directionAttr Horizontal)
	,layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIVarCons)) layoutVarCons
	]

layoutList :: LayoutRule
layoutList = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs SelectChildren (setUIAttributes (heightAttr WrapSize))
	,setUIAttributes (heightAttr WrapSize)
	,layoutSubUIs (SelectAND SelectDescendents (SelectByType UIList)) layoutList
	]

layoutAsButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) toButtonAttributes
	]
where
	toButtonAttributes attr 
		= maybe attr (\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a]) ('DM'.get "actionId" attr)

addToolBar = sequenceLayouts
	[insertChildUI 0 (ui UIToolBar)
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [0] 0 //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [0]) (layoutSubUIs SelectChildren layoutAsButton) //Transform actions to buttons
	]

addButtonBar = sequenceLayouts
	[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren layoutAsButton) //Transform actions to buttons
	]
