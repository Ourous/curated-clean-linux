implementation module iTasks.UI.Layout.StandardForms

import iTasks.UI.Definition
import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import StdBool, StdString, StdArray, Data.List, Data.Maybe, Data.Func, Text.GenJSON
import qualified Data.Map as DM
import Text.HTML

standardFormsSessionLayout :: LayoutRule
standardFormsSessionLayout = sequenceLayouts
	[layoutCombinatorContainers
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
	,removeSubUIs (SelectByType UIEmpty)
	]

layoutCombinatorContainers = sequenceLayouts
	[layoutSubUIs (SelectByClass "interact") layoutInteract
	,layoutSubUIs (SelectByClass "step-actions") layoutStepWithActions
	,layoutSubUIs (SelectByClass "parallel-actions") layoutParallelWithActions
	//There can still be buttons (e.g. when a parallel has been transformed to a tabset
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	]

layoutStepWithActions = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents (SelectByClass "step-actions")) layoutStepWithActions
	,layoutSubUIs SelectNestedStep removeDisabledActions
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIContainer)) (setUIType UIPanel) 
	,addButtonBar
	,modifyUIAttributes (SelectKeys ["class"]) (removeClassAttr "step-actions")
	]
where
	SelectNestedStep =
		(SelectAND                             // (Nested)
			(SelectByClass "step-actions")     // Steps (are steps)
				$ SelectByContains             // having
					$ SelectAND
						(SelectByClass "step-actions")  // steps
						SelectDescendents)     // under them

	SelectDisabledAction =
		SelectAND 
			SelectChildren
			(SelectAND	
				(SelectByType UIAction)
				(SelectByAttribute "enabled" ((==) (JSONBool False)))
			)

	removeDisabledActions = layoutSubUIs SelectDisabledAction hideUI

layoutParallelWithActions = sequenceLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIContainer)) (setUIType UIPanel) 
	,addToolBar
	,modifyUIAttributes (SelectKeys ["class"]) (removeClassAttr "parallel-actions")
	]

layoutInteract = sequenceLayouts
	[modifyUIAttributes (SelectKeys ["class"]) (removeClassAttr "interact") //Make sure the rule won't trigger twice
	,layoutEditor
	,decorateEditor
	]
where
	layoutEditor = sequenceLayouts
		[layoutSubUIs SelectFormElement layoutFormItem
		,layoutSubUIs (SelectByClass "record") layoutRecord
		,layoutSubUIs (SelectByClass "var-cons") layoutVarCons
		,layoutSubUIs (SelectByType UIList) layoutList
		]
	layoutFormItem = sequenceLayouts
		[toFormItem
		,layoutSubUIs (SelectAND SelectDescendents SelectFormElement) layoutFormItem
		]

	SelectFormElement = SelectByHasAttribute "label"

	decorateEditor = layoutSubUIs (SelectOR hasTitle hasPrompt) wrapEditor
	where
		hasTitle = SelectAND (SelectByPath []) (SelectByHasAttribute "title")
		hasPrompt = SelectAND (SelectByPath []) (SelectByHasAttribute "hint")
		wrapEditor = sequenceLayouts
			[wrapUI UIContainer
			,copySubUIAttributes SelectAll [0] []
			,layoutSubUIs hasTitle (setUIType UIPanel)
			,layoutSubUIs hasPrompt (sequenceLayouts [createPrompt,fillPrompt])
			]
		createPrompt = insertChildUI 0 prompt
		where
			prompt = UI UIContainer (classAttr ["itasks-prompt","itasks-flex-width","itasks-wrap-height"]) [ui UITextView]

		fillPrompt = sequenceLayouts
			[copySubUIAttributes (SelectKeys ["hint"]) [] [0,0]
			,layoutSubUIs (SelectByPath [0,0]) (modifyUIAttributes (SelectKeys ["hint"]) promptToValue)
			]
		where
			promptToValue attr = 'DM'.fromList [("value",JSONString (maybe "" (\(JSONString s) -> escapeStr s) ('DM'.get "hint" attr)))]

//Different types of editor containers
layoutRecord :: LayoutRule
layoutRecord = sequenceLayouts
	[setUIAttributes (heightAttr WrapSize)
	,layoutSubUIs (SelectAND SelectDescendents (SelectByClass "record")) layoutRecord
	]

layoutVarCons :: LayoutRule
layoutVarCons = sequenceLayouts
	[layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,layoutSubUIs (SelectAND SelectDescendents (SelectByClass "var-cons")) layoutVarCons
	]

layoutList :: LayoutRule
layoutList = sequenceLayouts
	[layoutSubUIs SelectChildren (setUIAttributes (heightAttr WrapSize))
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
