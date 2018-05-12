implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import Text.GenJSON
import Data.GenEq

from Data.Func import $
from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor, Data.Maybe
import Data.List, StdString
import iTasks.UI.Layout.Debug
import qualified Data.Map as DM

SelectParallel :== SelectOR (SelectByType UIParallel) (SelectByType UITabSet)

//Util:
sequenceAllLayouts [] = idLayout
sequenceAllLayouts list = foldl1 sequenceLayouts list 

defaultSessionLayout :: Layout
defaultSessionLayout = sequenceAllLayouts
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = sequenceAllLayouts
	[layoutSubUIs (SelectByType UIInteract) finalizeInteract
	,layoutSubUIs (SelectByType UIStep) finalizeStep
	,layoutSubUIs SelectParallel finalizeParallel
	,layoutSubUIs (SelectByType UIList) finalizeList
	]

finalizeList :: Layout
finalizeList = sequenceLayouts
	(layoutSubUIs (SelectByDepth 1) (setUIAttributes (heightAttr WrapSize)))
	(setUIAttributes (heightAttr WrapSize))

finalizeInteract :: Layout
finalizeInteract = sequenceAllLayouts
		[copyContentTitle
		,layoutSubUIs (SelectByPath [1]) finalizeEditor
		,removeEmptyPrompt
		,setUIType UIContainer
		,layoutSubUIs (SelectAND (SelectByPath []) (SelectByHasAttribute "title")) (setUIType UIPanel)
		] 
where
	copyContentTitle = copySubUIAttributes (SelectKeys ["title"]) [0] []
	removeEmptyPrompt = layoutSubUIs (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIs (SelectByPath [0]))

finalizeEditor :: Layout
finalizeEditor = sequenceAllLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIRecord)) finalizeRecord
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UICons)) finalizeCons
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIVarCons)) finalizeVarCons
	,layoutSubUIs (SelectAND (SelectByPath []) selectFormComponent) finalizeFormComponent
	//Fallback in case the editor is some other container (this happens with lists...)
	,layoutSubUIs (SelectAND SelectDescendents selectEditorIntermediate) finalizeEditor 
	]

selectFormComponent
	= SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		(foldl1 SelectOR [SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ])

finalizeFormComponent = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: Layout
finalizeRecord = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIType UIContainer
	,setUIAttributes (heightAttr WrapSize)
	]

finalizeCons :: Layout
finalizeCons = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeVarCons :: Layout
finalizeVarCons = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = sequenceAllLayouts
	[removeDisabledActionsOfNestedSteps //In case of nested steps, memove disabled actions
	//If there are no actions, unwrap
	,layoutSubUIs (ContainsNoChildOfType UIAction) (sequenceAllLayouts [copySubUIAttributes SelectAll [] [0], unwrapUI,finalizeUI])
	//If the previous rule did not eliminate the UIStep
	,layoutSubUIs RootIsStep
		$ sequenceAllLayouts
			[layoutSubUIs (SelectByPath [0]) finalizeUI
			,actionsToButtonBar
			,setUIType UIPanel]
	]
where
	// Nested steps are steps having steps under them
	removeDisabledActionsOfNestedSteps
		= layoutSubUIs
				(SelectAND                             // (Nested)
					(SelectByType UIStep)              // Steps (are steps)
						$ SelectByContains             // having
							$ SelectAND
								(SelectByType UIStep)  // steps
								SelectDescendents)     // under them
			removeDisabledActions

	removeDisabledActions
		= removeSubUIs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" ((==) (JSONBool False)))))

	ContainsNoChildOfType type = SelectAND (SelectByPath []) (SelectNOT (SelectByContains (SelectAND SelectChildren (SelectByType type))))
	RootIsStep = SelectAND (SelectByPath []) (SelectByType UIStep)

finalizeParallel :: Layout
finalizeParallel = sequenceAllLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectAND SelectParallel (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIParallel)) layoutWithoutActions
	,layoutSubUIs (SelectByType UIParallel) (setUIType UIContainer)
	]
where
	layoutWithoutActions = sequenceAllLayouts
		[layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		]
	layoutWithActions = sequenceAllLayouts
		[actionsToButtonBar
		,layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		//Move button bars for tabsets outside
		,layoutSubUIs (SelectAND (SelectByType UITabSet) (SelectByContains (SelectAND SelectChildren (SelectByType UIButtonBar)))) (sequenceAllLayouts
			[wrapUI UIContainer
			,moveSubUIs (SelectAND (SelectByDepth 2) (SelectByType UIButtonBar)) [] 1
			])
		]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]

actionsToButtonBar = sequenceAllLayouts
	[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren actionToButton) //Transform actions to buttons 
	]

