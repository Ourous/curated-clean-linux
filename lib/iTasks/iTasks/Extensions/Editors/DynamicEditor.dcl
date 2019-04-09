definition module iTasks.Extensions.Editors.DynamicEditors

import iTasks

:: DynamicEditor a =: DynamicEditor [DynamicEditorElement]
// phantom type only needed for top level
:: DynamicEditorValue a = DynamicEditorValue !DynamicConsId !DEVal | Undefined // TODO: Undefined can be removed once we have parametrised editors

:: DEVal = DEApplication ![(!DynamicConsId, !DEVal)]
         | DEJSONValue   !JSONNode

derive class iTask DynamicEditorValue

:: DynamicEditorElement = DynamicCons !DynamicCons | DynamicConsGroup !String ![DynamicCons]
:: DynamicCons
:: DynamicConsOption = HideIfOnlyChoice | UseAsDefault

(<<@@@) infixl 2 :: !DynamicCons !DynamicConsOption -> DynamicCons
(@@@>>) infixr 2 :: !DynamicConsOption !DynamicCons -> DynamicCons

:: DynamicConsId :== String
:: DynamicConsBuilder =     FunctionCons     !Dynamic
                     | E.a: CustomEditorCons !(Editor a) & JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|}, TC a
                     |      ListCons         !Dynamic    //* must contain a value of type [a] -> b

functionCons     :: !String !String !a          -> DynamicCons | TC a
listCons         :: !String !String !([a] -> b) -> DynamicCons | TC a & TC b
customEditorCons :: !String !String !(Editor a) -> DynamicCons | TC, JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|} a
// dynamic variants are required because this is the only way to use quantified type variables
functionConsDyn :: !String !String !Dynamic -> DynamicCons
listConsDyn     :: !String !String !Dynamic -> DynamicCons

dynamicEditor :: !(DynamicEditor a) -> Editor (DynamicEditorValue a) | TC a
parametrisedDynamicEditor
	:: !(p -> DynamicEditor a) -> Editor (!p, !DynamicEditorValue a)
	| TC a & gEq{|*|}, JSONEncode{|*|}, JSONDecode{|*|} p

toValue              :: !(DynamicEditor a) !(DynamicEditorValue a) -> a | TC a
dynEditorValToString :: !(DynamicEditor a) !(DynamicEditorValue a) -> String
