definition module iTasks.UI.Layout
/**
* This module provides a simple DSL for creating layouts.
* Layouts are stateful transformations on a stream of UIChange events.
* They rearrange UI's when they are initially created and modify incremental
* updates that are later applied accordingly.
*/

from iTasks.UI.Definition import :: UI, :: UIType, :: UIAttribute, :: UIAttributes, :: UIAttributeKey, :: UIChange, :: UIChildChange

from Data.Maybe import :: Maybe
from Data.Map  import :: Map
from Data.Set import :: Set
from Data.Either import :: Either
from Data.GenEq import generic gEq

from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from StdOverloaded import class <

// This type is a mini query language to describe a selection
// of nodes in a UI (use for removing, moving, hiding or layouting)
// We use a data type instead of a function of type (UI -> Bool) because
// we want to keep only minimal state. Using an opaque function would require
// keeping track of the full state

:: UISelection
	//Select only nodes matching the exact path
	= SelectByPath !UIPath
	//Only match nodes at a given depth
	| SelectByDepth !Int
	//Match any descendents of any depth
	| SelectDescendents
	//Match nodes of a certain type
	| SelectByType !UIType
	//Match nodes that have a matching attribute
	| SelectByAttribute !String !(JSONNode -> Bool)
	//Match nodes that have the attribute
	| SelectByHasAttribute !String
	//Match nodes that have a specific 'class' attribute
	| SelectByClass !String
	//Match nodes with exactly the given number of children
	| SelectByNumChildren !Int
	//Match nodes that match the given selection on traversal of the given path
	| SelectRelative !UIPath !UISelection
	//Check if another (sub)-selection exists
	//For example, to select child nodes that have a UIAction child you use:
	//SelectAND
	//	 SelectChildren
	//	(SelectByContains
	//		SelectAND
	//			(SelectByType UIAction)
	//			(SelectByDepth 2)
	//	)
	| SelectByContains !UISelection
	//No-op
	| SelectNone
	//Set operations
	| SelectAND !UISelection !UISelection //Intersection
	| SelectOR !UISelection !UISelection //Union
	| SelectNOT !UISelection //Inverse

:: UIAttributeSelection
	= SelectAll
	| SelectKeys ![String]

//Only match children
SelectChildren :== SelectByDepth 1



// In specifications of layouts, sub-parts of UI's are commonly addressed as 
// a path of child selections in the UI tree.
:: UIPath :== [Int]

// Basic DSL for creating layouts

// == Changing node types ==
setUIType:: !UIType -> LayoutRule

// == Changing attributes ==
setUIAttributes :: !UIAttributes -> LayoutRule
delUIAttributes :: !UIAttributeSelection -> LayoutRule
modifyUIAttributes :: !UIAttributeSelection !(UIAttributes -> UIAttributes) -> LayoutRule
copySubUIAttributes :: !UIAttributeSelection !UIPath !UIPath -> LayoutRule

// == Changing the structure of a UI ==
wrapUI :: !UIType -> LayoutRule
unwrapUI :: LayoutRule

/*
* Hide a (piece of a) UI
*/
hideUI :: LayoutRule
removeSubUIs selection :== layoutSubUIs selection hideUI

/*
* Insert a (static) element into a UI
*/
insertChildUI :: !Int !UI -> LayoutRule

/**
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubUIs :: !UISelection !UIPath !Int -> LayoutRule

/**
* Applying a rule locally to matching parts of a UI
* When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate.
*/
layoutSubUIs :: !UISelection !LayoutRule -> LayoutRule

/**
* Applying multiple rules one after another.
*/
sequenceLayouts :: ![LayoutRule] -> LayoutRule

// ### Implementation: ####

//Experimental type that encodes all changes that are in effect by layouts
//From this data structure both the UI with, and without the layout effects, can be deduced
:: LUI
	//UI nodes (with upstream changes)
	= LUINode !LUINode
	//Placeholder nodes
	| LUIShiftDestination !LUIShiftID
	| LUIMoveSource !LUIMoveID
	| LUIMoveDestination !LUIMoveID !LUINo

derive JSONEncode LUI
derive JSONDecode LUI

:: LUINode = { type       :: !UIType
             , attributes :: !UIAttributes
             , items      :: ![LUI]
             , changes    :: !LUIChanges
             , effects    :: !LUIEffects
             }

derive gEq LUINode

//Upstream UI changes
:: LUIChanges =
	{ toBeInserted  :: !Bool
	, toBeRemoved   :: !Bool
	, toBeReplaced  :: !Maybe LUI
	, toBeShifted   :: !Maybe LUIShiftID
	, setAttributes :: !UIAttributes
	, delAttributes :: !Set UIAttributeKey
	}

:: LUIEffects =
	{ overwrittenType       :: !LUIEffectStage (!LUINo, !UIType)
	, overwrittenAttributes :: !Map UIAttributeKey (LUIEffectStage (!LUINo, !JSONNode))
	, hiddenAttributes      :: !Map UIAttributeKey (LUIEffectStage LUINo)
	, additional            :: !LUIEffectStage LUINo
	, hidden                :: !LUIEffectStage LUINo
	, wrapper               :: !LUIEffectStage LUINo
	, unwrapped             :: !LUIEffectStage LUINo
	}

//Layout rules determine that an effect should according to that rule be applied or restored.
//This desired state change can be undone by a later rule
//Only when the downstream changes have been collected is an effect marked as 'applied'
:: LUIEffectStage a
	//In between events effects can only be either applied or not
	= ESNotApplied
	| ESApplied !a
	//While the layout rules are applied the effects can be in intermediate state
	| ESToBeApplied !a
	| ESToBeUpdated !a !a
	| ESToBeRemoved !a

derive JSONEncode LUIEffectStage
derive JSONDecode LUIEffectStage

//Nodes that are moved by a moveSubUIs rule need to be accesible both in their source location (to apply changes)
//and in their destination location (to apply further effects).
//To make this possible, we put those nodes in a separate table and put references in the tree

:: LUIMoves :== Map LUIMoveID (!LUIEffectStage LUINo, !LUI)

noChanges :: LUIChanges
noEffects :: LUIEffects

//When layout rules make changes, it must be tracable which layout rule caused the change
:: LUINo =: LUINo [Int]

derive JSONEncode LUINo
derive JSONDecode LUINo

instance < LUINo
instance == LUINo
instance toString LUINo

//When shifting children, it must be tracable which source connects to which destination
:: LUIShiftID :== Int

:: LUIMoveID :== Int

//A layout rule is simply a function that applies (or undoes) an effect to a LUI tree
:: LayoutRule :== LUINo (!LUI, !LUIMoves) -> (LUI, LUIMoves)

initLUI :: !UI -> LUI
initLUIMoves :: LUIMoves

extractResetChange :: !(!LUI, !LUIMoves) -> (!UIChange, !(!LUI, !LUIMoves))

applyUpstreamChange :: !UIChange !(!LUI, !LUIMoves) -> (!LUI, !LUIMoves)

extractDownstreamChange :: !(!LUI, !LUIMoves) -> (!UIChange, !(!LUI, !LUIMoves))

//Helper functions (exported for unit testing)
scanToPosition_ :: !LUINo !Int ![LUI] !LUIMoves -> (!Int, !Bool, !Maybe LUI)
nodeExists_ :: !LUINo !LUI !LUIMoves -> Bool
selectChildNodes_ :: !LUINo !(![LUI], !LUIMoves) -> [LUI]
updateChildNodes_ :: !LUINo !(Int (!LUI, !LUIMoves) -> (LUI, LUIMoves)) !(![LUI], !LUIMoves) -> (![LUI], !LUIMoves)
selectSubNode_ :: !LUINo !UIPath !(!LUI, !LUIMoves) -> Maybe LUI
updateSubNode_ :: !LUINo !UIPath !((!LUI, !LUIMoves) -> (LUI, LUIMoves)) !(!LUI, !LUIMoves) -> (!LUI, !LUIMoves)
selectAttributes_ :: !UIAttributeSelection !UIAttributes -> UIAttributes
overwriteAttribute_ :: !LUINo !UIAttribute !(Map UIAttributeKey (LUIEffectStage (!LUINo, !JSONNode))) -> (Map UIAttributeKey (LUIEffectStage (!LUINo, !JSONNode)))
hideAttribute_ :: !LUINo !(UIAttributeKey -> Bool) !UIAttributeKey !(Map UIAttributeKey (LUIEffectStage LUINo)) -> (Map UIAttributeKey (LUIEffectStage LUINo))
matchAttributeKey_ :: !UIAttributeSelection !UIAttributeKey -> Bool
extractUIWithEffects_ :: !(!LUI, !LUIMoves) -> Maybe UI
isPartOf_ :: !LUINo !LUINo -> Bool
