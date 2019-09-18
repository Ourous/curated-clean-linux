implementation module iTasks.WF.Tasks.Interaction

from StdFunc import id, const, o, flip
import Data.Func
from Data.Tuple import appSnd
from Data.List import isMemberGen, findIndex, instance Functor [], getItems
from Data.Map import qualified get, put
import qualified Data.Map as DM

import StdBool, StdList, StdMisc, StdTuple, Data.Functor, Data.Maybe, StdString
import iTasks.WF.Derives
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.SDS
import iTasks.WF.Combinators.Overloaded
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Core
import iTasks.SDS.Sources.System
import iTasks.SDS.Combinators.Common
import iTasks.Internal.Util
import iTasks.Internal.SDS
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Text.HTML

derive class iTask ChoiceText, ChoiceGrid, ChoiceRow, ChoiceNode

//Boilerplate access functions
selectAttributes :: [SelectOption a b] -> UIAttributes
selectAttributes options = foldr addOption 'DM'.newMap options
where
	addOption (SelectMultiple multiple) attr = 'DM'.union (multipleAttr multiple) attr
	addOption _ attr = attr

viewEditor :: [ViewOption m] -> ViewOption m | iTask m
viewEditor [ViewUsing tof editor:_] = ViewUsing tof editor
viewEditor [ViewAs tof:_] = ViewUsing tof gEditor{|*|}
viewEditor [_:es] = viewEditor es
viewEditor [] =  ViewUsing id gEditor{|*|}

enterEditor :: [EnterOption m] -> EnterOption m | iTask m
enterEditor [EnterUsing fromf editor:_] = EnterUsing fromf editor
enterEditor [EnterAs fromf:_] = EnterUsing fromf gEditor{|*|}
enterEditor [_:es] = enterEditor es
enterEditor [] =  EnterUsing id gEditor{|*|}

updateEditor :: [UpdateOption m] -> UpdateOption m | iTask m
updateEditor [UpdateUsing tof fromf editor:_] = UpdateUsing tof fromf editor
updateEditor [UpdateAs tof fromf:_] = UpdateUsing tof fromf gEditor{|*|}
updateEditor [_:es] = updateEditor es
updateEditor [] =  UpdateUsing id (flip const) gEditor{|*|}

updateSharedEditor :: [UpdateSharedOption r w] -> UpdateSharedOption r w | iTask r & iTask w
updateSharedEditor [UpdateSharedUsing tof fromf conflictf editor:_] = UpdateSharedUsing tof fromf conflictf editor
updateSharedEditor [UpdateSharedAs tof fromf conflictf:_] = UpdateSharedUsing tof fromf conflictf gEditor{|*|}
updateSharedEditor [_:es] = updateSharedEditor es
updateSharedEditor [] =  UpdateSharedUsingAuto dynid (flip const) const gEditor{|*|}
where
	//If r == w then this is just the identity, otherwise the editor will use a default value
	dynid x = case dynamic id :: A.a: (a -> a) of
		(rtow :: r^ -> w^) = Just (rtow x)
		_                  = Nothing

selectEditor :: [SelectOption c a] -> SelectOption c a
selectEditor [SelectInDropdown toView fromView:_] = SelectUsing toView fromView dropdown
selectEditor [SelectInCheckGroup toView fromView:_] = SelectUsing toView fromView checkGroup
selectEditor [SelectInList toView fromView:_] = SelectUsing toView fromView choiceList
selectEditor [SelectInGrid toView fromView:_] = SelectUsing toView fromView grid
selectEditor [SelectInTree toView fromView:_] = SelectUsing toView fromView tree
selectEditor [SelectInTabs toView fromView:_] = SelectUsing toView fromView tabBar
selectEditor [_:es] = selectEditor es
selectEditor [] = SelectUsing (const []) (\_ _ -> []) dropdown //Empty dropdown


//Convert choice options to select options
selectOptions :: (o -> s) [ChoiceOption o] -> [SelectOption [o] s] | gText{|*|} o
selectOptions target options = selectOptions` False options 
where
	selectOptions` _ [ChooseFromDropdown f:os] = [SelectInDropdown (toTexts f) (findSelection target):selectOptions` True os]
	selectOptions` _ [ChooseFromCheckGroup f:os] = [SelectInCheckGroup (toTexts f)  (findSelection target):selectOptions` True os]
	selectOptions` _ [ChooseFromList f:os] = [SelectInList (toTexts f) (findSelection target):selectOptions` True os]
	selectOptions` _ [ChooseFromGrid f:os] = [SelectInGrid (toGrid f) (findSelection target):selectOptions` True os]
	selectOptions` _ [ChooseFromTabs f:os] = [SelectInTabs (toTexts f) (findSelection target):selectOptions` True os]
	selectOptions` True [] = []
	selectOptions` False [] = [SelectInDropdown (toTexts id) (findSelection target)]

	toTexts f options = [{ChoiceText|id=i,text=toSingleLineText (f o)} \\ o <- options & i <- [0..]]
	toGrid f options = {ChoiceGrid|header=gText{|*|} AsHeader (fixtype vals),rows = [{ChoiceRow|id=i,cells=map Text (gText{|*|} AsRow (Just v))} \\ v <- vals & i <- [0..]]}
	where
		vals = map f options
		fixtype :: [a] -> Maybe a
		fixtype _ = Nothing

findSelection :: (o -> s) [o] [Int] -> [s]
findSelection target options idxs = target <$> getItems options idxs

enterInformation :: ![EnterOption m] -> Task m | iTask m
enterInformation options = enterInformation` (enterEditor options)
enterInformation` (EnterUsing fromf editor)
	= interactRW unitShare handlers editor @ (\((),v) -> fromf v)
where
	handlers = {onInit = const ((), Enter), onEdit = \_ l -> (l, Nothing), onRefresh = \r l _ -> (l,undef,Nothing)}

viewInformation :: ![ViewOption m] !m -> Task m | iTask m
viewInformation options m = viewInformation`  (viewEditor options) m
viewInformation` (ViewUsing tof editor) m
	= interactR unitShare {onInit = const ((),View $ tof m), onEdit = \_ l -> (l, Nothing), onRefresh = \r l (Just v) -> (l,v,Nothing)} editor @! m

updateInformation :: ![UpdateOption m] m -> Task m | iTask m
updateInformation options m = updateInformation` (updateEditor options) m
updateInformation` (UpdateUsing tof fromf editor) m
	= interactRW unitShare {onInit = const ((), Update $ tof m), onEdit = \_ l -> (l, Nothing), onRefresh = \r l (Just v) -> (l,v,Nothing)}
		editor @ (\((),v) -> fromf m v)

updateSharedInformation :: ![UpdateSharedOption r w] !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
updateSharedInformation options sds = updateSharedInformation` (updateSharedEditor options) sds
updateSharedInformation` (UpdateSharedUsing tof fromf conflictf editor) sds
	= interactRW sds {onInit = \r -> (r, Update $ tof r), onEdit = \v l -> (l, Just (\r -> fromf r v)), onRefresh = \r _ (Just v) -> (r,conflictf (tof r) v, Nothing)}
		editor @ fst
updateSharedInformation` (UpdateSharedUsingAuto tof fromf conflictf editor) sds
	= interactRW sds {onInit = \r -> (r, maybe Enter Update (tof r)), onEdit = \v l -> (l, Just (\r -> fromf r v))
			, onRefresh = \r _ (Just v) -> (r, maybe v (\r` -> conflictf r` v) (tof r), Nothing)}
		editor @ fst

viewSharedInformation :: ![ViewOption r] !(sds () r w) -> Task r | iTask r & TC w & Registrable sds
viewSharedInformation options sds = viewSharedInformation` (viewEditor options) sds
viewSharedInformation` (ViewUsing tof editor) sds
	= interactR sds {onInit = \r -> (r, View $ tof r), onEdit = \_ l -> (l, Nothing), onRefresh = \r _ _ -> (r,tof r,Nothing)} editor @ fst

updateInformationWithShared :: ![UpdateSharedOption (r,m) m] !(sds () r w) m -> Task m | iTask r & iTask m & TC w & RWShared sds
updateInformationWithShared options sds m = updateInformationWithShared` (updateSharedEditor options) sds m
updateInformationWithShared` (UpdateSharedUsing tof fromf conflictf editor) sds m
	= interactRW sds
		{onInit = \r -> ((r,m), Update $ tof (r,m))
		,onEdit = \v (r,m)      -> let nm = fromf (r,m) v in ((r,nm),Nothing)
		,onRefresh = \r (_,m) _ -> ((r,m),tof (r,m),Nothing)
		} gEditor{|*|} @ (snd o fst)

editSelection :: ![SelectOption c a] c [Int] -> Task [a] | iTask a
editSelection options container sel = editSelection` (selectAttributes options) (selectEditor options) container sel
editSelection` attributes (SelectUsing toView fromView editor) container sel
	= interactRW unitShare
		{onInit = \r   -> ((), Update (toView container,sel))
		,onEdit = \_ l -> (l, Nothing)
		,onRefresh = \_ l (Just v) -> (l,v,Nothing)
		} (attributes @>> editor) @ (\(_,(_,sel)) -> fromView container sel)

editSelectionWithShared :: ![SelectOption c a] (sds () c w) (c -> [Int]) -> Task [a] | iTask c & iTask a & TC w & RWShared sds
editSelectionWithShared options sharedContainer initSel = editSelectionWithShared` (selectAttributes options) (selectEditor options) sharedContainer initSel
editSelectionWithShared` attributes (SelectUsing toView fromView editor) sharedContainer initSel
	= interactRW sharedContainer
		{onInit = \r     -> (r, Update(toView r, initSel r))
		,onEdit = \_ l -> (l, Nothing)
		,onRefresh = \r l (Just (v,sel)) -> (r,(toView r,sel),Nothing)
		} (attributes @>> editor) @ (\(container,(_,sel)) -> fromView container sel)

editSharedSelection :: ![SelectOption c a] c (Shared sds [Int]) -> Task [a] | iTask c & iTask a & RWShared sds
editSharedSelection options container sharedSel = editSharedSelection` (selectAttributes options) (selectEditor options) container sharedSel
editSharedSelection` attributes (SelectUsing toView fromView editor) container sharedSel
	= interactRW sharedSel
		{onInit = \r        -> ((), Update (toView container,r))
		,onEdit = \(_,vs) l -> (l, Just (const vs))
		,onRefresh = \r l (Just (vt,vs)) -> (l,(vt,r),Nothing)
		} (attributes @>> editor) @ (\(_,(_,sel)) -> fromView container sel)

editSharedSelectionWithShared :: ![SelectOption c a] (sds1 () c w) (Shared sds2 [Int]) -> Task [a] | iTask c & iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedSelectionWithShared options sharedContainer sharedSel
	= editSharedSelectionWithShared` (selectAttributes options) (selectEditor options) sharedContainer sharedSel
editSharedSelectionWithShared` attributes (SelectUsing toView fromView editor) sharedContainer sharedSel
	= interactRW (sharedContainer |*< sharedSel)
		{onInit = \(rc, rs)  -> (rc, Update (toView rc,rs))
		,onEdit = \(_, vs) l -> (l, Just (const vs))
		,onRefresh = \(rc, rs)   _ _ -> (rc, (toView rc, rs), Nothing)
		} (attributes @>> editor) @ (\(container, (_, sel)) -> fromView container sel)

//Core choice tasks
editChoice :: ![ChoiceOption a] ![a] (Maybe a) -> Task a | iTask a
editChoice options container mbSel = editChoiceAs options container id mbSel

editChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | iTask o & iTask a
editChoiceAs vopts container target mbSel = editSelection [SelectMultiple False:selectOptions target vopts] container (findIndex target mbSel container) @? tvHd

editMultipleChoice :: ![ChoiceOption a] ![a] [a] -> Task [a] | iTask a
editMultipleChoice options container mbSel = editMultipleChoiceAs options container id mbSel

editMultipleChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | iTask o & iTask a
editMultipleChoiceAs vopts container target sel = editSelection [SelectMultiple True:selectOptions target vopts] container (findIndices target sel container)

enterChoice :: ![ChoiceOption a] ![a] -> Task a | iTask a
enterChoice options container = editChoice options container Nothing

enterChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) -> Task a | iTask o & iTask a
enterChoiceAs options container targetFun = editChoiceAs options container targetFun Nothing

enterMultipleChoice :: ![ChoiceOption a] ![a] -> Task [a] | iTask a
enterMultipleChoice options container = editMultipleChoice options container []

enterMultipleChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) -> Task [a] | iTask o & iTask a
enterMultipleChoiceAs options container targetFun = editMultipleChoiceAs options container targetFun []

updateChoice :: ![ChoiceOption a] ![a] a -> Task a | iTask a
updateChoice options container sel = editChoice options container (Just sel)

updateChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) a -> Task a | iTask o & iTask a
updateChoiceAs options container targetFun sel = editChoiceAs options container targetFun (Just sel)

updateMultipleChoice :: ![ChoiceOption a] ![a] [a] -> Task [a] | iTask a
updateMultipleChoice options container sel = editMultipleChoice options container sel

updateMultipleChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | iTask o & iTask a
updateMultipleChoiceAs options container targetFun sel = editMultipleChoiceAs options container targetFun sel

editChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) (Maybe a) -> Task a | iTask a & TC w & RWShared sds
editChoiceWithShared options container mbSel = editChoiceWithSharedAs options container id mbSel

editChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) (Maybe a) -> Task a | iTask o & TC w & iTask a & RWShared sds
editChoiceWithSharedAs vopts sharedContainer target mbSel
	= editSelectionWithShared [SelectMultiple False:selectOptions target vopts] sharedContainer (findIndex target mbSel) @? tvHd

editMultipleChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | iTask a & TC w & RWShared sds
editMultipleChoiceWithShared options container sel = editMultipleChoiceWithSharedAs options container id sel

editMultipleChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | iTask o & TC w & iTask a & RWShared sds
editMultipleChoiceWithSharedAs vopts sharedContainer target sel
	= editSelectionWithShared [SelectMultiple True:selectOptions target vopts] sharedContainer (findIndices target sel)

enterChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) -> Task a | iTask a & TC w & RWShared sds
enterChoiceWithShared options container = editChoiceWithShared options container Nothing

enterChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task a | iTask o & TC w & iTask a & RWShared sds
enterChoiceWithSharedAs options container targetFun = editChoiceWithSharedAs options container targetFun Nothing

enterMultipleChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) -> Task [a] | iTask a & TC w & RWShared sds
enterMultipleChoiceWithShared options container = editMultipleChoiceWithShared options container []

enterMultipleChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task [a] | iTask o & TC w & iTask a & RWShared sds
enterMultipleChoiceWithSharedAs options container targetFun = editMultipleChoiceWithSharedAs options container targetFun []

updateChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) a -> Task a | iTask a & TC w & RWShared sds
updateChoiceWithShared options container sel = editChoiceWithShared options container (Just sel)

updateChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) a -> Task a | iTask o & TC w & iTask a & RWShared sds
updateChoiceWithSharedAs options container targetFun sel = editChoiceWithSharedAs options container targetFun (Just sel)

updateMultipleChoiceWithShared :: ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | iTask a & TC w & RWShared sds
updateMultipleChoiceWithShared options container sel = editMultipleChoiceWithShared options container sel

updateMultipleChoiceWithSharedAs :: ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | iTask o & TC w & iTask a & RWShared sds
updateMultipleChoiceWithSharedAs options container targetFun sel = editMultipleChoiceWithSharedAs options container targetFun sel

editSharedChoice :: ![ChoiceOption a] ![a] (Shared sds (Maybe a)) -> Task a | iTask a & RWShared sds
editSharedChoice options container sharedSel = editSharedChoiceAs options container id sharedSel

editSharedChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) (Shared sds (Maybe a)) -> Task a | iTask o & iTask a & RWShared sds
editSharedChoiceAs vopts container target sharedSel
	= editSharedSelection [SelectMultiple False:selectOptions target vopts] container (findIndexShare target container sharedSel) @? tvHd

editSharedMultipleChoice :: ![ChoiceOption a] ![a] (Shared sds [a]) -> Task [a] | iTask a & RWShared sds
editSharedMultipleChoice options container sharedSel = editSharedMultipleChoiceAs options container id sharedSel

editSharedMultipleChoiceAs :: ![ChoiceOption o] ![o] !(o -> a) (Shared sds [a]) -> Task [a] | iTask o & iTask a & RWShared sds
editSharedMultipleChoiceAs vopts container target sharedSel
	= editSharedSelection [SelectMultiple True:selectOptions target vopts] container (findIndicesShare target container sharedSel)

editSharedChoiceWithShared :: ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 (Maybe a)) -> Task a | iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedChoiceWithShared options sharedContainer sharedSel = editSharedChoiceWithSharedAs options sharedContainer id sharedSel

editSharedChoiceWithSharedAs :: ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 (Maybe a)) -> Task a | iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2
editSharedChoiceWithSharedAs vopts sharedContainer target sharedSel
	= editSharedSelectionWithShared [SelectMultiple False:selectOptions target vopts] sharedContainer (findIndexShareWithShared target (sharedContainer |*< sharedSel)) @? tvHd

editSharedMultipleChoiceWithShared :: ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 [a]) -> Task [a] | iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithShared options sharedContainer sharedSel = editSharedMultipleChoiceWithSharedAs options sharedContainer id sharedSel

editSharedMultipleChoiceWithSharedAs :: ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 [a]) -> Task [a] | iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithSharedAs vopts sharedContainer target sharedSel
	= editSharedSelectionWithShared [SelectMultiple True:selectOptions target vopts] sharedContainer (findIndicesShareWithShared target (sharedContainer |*< sharedSel))

findIndex :: (o -> a) (Maybe a) [o] -> [Int] | gEq{|*|} a
findIndex target Nothing options = []
findIndex target (Just val) options = [i \\ o <- options & i <- [0..] | target o === val]

findIndices :: (o -> a) [a] [o] -> [Int] | gEq{|*|} a
findIndices target vals options = [i \\ o <- options & i <- [0..] | isMemberGen (target o) vals]

findIndexShare :: (o -> a) [o] (Shared sds (Maybe a)) -> SimpleSDSLens [Int] | TC a & RWShared sds & gEq{|*|} a
findIndexShare target options sds = mapReadWrite (tof sds target options,fromf sds target options) Nothing sds
where
	tof :: (Shared sds (Maybe a)) (o -> a) [o] (Maybe a) -> [Int] |  gEq{|*|} a
	tof _ target options mbv = findIndex target mbv options

	fromf :: (Shared sds (Maybe a)) (o -> a) [o] [Int] (Maybe a) -> Maybe (Maybe a) |  gEq{|*|} a
	fromf _ target options w _ = Just (listToMaybe (findSelection target options w))

findIndicesShare :: (o -> a) [o] (Shared sds [a]) -> SimpleSDSLens [Int] | TC a & RWShared sds & gEq{|*|} a
findIndicesShare target options sds = mapReadWrite (tof,fromf) Nothing sds
where
	tof v = findIndices target v options
	fromf w _ = Just (findSelection target options w)

findIndexShareWithShared target sds = mapReadWrite (tof,fromf) Nothing sds
where
	tof (options,mbv) = findIndex target mbv options

	fromf w (options,_) = Just (listToMaybe (findSelection target options w))

findIndicesShareWithShared target sds = mapReadWrite (tof,fromf) Nothing sds
where
	tof (options,mbv) = findIndices target mbv options
	fromf w (options,_) = Just (findSelection target options w)

wait :: (r -> Bool) !(sds () r w) -> Task r | iTask r & TC w & Registrable sds
wait pred shared
	=	viewSharedInformation [ViewAs (const "Waiting for information update")] shared
	>>* [OnValue (ifValue pred return)]

chooseAction :: ![(Action,a)] -> Task a | iTask a
chooseAction actions
	=	viewInformation [] ()
	>>* [OnAction action (always (return val)) \\ (action,val) <- actions]

viewTitle :: !a -> Task a | iTask a
viewTitle a = Title title @>> viewInformation [ViewAs view] a
where
	title = toSingleLineText a
	view a	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text title]]

viewSharedTitle :: !(sds () r w) -> Task r | iTask r & Registrable sds & TC w
viewSharedTitle s = whileUnchanged s viewTitle

crudWith :: ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (sds () (f r) (f` w))
         -> Task r | iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
crudWith choiceOpts enterOpts viewOpts updateOpts toList putItem delItem sh = goCRUD
  where
  goCRUD
    =   enterChoiceWithShared choiceOpts (mapRead toList sh)
    >>* [ OnAction (Action "New")    (always   newItem)
        , OnAction (Action "View")   (hasValue viewItem)
        , OnAction (Action "Edit")   (hasValue editItem)
        , OnAction (Action "Delete") (hasValue deleteItem)
        ]
  newItem
    =            Title "New item" @>> enterInformation enterOpts
    >>= \item -> upd (putItem item) sh
    >-|          goCRUD
  viewItem x
    =            Title "View item" @>> viewInformation viewOpts x
    >>|          goCRUD
  editItem x
    =            Title "Edit item" @>> updateInformation updateOpts x
    >>= \item -> upd (putItem item) sh
    >-|          goCRUD
  deleteItem x
    =            upd (delItem x) sh
    >-|          goCRUD

crud :: !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (sds () (f r) (f` w))
     -> Task r | iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
crud toList putItem delItem sh = crudWith [] [] [] [] toList putItem delItem sh
