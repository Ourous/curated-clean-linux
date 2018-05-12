implementation module iTasks.WF.Tasks.Interaction

from StdFunc import id, const, o, flip
from Data.Tuple import appSnd
from Data.List import isMemberGen, findIndex, instance Functor [], getItems
from Data.Map import qualified get, put

import StdBool, StdList, StdMisc, StdTuple, Data.Functor
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.SDS
import iTasks.WF.Combinators.Overloaded
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Core
import iTasks.SDS.Sources.System
import iTasks.SDS.Combinators.Common
import iTasks.Internal.Util
import iTasks.UI.Layout, iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Prompt, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Text.HTML

derive class iTask ChoiceText, ChoiceGrid, ChoiceRow, ChoiceNode

unitShare :: SDS () () ()
unitShare = nullShare

enterInformation :: !d ![EnterOption m] -> Task m | toPrompt d & iTask m
enterInformation d [EnterAs fromf:_]
	= interact d Enter unitShare {onInit = const ((),defaultValue), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)} gEditor{|*|} @ (\((),v) -> fromf v) 
enterInformation d opts=:[EnterUsing fromf editor:_]
	= interact d Enter unitShare {onInit = const ((),defaultValue), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)} editor @ (\((),v) -> fromf v) 
enterInformation d _ = enterInformation d [EnterAs id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | toPrompt d & iTask m
updateInformation d [UpdateAs tof fromf:_] m
	= interact d Update unitShare {onInit = const ((),tof m), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)}
		gEditor{|*|} @ (\((),v) -> fromf m v)
updateInformation d [UpdateUsing tof fromf editor:_] m
	= interact d Update unitShare {onInit = const ((),tof m), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)}
		editor @ (\((),v) -> fromf m v)
updateInformation d _ m = updateInformation d [UpdateAs (\l -> l) (\_ v -> v)] m

viewInformation :: !d ![ViewOption m] !m -> Task m | toPrompt d & iTask m
viewInformation d [ViewAs tof:_] m 
	= interact d View unitShare {onInit = const ((),tof m), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)} gEditor{|*|} @! m
viewInformation d [ViewUsing tof editor:_] m
	= interact d View unitShare {onInit = const ((),tof m), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r l v -> (l,v,Nothing)} editor @! m
viewInformation d _ m = viewInformation d [ViewAs id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & iTask w
updateSharedInformation d [UpdateAs tof fromf:_] shared
	= interact d Update shared {onInit = \r -> (r, tof r), onEdit = \v l _ -> (l,v,Just (\r -> fromf r v)), onRefresh = \r _ v -> (r,tof r,Nothing)}
				gEditor{|*|} @ fst

updateSharedInformation d [UpdateUsing tof fromf editor:_] shared
	= interact d Update shared {onInit = \r -> (r,tof r), onEdit = \v l _ -> (l,v,Just (\r -> fromf r v)), onRefresh = \r _ v -> (r,tof r,Nothing)}
				editor @ fst

updateSharedInformation d [UpdateSharedAs tof fromf conflictf:_] shared
	= interact d Update shared {onInit = \r -> (r,tof r), onEdit = \v l _ -> (l,v,Just (\r -> fromf r v)), onRefresh = \r _ v -> (r,conflictf (tof r) v, Nothing)}
				gEditor{|*|} @ fst

updateSharedInformation d _ shared			
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we just display the read type r 
	= case dynamic id :: A.a: (a -> a) of
		(rtow :: r^ -> w^) = updateSharedInformation d [UpdateAs rtow (flip const)] shared 
		_                  = viewSharedInformation d [] shared

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & TC w
viewSharedInformation d [ViewAs tof:_] shared
	= interact d View shared {onInit = \r -> (r,tof r), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r _ v -> (r,tof r,Nothing)} gEditor{|*|} @ fst

viewSharedInformation d [ViewUsing tof editor:_] shared
	= interact d View shared {onInit = \r -> (r,tof r), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \r _ v -> (r,tof r,Nothing)} editor @ fst
viewSharedInformation d _ shared = viewSharedInformation d [ViewAs id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | toPrompt d & iTask r & iTask m & TC w
updateInformationWithShared d [UpdateAs tof fromf:_] shared m
	= interact d Update shared
		{onInit = \r -> ((r,m),tof (r,m))
		,onEdit = \v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing)
		,onRefresh = \r (_,m) v -> ((r,m),tof (r,m),Nothing)
		} gEditor{|*|} @ (snd o fst)

updateInformationWithShared d [UpdateUsing tof fromf editor:_] shared m
	= interact d Update shared
		{onInit = \r -> ((r,m),tof (r,m))
		,onEdit = \v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing)
		,onRefresh = \r (_,m) v -> ((r,m),tof (r,m),Nothing)
		} editor @ (snd o fst)

updateInformationWithShared d _ shared m
    = updateInformation d [] m

editSelection :: !d !Bool !(SelectOption c a) c [Int] -> Task [a] | toPrompt d & iTask a
editSelection d multi (SelectInDropdown toView fromView) container sel = editSelection` d (dropdown <<@ multipleAttr multi) toView fromView container sel
editSelection d multi (SelectInCheckGroup toView fromView) container sel = editSelection` d (checkGroup <<@ multipleAttr multi) toView fromView container sel
editSelection d multi (SelectInList toView fromView) container sel = editSelection` d (choiceList <<@ multipleAttr multi) toView fromView container sel
editSelection d multi (SelectInGrid toView fromView) container sel = editSelection` d (grid <<@ multipleAttr multi) toView fromView container sel
editSelection d multi (SelectInTree toView fromView) container sel = editSelection` d (tree <<@ multipleAttr multi) toView fromView container sel
editSelection` d editor toView fromView container sel
	= interact d (if (isEmpty sel) Enter Update) unitShare
		{onInit = \r     -> ((),(toView container,sel))
		,onEdit = \v l _ -> (l,v,Nothing)
		,onRefresh = \_ l v -> (l,v,Nothing)
		} editor @ (\(_,(_,sel)) -> fromView container sel)

editSelectionWithShared :: !d !Bool !(SelectOption c a) (ReadWriteShared c w) (c -> [Int]) -> Task [a] | toPrompt d & iTask c & iTask a & TC w
editSelectionWithShared d multi (SelectInDropdown toView fromView) sharedContainer initSel = editSelectionWithShared` d (dropdown <<@ multipleAttr multi) toView fromView sharedContainer initSel
editSelectionWithShared d multi (SelectInCheckGroup toView fromView) sharedContainer initSel = editSelectionWithShared` d (checkGroup <<@ multipleAttr multi) toView fromView sharedContainer initSel
editSelectionWithShared d multi (SelectInList toView fromView) sharedContainer initSel = editSelectionWithShared` d (choiceList <<@ multipleAttr multi) toView fromView sharedContainer initSel
editSelectionWithShared d multi (SelectInGrid toView fromView) sharedContainer initSel = editSelectionWithShared` d (grid <<@ multipleAttr multi) toView fromView sharedContainer initSel
editSelectionWithShared d multi (SelectInTree toView fromView) sharedContainer initSel = editSelectionWithShared` d (tree <<@ multipleAttr multi) toView fromView sharedContainer initSel
editSelectionWithShared` d editor toView fromView sharedContainer initSel
	= interact d Update sharedContainer 
		{onInit = \r     -> (r,(toView r, initSel r))
		,onEdit = \v l _ -> (l,v,Nothing)
		,onRefresh = \r l (v,sel) -> (r,(toView r,sel),Nothing)
		} editor @ (\(container,(_,sel)) -> fromView container sel)

editSharedSelection :: !d !Bool !(SelectOption c a) c (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 
editSharedSelection d multi (SelectInDropdown toView fromView) container sharedSel = editSharedSelection` d (dropdown <<@ multipleAttr multi) toView fromView container sharedSel
editSharedSelection d multi (SelectInCheckGroup toView fromView) container sharedSel = editSharedSelection` d (checkGroup <<@ multipleAttr multi) toView fromView container sharedSel
editSharedSelection d multi (SelectInList toView fromView) container sharedSel = editSharedSelection` d (choiceList <<@ multipleAttr multi) toView fromView container sharedSel
editSharedSelection d multi (SelectInGrid toView fromView) container sharedSel = editSharedSelection` d (grid <<@ multipleAttr multi) toView fromView container sharedSel
editSharedSelection d multi (SelectInTree toView fromView) container sharedSel = editSharedSelection` d (tree <<@ multipleAttr multi) toView fromView container sharedSel
editSharedSelection` d editor toView fromView container sharedSel 
	= interact d Update sharedSel
		{onInit = \r           -> ((),(toView container,r))
		,onEdit = \(vt,vs) l _ -> (l,(vt,vs),Just (const vs))
		,onRefresh = \r l (vt,vs) -> (l,(vt,r),Nothing)
		} editor @ (\(_,(_,sel)) -> fromView container sel)

editSharedSelectionWithShared :: !d !Bool !(SelectOption c a) (ReadWriteShared c w) (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a & TC w
editSharedSelectionWithShared d multi (SelectInDropdown toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d (dropdown <<@ multipleAttr multi) toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d multi (SelectInCheckGroup toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d (checkGroup <<@ multipleAttr multi) toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d multi (SelectInList toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d (choiceList <<@ multipleAttr multi) toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d multi (SelectInGrid toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d (grid <<@ multipleAttr multi) toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d multi (SelectInTree toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d (tree <<@ multipleAttr multi) toView fromView sharedContainer sharedSel
editSharedSelectionWithShared` d editor toView fromView sharedContainer sharedSel 
	= interact d Update (sharedContainer |+< sharedSel)
		{onInit = \(rc, rs)       -> (rc, (toView rc,rs))
		,onEdit = \v=:(_, vs) l _ -> (l, v, Just (const vs))
		,onRefresh = \(rc, rs)   _ _ -> (rc, (toView rc, rs), Nothing)
		} editor @ (\(container, (_, sel)) -> fromView container sel)

//Core choice tasks
editChoice :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | toPrompt d & iTask a
editChoice d options container mbSel = editChoiceAs d options container id mbSel

editChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask a
editChoiceAs d vopts container target mbSel = editSelection d False (selectOption target vopts) container (findIndex target mbSel container) @? tvHd

editMultipleChoice :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
editMultipleChoice d options container mbSel = editMultipleChoiceAs d options container id mbSel

editMultipleChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a
editMultipleChoiceAs d vopts container target sel = editSelection d True (selectOption target vopts) container (findIndices target sel container)

enterChoice :: !d ![ChoiceOption a] ![a] -> Task a | toPrompt d & iTask a
enterChoice d options container = editChoice d options container Nothing

enterChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task a | toPrompt d & iTask o & iTask a
enterChoiceAs d options container targetFun = editChoiceAs d options container targetFun Nothing

enterMultipleChoice :: !d ![ChoiceOption a] ![a] -> Task [a] | toPrompt d & iTask a
enterMultipleChoice d options container = editMultipleChoice d options container []

enterMultipleChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task [a] | toPrompt d & iTask o & iTask a
enterMultipleChoiceAs d options container targetFun = editMultipleChoiceAs d options container targetFun []

updateChoice :: !d ![ChoiceOption a] ![a] a -> Task a | toPrompt d & iTask a
updateChoice d options container sel = editChoice d options container (Just sel)

updateChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | toPrompt d & iTask o & iTask a
updateChoiceAs d options container targetFun sel = editChoiceAs d options container targetFun (Just sel)

updateMultipleChoice   :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
updateMultipleChoice d options container sel = editMultipleChoice d options container sel

updateMultipleChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a
updateMultipleChoiceAs d options container targetFun sel = editMultipleChoiceAs d options container targetFun sel

editChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Maybe a) -> Task a | toPrompt d & iTask a & iTask w
editChoiceWithShared d options container mbSel = editChoiceWithSharedAs d options container id mbSel

editChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editChoiceWithSharedAs d vopts sharedContainer target mbSel 
	= editSelectionWithShared d False (selectOption target vopts) sharedContainer (findIndex target mbSel) @? tvHd

editMultipleChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) [a] -> Task [a] | toPrompt d & iTask a & iTask w
editMultipleChoiceWithShared d options container sel = editMultipleChoiceWithSharedAs d options container id sel

editMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask w & iTask a
editMultipleChoiceWithSharedAs d vopts sharedContainer target sel
	= editSelectionWithShared d True (selectOption target vopts) sharedContainer (findIndices target sel)

enterChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task a | toPrompt d & iTask a & iTask w
enterChoiceWithShared d options container = editChoiceWithShared d options container Nothing

enterChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
enterChoiceWithSharedAs d options container targetFun = editChoiceWithSharedAs d options container targetFun Nothing

enterMultipleChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task [a] | toPrompt d & iTask a & iTask w
enterMultipleChoiceWithShared d options container = editMultipleChoiceWithShared d options container []

enterMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task [a] | toPrompt d & iTask o & iTask w & iTask a
enterMultipleChoiceWithSharedAs d options container targetFun = editMultipleChoiceWithSharedAs d options container targetFun []

updateChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) a -> Task a | toPrompt d & iTask a & iTask w
updateChoiceWithShared d options container sel = editChoiceWithShared d options container (Just sel)

updateChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) a -> Task a | toPrompt d & iTask o & iTask w & iTask a
updateChoiceWithSharedAs d options container targetFun sel = editChoiceWithSharedAs d options container targetFun (Just sel)

updateMultipleChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) [a] -> Task [a] | toPrompt d & iTask a & iTask w
updateMultipleChoiceWithShared d options container sel = editMultipleChoiceWithShared d options container sel

updateMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask w & iTask a
updateMultipleChoiceWithSharedAs d options container targetFun sel = editMultipleChoiceWithSharedAs d options container targetFun sel

editSharedChoice :: !d ![ChoiceOption a] ![a] (Shared (Maybe a)) -> Task a | toPrompt d & iTask a
editSharedChoice d options container sharedSel = editSharedChoiceAs d options container id sharedSel

editSharedChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask a
editSharedChoiceAs d vopts container target sharedSel 
	= editSharedSelection d False (selectOption target vopts) container (findIndexShare target container sharedSel) @? tvHd

editSharedMultipleChoice :: !d ![ChoiceOption a] ![a] (Shared [a]) -> Task [a] | toPrompt d & iTask a
editSharedMultipleChoice d options container sharedSel = editSharedMultipleChoiceAs d options container id sharedSel

editSharedMultipleChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared [a]) -> Task [a] | toPrompt d & iTask o & iTask a
editSharedMultipleChoiceAs d vopts container target sharedSel 
	= editSharedSelection d True (selectOption target vopts) container (findIndicesShare target container sharedSel)

editSharedChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared (Maybe a)) -> Task a | toPrompt d & iTask a & iTask w
editSharedChoiceWithShared d options sharedContainer sharedSel = editSharedChoiceWithSharedAs d options sharedContainer id sharedSel

editSharedChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editSharedChoiceWithSharedAs d vopts sharedContainer target sharedSel
	= editSharedSelectionWithShared d False (selectOption target vopts) sharedContainer (findIndexShareWithShared target (sharedContainer |+< sharedSel)) @? tvHd

editSharedMultipleChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared [a]) -> Task [a] | toPrompt d & iTask a & iTask w
editSharedMultipleChoiceWithShared d options sharedContainer sharedSel = editSharedMultipleChoiceWithSharedAs d options sharedContainer id sharedSel

editSharedMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared [a]) -> Task [a] | toPrompt d & iTask o & iTask w & iTask a
editSharedMultipleChoiceWithSharedAs d vopts sharedContainer target sharedSel
	= editSharedSelectionWithShared d True (selectOption target vopts) sharedContainer (findIndicesShareWithShared target (sharedContainer |+< sharedSel))

//Helper functions for the edit*Choice* tasks
selectOption :: (o -> s) [ChoiceOption o] -> SelectOption [o] s | gText{|*|} o
selectOption target opts = case opts of
	[(ChooseFromDropdown f):_]     = SelectInDropdown   (toTexts f)  (findSelection target)
	[(ChooseFromCheckGroup f):_]   = SelectInCheckGroup (toTexts f)  (findSelection target)
	[(ChooseFromList f):_]         = SelectInList       (toTexts f)  (findSelection target)
	[(ChooseFromGrid f):_]         = SelectInGrid       (toGrid f)   (findSelection target)
	_                              = SelectInDropdown   (toTexts id) (findSelection target)

toTexts f options = [{ChoiceText|id=i,text=toSingleLineText (f o)} \\ o <- options & i <- [0..]]
toGrid f options = {ChoiceGrid|header=gText{|*|} AsHeader (fixtype vals),rows = [{ChoiceRow|id=i,cells=map Text (gText{|*|} AsRow (Just v))} \\ v <- vals & i <- [0..]]}
where
	vals = map f options

	fixtype :: [a] -> Maybe a
	fixtype _ = Nothing

findSelection :: (o -> s) [o] [Int] -> [s]
findSelection target options idxs = target <$> getItems options idxs

findIndex target Nothing options = []
findIndex target (Just val) options = [i \\ o <- options & i <- [0..] | target o === val]

findIndices target vals options = [i \\ o <- options & i <- [0..] | isMemberGen (target o) vals]

findIndexShare target options sds = mapReadWrite (tof,fromf) sds
where
	tof mbv = findIndex target mbv options
	fromf w _ = Just (listToMaybe (findSelection target options w))

findIndicesShare target options sds = mapReadWrite (tof,fromf) sds
where
	tof v = findIndices target v options
	fromf w _ = Just (findSelection target options w)

findIndexShareWithShared target sds = mapReadWrite (tof,fromf) sds
where
	tof (options,mbv) = findIndex target mbv options
	fromf w (options,_) = Just (listToMaybe (findSelection target options w))

findIndicesShareWithShared target sds = mapReadWrite (tof,fromf) sds
where
	tof (options,mbv) = findIndices target mbv options
	fromf w (options,_) = Just (findSelection target options w)

wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & TC w
wait desc pred shared
	=	viewSharedInformation desc [ViewAs (const "Waiting for information update")] shared
	>>* [OnValue (ifValue pred return)]
	
chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions
	=	viewInformation () [] ()
	>>* [OnAction action (always (return val)) \\ (action,val) <- actions]

viewTitle :: !a -> Task a | iTask a
viewTitle a = viewInformation (Title title) [ViewAs view] a
where
	title = toSingleLineText a
	view a	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text title]]

viewSharedTitle :: !(ReadWriteShared r w) -> Task r | iTask r
viewSharedTitle s = whileUnchanged s viewTitle

crudWith :: !d ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (RWShared () (f r) (f` w))
         -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)
crudWith descr choiceOpts enterOpts viewOpts updateOpts toList putItem delItem sh = goCRUD
  where
  goCRUD
    =   enterChoiceWithShared descr choiceOpts (mapRead toList sh)
    >>* [ OnAction (Action "New")    (always   newItem)
        , OnAction (Action "View")   (hasValue viewItem)
        , OnAction (Action "Edit")   (hasValue editItem)
        , OnAction (Action "Delete") (hasValue deleteItem)
        ]
  newItem
    =            enterInformation (Title "New item") enterOpts
    >>= \item -> upd (putItem item) sh
    >>|          goCRUD
  viewItem x
    =            viewInformation (Title "View item") viewOpts x
    >>|          goCRUD
  editItem x
    =            updateInformation (Title "Edit item") updateOpts x
    >>= \item -> upd (putItem item) sh
    >>|          goCRUD
  deleteItem x
    =            upd (delItem x) sh
    >>|          goCRUD

crud :: !d !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (RWShared () (f r) (f` w))
     -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)
crud descr toList putItem delItem sh = crudWith descr [] [] [] [] toList putItem delItem sh
