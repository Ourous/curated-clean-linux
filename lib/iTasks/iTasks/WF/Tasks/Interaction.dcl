definition module iTasks.WF.Tasks.Interaction

import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: Action
from iTasks.UI.Prompt import class toPrompt
from iTasks.UI.Editor.Controls import :: ChoiceText, :: ChoiceGrid, :: ChoiceNode
import iTasks.SDS.Definition

from Data.Functor import class Functor

/*** General input/update/output tasks ***/

:: ViewOption a 		= E.v: ViewAs 	    (a -> v)                       & iTask v
						| E.v: ViewUsing 	(a -> v) (Editor v)            & iTask v //Use a custom editor to view the data

:: EnterOption a		= E.v: EnterAs      (v -> a)                       & iTask v
						| E.v: EnterUsing 	(v -> a) (Editor v)            & iTask v //Use a custom editor to enter the data

:: UpdateOption a b		= E.v: UpdateAs     (a -> v) (a v -> b)	           & iTask v
						| E.v: UpdateUsing  (a -> v) (a v -> b) (Editor v) & iTask v //Use a custom editor to enter the data
                        //When using an update option for a task that uses a shared data source
                        //you can use UpdateWithShared instead of UpdateWith which allows you
                        //to specify how the view must be updated when both the share changed and
                        //the user changed the view simultaneously. This conflict resolution function
                        //is applied before the new 'b' is generated from the view ('v') value
                        | E.v: UpdateSharedAs (a -> v) (a v -> b) (v v -> v)  & iTask v

//Selection in arbitrary containers (explicit identification is needed)
:: SelectOption c s     = SelectInDropdown   (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInCheckGroup (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInList       (c -> [ChoiceText]) (c [Int] -> [s])
     					| SelectInGrid       (c -> ChoiceGrid)   (c [Int] -> [s])
     					| SelectInTree       (c -> [ChoiceNode]) (c [Int] -> [s])

//Choosing from lists
:: ChoiceOption o	    = E.v: ChooseFromDropdown (o -> v)   & iTask v
						| E.v: ChooseFromCheckGroup (o -> v) & iTask v
						| E.v: ChooseFromList (o -> v)       & iTask v
						| E.v: ChooseFromGrid (o -> v)       & iTask v

/*** General input/update/output tasks ***/

/**
* Ask the user to enter information.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Views
*
* @return					Value entered by the user
*/
enterInformation :: !d ![EnterOption m] -> Task m | toPrompt d & iTask m

/**
* Ask the user to update predefined information.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; if no view is defined a default view with the id lens is used
* @param Data model:		The data updated by the user
*
* @return					Value updated by the user
*/
updateInformation :: !d ![UpdateOption m m] m -> Task m | toPrompt d & iTask m

/**
* Show information to the user.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; only get parts of Views are used, Putbacks are ignored; if no get is defined the id get is used
* @param Data model:		The data shown to the user
*
* @return					Value shown to the user, the value is not modified
*/
viewInformation :: !d ![ViewOption m] !m -> Task m | toPrompt d & iTask m

/**
* Ask the user to update predefined local and shared information.
*
* @param Description:		A description of the task to display to the user
* @param Views:				Interaction views; if no view is defined & w = r a default view with the id lens is used, if r <> w the value of the shared state (r) is shown to the user; the default for the local data is always the id lens
* @param Shared:			Reference to the shared state to update
* @param Local:				The local data updated by the user

* @return 					Current value of the shared thats being modified and local modified copy
*/
updateSharedInformation :: !d ![UpdateOption r w] !(sds () r w) -> Task r | toPrompt d & iTask r & iTask w & RWShared sds

/**
* Show a shared value.
*
* @param Description:		A description of the task to display to the user
* @param Options:			Views options
* @param Shared:			Reference to the shared state to monitor
*
* @return					Last value of the monitored state
*/
viewSharedInformation :: !d ![ViewOption r] !(sds () r w) -> Task r | toPrompt d & iTask r & TC w & Registrable sds

/*** Special tasks for a mix of manipulating shared and local information ***/

/**
* Update a local value, making use of shared information.
*/
updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(sds () r w) m -> Task m | toPrompt d & iTask r & iTask m & TC w & RWShared sds

/**
* General selection with explicit identification in arbitrary containers
*/

//Options: local, selection: local
editSelection :: !d !Bool !(SelectOption c a) c [Int] -> Task [a] | toPrompt d & iTask a

//Options: shared, selection: local
editSelectionWithShared :: !d !Bool !(SelectOption c a) (sds () c w) (c -> [Int]) -> Task [a] | toPrompt d & iTask c & iTask a & TC w & RWShared sds

//Options: local, selection: shared
editSharedSelection :: !d !Bool !(SelectOption c a) c (Shared sds [Int]) -> Task [a] | toPrompt d & iTask c & iTask a & RWShared sds

//Options: shared, selection: shared
editSharedSelectionWithShared :: !d !Bool !(SelectOption c a) (sds1 () c w) (Shared sds2 [Int]) -> Task [a] | toPrompt d & iTask c & iTask a & TC w & RWShared sds1 & RWShared sds2

/**
* More specific selection from lists
*/
editChoice                           :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | toPrompt d & iTask a
editChoiceAs                         :: !d ![ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask a
editMultipleChoice                   :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
editMultipleChoiceAs                 :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a

enterChoice                          :: !d ![ChoiceOption a] ![a] -> Task a | toPrompt d & iTask a
enterChoiceAs                        :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task a | toPrompt d & iTask o & iTask a
enterMultipleChoice                  :: !d ![ChoiceOption a] ![a] -> Task [a] | toPrompt d & iTask a
enterMultipleChoiceAs                :: !d ![ChoiceOption o] ![o] !(o -> a) -> Task [a] | toPrompt d & iTask o & iTask a

updateChoice                         :: !d ![ChoiceOption a] ![a] a -> Task a | toPrompt d & iTask a
updateChoiceAs                       :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | toPrompt d & iTask o & iTask a
updateMultipleChoice                 :: !d ![ChoiceOption a] ![a] [a] -> Task [a] | toPrompt d & iTask a
updateMultipleChoiceAs               :: !d ![ChoiceOption o] ![o] !(o -> a) [a] -> Task [a] | toPrompt d & iTask o & iTask a

editChoiceWithShared                 :: !d ![ChoiceOption a] !(sds () [a] w) (Maybe a) -> Task a | toPrompt d & iTask a & TC w & RWShared sds
editChoiceWithSharedAs               :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & TC w & iTask a & RWShared sds
editMultipleChoiceWithShared         :: !d ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | toPrompt d & iTask a & TC w & RWShared sds
editMultipleChoiceWithSharedAs       :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & TC w & iTask a & RWShared sds

enterChoiceWithShared                :: !d ![ChoiceOption a] !(sds () [a] w) -> Task a | toPrompt d & iTask a & TC w & RWShared sds
enterChoiceWithSharedAs              :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task a | toPrompt d & iTask o & TC w & iTask a & RWShared sds
enterMultipleChoiceWithShared        :: !d ![ChoiceOption a] !(sds () [a] w) -> Task [a] | toPrompt d & iTask a & TC w & RWShared sds
enterMultipleChoiceWithSharedAs      :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) -> Task [a] | toPrompt d & iTask o & TC w & iTask a & RWShared sds

updateChoiceWithShared               :: !d ![ChoiceOption a] !(sds () [a] w) a -> Task a | toPrompt d & iTask a & TC w & RWShared sds
updateChoiceWithSharedAs             :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) a -> Task a | toPrompt d & iTask o & TC w & iTask a & RWShared sds
updateMultipleChoiceWithShared       :: !d ![ChoiceOption a] !(sds () [a] w) [a] -> Task [a] | toPrompt d & iTask a & TC w & RWShared sds
updateMultipleChoiceWithSharedAs     :: !d ![ChoiceOption o] !(sds () [o] w) (o -> a) [a] -> Task [a] | toPrompt d & iTask o & TC w & iTask a & RWShared sds

editSharedChoice                     :: !d ![ChoiceOption a] ![a] (Shared sds (Maybe a)) -> Task a | toPrompt d & iTask a & RWShared sds
editSharedChoiceAs                   :: !d [ChoiceOption o] ![o] !(o -> a) (Shared sds (Maybe a)) -> Task a | toPrompt d & iTask o & iTask a & RWShared sds
editSharedMultipleChoice             :: !d ![ChoiceOption a] ![a] (Shared sds [a]) -> Task [a] | toPrompt d & iTask a & RWShared sds
editSharedMultipleChoiceAs           :: !d [ChoiceOption o] ![o] !(o -> a) (Shared sds [a]) -> Task [a] | toPrompt d & iTask o & iTask a & RWShared sds

editSharedChoiceWithShared           :: !d ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 (Maybe a)) -> Task a | toPrompt d & iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedChoiceWithSharedAs         :: !d ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 (Maybe a)) -> Task a | toPrompt d & iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithShared   :: !d ![ChoiceOption a] !(sds1 () [a] w) (Shared sds2 [a]) -> Task [a] | toPrompt d & iTask a & TC w & RWShared sds1 & RWShared sds2
editSharedMultipleChoiceWithSharedAs :: !d ![ChoiceOption o] !(sds1 () [o] w) (o -> a) (Shared sds2 [a]) -> Task [a] | toPrompt d & iTask o & TC w & iTask a & RWShared sds1 & RWShared sds2

/**
* Wait for a share to match a certain predicate
*
* @param Description:		A description of the task to display to the user
* @param Predicate:			A predicate to test when to continue. The task completes as soon as the predicate is true
* @param Shared:			Reference to the shared state to wait for
*
* @return					The value of the shared when the predicate becomes true
*/
wait :: !d (r -> Bool) !(sds () r w) -> Task r | toPrompt d & iTask r & TC w & Registrable sds


/*** Special tasks for choosing actions ***/

/**
* Ask the user to choose an action.
*
* @param Action list:	A list of actions the user can choose from. Each actions yields the given result if it's chosen.
*
* @return 				Value associated with chosen action
*/
chooseAction :: ![(!Action,a)] -> Task a | iTask a

/**
* View data as a title
*/
viewTitle :: !a -> Task a | iTask a

/**
* View shared data as a title
*/
viewSharedTitle :: !(sds () r w) -> Task r | iTask r & Registrable sds & TC w

/**
* Basic Create, Read, Update, Delete (CRUD) editor for a shared collection
*/
crud :: !d !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (sds () (f r) (f` w))
     -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds

crudWith :: !d ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (sds () (f r) (f` w))
         -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
