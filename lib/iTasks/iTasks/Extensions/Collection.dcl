definition module iTasks.Extensions.Collection
/**
* This module provides generic tasks for managing data collections
* It provides basic CRUD functions for a shared list of items
*/
import iTasks

/**
* Generic CRUD task for managing a collection of items
*
* @param	Description of the task
* @param	Item name, this is used to make task descriptions for the subtasks (E.g. "person" -> "Select person","Add person" etc.)
* @param	Identification function. Required to identify items in the collection when updating
* @param	Shared collection
*
* @return	The last selection
*/
manageCollection :: !String (c -> i) (Shared [c]) -> Task (Maybe i) | iTask c & iTask i

/**
* Configurable collection management task.
*
* @param	Description of the task
* @param	Selection task, task that makes a selection from the collection
* @param	Use selection task, used in parallel with selection and restarted when selection changes
* @param	Selection actions. These are added to the selection task
* @param	Identification function. Required to identify items in the collection when updating
* @param	Shared item function. Derives a shared single item from a shared collection
* @param	Shared collection
*
* @return	The last selection 
*/
manageCollectionWith ::
	((Shared [c]) (c -> i) -> Task i)											//Make selection
	((Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task a)		//Use selection
	[TaskCont i (Task (Maybe i))]												//Actions
	(c -> i)																	//Identification function
	((Shared [c]) i -> Shared (Maybe c))										//Item share function
	(Shared [c])																//Shared collection
	-> Task (Maybe i) | iTask c & iTask i & iTask a

/**
* Create an item share by looking up an item identified by an identitication function
*
* @param	Identification function. Required to identify items in the collection when updating
* @param	Shared collection
* @param	Item identification
*/
itemShare :: (c -> i) (Shared [c]) i -> Shared (Maybe c) | gEq{|*|} i & gEq{|*|} c
/**
* Select an item from a shared collection and project the selection on another shared state
*
*/
selectItem :: !d (Shared [c]) (c -> i) -> Task i | toPrompt d & iTask c & iTask i
/**
* View an item in the collection (without actions)
*/
viewItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task (Maybe i) | toPrompt d & iTask c & iTask i
/**
* Add an item to the collection
*/
addItem :: !d (Shared [c]) (c -> i) -> Task (Maybe i) | toPrompt d & iTask i & iTask c
/**
* Edit an item in the collection
*/
editItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | toPrompt d & iTask c & iTask i
/**
* Delete an item from the collection
*/
deleteItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | toPrompt d & iTask c & iTask i
