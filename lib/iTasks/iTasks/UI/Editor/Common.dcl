definition module iTasks.UI.Editor.Common
/**
* This module provides some convenient editors
*/
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Definition import :: UI, :: UIChildChange, :: UIChange
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, :: JSONNode, generic JSONDecode
import iTasks.Internal.Generic.Defaults
from Data.GenEq import generic gEq

/**
* Editor that does nothing.
*
* @result the empty editor
*/
emptyEditor :: Editor a | JSONEncode{|*|}, JSONDecode{|*|} a

/**
* Editor that does nothing and gives a default value in enter mode.
*
* @param default value used when editor is generated in edit mode
* @result the empty editor
*/
emptyEditorWithDefaultInEnterMode :: !a -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a

//Version without overloading, for use in generic case
//The first two argument should be JSONEncode{|*|} and JSONDecode{|*|} which cannot be used by overloading within generic functions
emptyEditorWithDefaultInEnterMode_ :: !(Bool a -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe a, ![JSONNode])) !a -> Editor a

/**
* Editor that does nothing and gives an error in enter mode.
*
* @param the error messsage used when the editor is used in enter mode
* @result the empty editor
*/
emptyEditorWithErrorInEnterMode :: !String -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a

//Version without overloading, for use in generic case
//The first two argument should be JSONEncode{|*|} and JSONDecode{|*|} which cannot be used by overloading within generic functions
emptyEditorWithErrorInEnterMode_ :: !(Bool a -> [JSONNode]) !(Bool [JSONNode] -> (!Maybe a, ![JSONNode])) !String
                                 -> Editor a

/**
 * Indicates if and how a UI child can be updated to another one.
 */
:: ChildUpdate = ChildUpdateImpossible //* the child a cannot be update
               | NoChildUpdateRequired //* no update is required, i.e. the child already equals the existing one
               | ChildUpdate !UIChange //* the child has to be changed

/**
* Determines the diff between an old and a new list of children,
* consisting of insert, remove and move change instructions.
* If possible move instructors are generated instead of a remove/insert combination.
* The worst-case time complexity is O(nm) (where n is the length of the old
* and m the length of the new children list). The complexity however decreases with
* more similar old and new lists and is O(n) for equal lists.
*
* @param: Old:                The previous child list.
* @param: New:                The new child list.
* @param  UpdateFromOldToNew: If and how an old value can be updated to a new one.
* @param: To UI:              A function to map children to UIs.
* @return                     A list of index/change pairs as expected by 'iTasks.UI.Definition.ChangeUI'.
*/
diffChildren :: ![a] ![a] !(a a -> ChildUpdate) !(a -> UI) -> [(!Int, !UIChildChange)]

/**
* Simple dropdown that edits an index by choosing from a list of labels
*/
chooseWithDropdown :: [String] -> Editor Int

/**
* Show Editor for lists
*
* @param Add:		        Determines whether new elements can be added.
*                           If this is the case a function on the current values determines the element to add,
*                           where the result of the function means:
*                               Nothing: a new value has to be entered for the new element (Enter mode)
*                               Just x:  a new element with value 'x' is added and can be updated (Update mode)
* @param Remove:            Can elements be removed?
* @param Reorder:           Can elements be reordered?
* @param Summary:           Optionally generates a summary of the list (e.g. the nr of items)
* @param Children editor:   The editor for the children
*
* @return					The list editor
*/
listEditor :: (Maybe ([Maybe a] -> Maybe a)) Bool Bool (Maybe ([Maybe a] -> String)) (Editor a) -> Editor [a]
            | JSONEncode{|*|} a

//Version without overloading, for use in generic case
//The first argument should be JSONEncode{|*|} which cannot be used by overloading within generic functions
listEditor_ :: (Bool a -> [JSONNode]) (Maybe ([Maybe a] -> Maybe a)) Bool Bool (Maybe ([Maybe a] -> String)) (Editor a)
            -> Editor [a]
