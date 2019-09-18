definition module iTasks.UI.Editor.Modifiers
/**
* This module provides combinator functions for combining editors
*/
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.Tune
import Data.Error

//### Modifying atributes of editors ### 
/**
* Adds an attribute that tags the UI with the edit mode of the editor: view, enter, update
* This does not change the mode of the editor
*/
withEditModeAttr :: !(Editor a) -> Editor a

/**
* Adds a hint and hint-type attribute based on the editor's state.
* Layouts (such as the automatic default layout) can use these attributes to create hint icons and tooltips
*/
withDynamicHintAttributes :: !String !(Editor a) -> Editor a

//### Changing the edit mode ### 
/*
* Change the edit mode (view/enter/update)
*/
withChangedEditMode :: !((EditMode a) -> EditMode a) !(Editor a) -> Editor a

/**
* Selecting between editors based on the edit mode of the interaction (view/enter/update)
* 
* @param Editor for viewing a value
* @param Editor for entering a value
* @param Editor for updating a value
*/
selectByMode :: !(Editor a) !(Editor a) !(Editor a) -> Editor a

/**
 * Uses the given editor to view a constant value.
 *
 * @param The constant value to view.
 * @param The editor used to view the value.
 * @result An editor viewing the constant value.
 */
viewConstantValue :: !a !(Editor a) -> Editor ()

// ### Changing the model value of the editor ###


/**
* Map the value of an editor to another (isomorphic) domain
*/
bijectEditorValue :: !(a -> b) !(b -> a) !(Editor b) -> Editor a

/**
* Map the value of an editor to another domain which is 'bigger' than the original domain
* so conversion back to the original is not always possible
*/
injectEditorValue :: !(a -> b) !(b -> MaybeErrorString a) !(Editor b) -> Editor a

/**
* Map the value of an editor to another domain which is 'smaller' than the original domain
*/
surjectEditorValue :: !(a (Maybe b) -> b) !(b (Maybe a) -> a) !(Editor b) -> Editor a | JSONEncode{|*|}, JSONDecode{|*|} a

/**
* Map the value of an editor to another domain, without mapping changes in the editor back
*/
comapEditorValue :: !(b -> a) !(Editor a) -> Editor b | JSONEncode{|*|}, JSONDecode{|*|} b
