definition module iTasks.Internal.Client.LinkerSupport

import StdString
import Data.Maybe
import iTasks.Internal.IWorld
import iTasks.UI.Editor

/**
* Links all necessary Sapl functions for an editlet and compiles them to Javascript 
*
* @param initUI function
* @param IWorld state
*
* @return JS code of the support code for all the expressions
* @return JS code of the initUI function
* @return IWorld state
*/
editorLinker :: !f !*IWorld -> *(!MaybeErrorString (!String,!String),!*IWorld)
