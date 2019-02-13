implementation module iTasks.Internal.Client.JSStore

import StdString, StdMisc
import Data.Maybe
from iTasks.Internal.IWorld import :: IWorld

jsStoreValue :: !String !String !a !*IWorld -> *IWorld
jsStoreValue namespace key value iworld = abort "jsDeleteValues"	

jsLoadValue :: !String !String !*IWorld -> (!Maybe a,!*IWorld)
jsLoadValue namespace key iworld = abort "jsDeleteValues"

jsDeleteValue :: !String !String !*IWorld -> *IWorld
jsDeleteValue namespace key iworld  = abort "jsDeleteValues"

jsDeleteValues :: !String !String !*IWorld -> *IWorld
jsDeleteValues namespace keyprefix iworld = abort "jsDeleteValues"



