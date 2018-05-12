definition module iTasks.Internal.RemoteAccess

import StdString
from iTasks.Internal.IWorld import :: IWorld
import Data.Maybe, Internet.HTTP, Text.URI

httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
