definition module iTasks.Extensions.Distributed.Authentication

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from iTasks.Internal.Distributed.Domain import :: Domain
from iTasks.Extensions.User import :: User, :: Username, ::Password

import iTasks.SDS.Definition

remoteAuthenticateUser	:: !Username !Password	-> Task (Maybe User)

domainAuthServer :: Task ()

/*
 * Get users from domain.
 */
usersOf :: Domain -> Task [User]

startAuthEngine :: Domain -> Task ()

enterDomain :: Task Domain

currentDistributedUser :: SimpleSDSParallel (User,Domain)

currentDomain :: SDSLens () Domain ()
