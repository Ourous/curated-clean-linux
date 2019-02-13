definition module iTasks.Extensions.Distributed.Task

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from iTasks.Internal.Distributed.Domain import :: Domain

:: Requires = Requires String

class (@:) infix 3 u a :: u a -> a

instance @: worker (Task a) | iTask a & toUserConstraint worker

instance @: Domain (Task a) | iTask a

instance @: DomainUser (Task a) | iTask a

instance @: Requires (Task a) | iTask a

:: DomainUser = E. a: DomainUser a Domain & toUserConstraint a & gText{|*|} a & toString a

derive gText DomainUser	

instance toString DomainUser

class (@.) infix 4 u a :: u a -> DomainUser

instance @. worker Domain | toUserConstraint worker & gText{|*|} worker & toString worker
