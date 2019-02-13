implementation module iTasks.Extensions.Distributed.Task

import StdString
import StdInt

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes
from Data.Maybe import :: Maybe, maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint, instance toString UserConstraint, instance toUserConstraint User, instance toString UserConstraint
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat(..)
import qualified iTasks.Extensions.User as U
import Data.Map
from symbols_in_program import :: Symbol
from iTasks.Internal.Distributed.RemoteTask import remoteAssignTask
import iTasks.Internal.Distributed.Domain
from iTasks.WF.Combinators.Common import -&&-, >>-
from iTasks.SDS.Sources.System import currentDateTime
from iTasks.Extensions.DateTime import :: DateTime, instance toString DateTime
from iTasks.Extensions.User import currentUser, :: User(..), :: UserTitle, :: Role, :: UserId, assign, workerAttributes
from iTasks.Extensions.Distributed.Authentication import currentDomain
import qualified iTasks.WF.Tasks.SDS as C
import iTasks.Internal.Distributed.Symbols
import iTasks.Internal.SDS

instance @: worker (Task a) | iTask a & toUserConstraint worker
where
	(@:) worker task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> assign (workerAttributes worker
			[ ("title",      toTitle worker)
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("createdFor", toString (toUserConstraint worker))
			]) task

instance @: Domain (Task a) | iTask a
where
	(@:) domain task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> remoteAssignTask (fromList
			[ ("title",      "None")
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			]) task domain

instance @: DomainUser (Task a) | iTask a
where
	(@:) (DomainUser worker domain) task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> remoteAssignTask (fromList
			[ ("title",      toTitle worker)
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("createdFor", toString (toUserConstraint worker))
			]) task domain

instance @: Requires (Task a) | iTask a
where
	(@:) (Requires requires) task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> 'C'.get currentDomain
		>>- \domain -> remoteAssignTask (fromList
			[ ("title",      "None")
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("requires",   requires)
			]) task domain


instance @. worker Domain | toUserConstraint worker & gText{|*|} worker & toString worker
where
	(@.) worker domain
		= (DomainUser worker domain)

gText{|DomainUser|} format user = maybe [""] (\(DomainUser worker _) -> gText{|*|} format (Just worker)) user

instance toString DomainUser
where
	toString (DomainUser worker _) = toString worker

