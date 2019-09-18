definition module iTasks.Extensions.Distributed.iTasks

import StdBool
import StdString
import StdInt
import StdTuple
import StdFile
import StdOrdList
from StdFunc import const, o

import System.OS

from iTasks.WF.Definition import class iTask
from iTasks.Internal.Task import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint, instance toString UserConstraint, instance toUserConstraint User, instance toString UserConstraint, instance toString User
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat(..)
import qualified iTasks.Extensions.User as U
from iTasks.WF.Combinators.Common import -&&-, >>-
from iTasks.SDS.Sources.System import currentDateTime
from iTasks.Extensions.User import currentUser, :: User(..), :: UserTitle, :: Role, :: UserId, assign, workerAttributes, :: Password, :: Username, workAs, :: Credentials{..}, users
from iTasks.UI.Definition import :: Title(..), :: Hint(..)
from iTasks.UI.Tune import @>>, <<@, class tune, instance tune Hint Task, instance tune Title Task
from iTasks.SDS.Definition import class RWShared(..)
from iTasks.WF.Tasks.Core import accWorld
import iTasks.Internal.Distributed.Symbols
from iTasks.Internal.Distributed.Instance import instanceServer, instanceClient, instanceFilter, instanceClameFilter
from Data.Map import :: Map
from iTasks.Extensions.Admin.WorkflowAdmin import installWorkflows, loginAndManageWork,manageWorkOfCurrentUser, workflow, class toWorkflow(..), :: Workflow, publish, :: TaskWrapper(..), instance toWorkflow (Task a), instance toWorkflow (WorkflowContainer a), instance toWorkflow (a -> Task b), instance toWorkflow (ParamWorkflowContainer a b), :: WorkflowContainer, :: ParamWorkflowContainer
from System.FilePath import :: FilePath, </>
from iTasks.Engine import doTasksWithOptions, doTasks, :: StartableTask, onRequestFromRequest, startEngineWithOptions, :: EngineOptions(..), startEngine, class Startable, instance Startable [StartableTask], :: WebTaskWrapper
from Internet.HTTP import :: HTTPRequest(..), :: HTTPUpload, :: HTTPProtocol, :: HTTPMethod
import iTasks.WF.Combinators.Common
from iTasks.WF.Combinators.Common import :: TaskCont
from iTasks.WF.Tasks.Interaction import enterInformation, :: EnterOption, :: ViewOption, enterChoice, :: ChoiceOption, viewInformation, enterChoiceWithShared, updateInformationWithShared, updateSharedInformation, :: UpdateOption, :: UpdateSharedOption
from iTasks.Extensions.DateTime import :: DateTime, :: Time, waitForTimer
from iTasks.Extensions.Admin.UserAdmin import manageUsers
from iTasks.SDS.Sources.System import currentTime
from iTasks.SDS.Sources.Store import sharedStore
from iTasks.WF.Combinators.SDS import withShared
from iTasks.Internal.Distributed.Domain import :: Domain
import iTasks.Extensions.Distributed.Task
from iTasks.Extensions.Distributed.Authentication import domainAuthServer, usersOf, remoteAuthenticateUser, startAuthEngine, enterDomain, currentDistributedUser, currentDomain
import Text
import iTasks.Extensions.Distributed.InteractionTasks
from StdList import ++
import iTasks.WF.Combinators.Overloaded

from Internet.HTTP import :: HTTPResponse{..}, :: HTTPMethod(..)
from Text.URI import :: URI{..}, parseURI
from iTasks.Extensions.Web import callHTTP

from iTasks.Extensions.Device.Features import hasCamera, device, :: DeviceFeatures, manageDeviceFeaturs
from iTasks.Extensions.Picture.JPEG import :: JPEGPicture(..)
from iTasks.Extensions.Device.Camera import takePicture
from iTasks.Extensions.Device.Location import :: Coordinates(..), getLocation

from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode, jsonQuery, :: JSONNode(JSONNull), instance toString JSONNode, instance fromString JSONNode

from StdList import isEmpty
from StdOverloaded import class toReal
import StdReal
from Data.Error import :: MaybeError(..)

import iTasks.WF.Tasks.SDS
