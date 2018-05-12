definition module iTasks.Internal.SDSService

from Internet.HTTP					import :: HTTPRequest, :: HTTPResponse
from iTasks.Internal.IWorld		import :: IWorld
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService
from iTasks.Internal.SDS			import :: RWShared
from iTasks.Internal.Task			import :: Task, :: InstanceNo
from iTasks.Internal.TaskState	import :: TIUIState
from iTasks.UI.Definition           import :: UIChange
from Data.Queue						import :: Queue
from Data.Maybe                     import :: Maybe
from Data.Error                     import :: MaybeError, :: MaybeErrorString
from Data.Map                       import :: Map
from Text.GenJSON                      import :: JSONNode

from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq

from iTasks.SDS.Definition import :: SDS

sdsService :: WebService a a

readRemoteSDS  ::           !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString (),     !*IWorld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w
