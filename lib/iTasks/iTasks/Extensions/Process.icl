implementation module iTasks.Extensions.Process

import iTasks
import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.IO
import iTasks.WF.Tasks.Interaction
import iTasks.SDS.Sources.Core
import iTasks.UI.Editor.Modifiers

import StdString, StdList
import Data.Maybe, Data.Error
import System.Time
import Text
import qualified System.Process

derive class iTask ProcessInformation, ProcessStatus, CallException

instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err

callProcess :: !d ![ViewOption ProcessInformation] !FilePath ![String] !(Maybe FilePath) (Maybe ProcessPtyOptions) -> Task ProcessInformation | toPrompt d
callProcess prompt vopts fp args wd pty
	= withShared [] \stdin->withShared ([], []) \out->
		let s = (mapRead (\(stdout,stderr)->
				{ executable=fp
				, arguments=args
				, stdout=concat stdout
				, stderr=concat stderr
				, status=RunningProcess}) out) in
		externalProcess {tv_sec=0,tv_nsec=100000000} fp args wd pty stdin out
		-|| viewSharedInformation prompt vopts s
		>>- \c->get s @ \s->{s & status=CompletedProcess c}

callInstantProcess :: !FilePath ![String] !(Maybe FilePath) -> Task Int
callInstantProcess cmd args dir = accWorldError (\world -> 'System.Process'.callProcess cmd args dir world) CallFailed
