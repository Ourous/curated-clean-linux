implementation module iTasks.Extensions.Process

import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.IO
import iTasks.WF.Tasks.Interaction
import iTasks.SDS.Sources.Core
import iTasks.UI.Editor.Modifiers

import StdString, StdList
import Data.Maybe, Data.Error
import qualified System.Process

derive class iTask ProcessInformation, ProcessStatus, CallException

instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err

callProcess :: !d ![ViewOption ProcessInformation] !FilePath ![String] !(Maybe FilePath) (Maybe ProcessPtyOptions) -> Task ProcessInformation | toPrompt d
callProcess prompt [ViewAs tof:_] executable arguments workingDirectory pty
	= externalProcess prompt executable arguments workingDirectory unitShare (callProcessHandlers executable arguments) pty (comapEditorValue tof gEditor{|*|})
callProcess prompt [ViewUsing tof editor:_] executable arguments workingDirectory pty
	= externalProcess prompt executable arguments workingDirectory unitShare (callProcessHandlers executable arguments) pty (comapEditorValue tof editor)
callProcess prompt _ executable arguments workingDirectory pty
	= externalProcess prompt executable arguments workingDirectory unitShare (callProcessHandlers executable arguments) pty gEditor{|*|}

callProcessHandlers executable arguments
	= {onStartup = onStartup, onOutData = onOutData, onErrData = onErrData, onShareChange = onShareChange, onExit = onExit}
where
    onStartup _ = (Ok {ProcessInformation|executable=executable,arguments=arguments,stdout="",stderr="",status=RunningProcess}, Nothing, [], False)
    onOutData data info=:{ProcessInformation|stdout} _ = (Ok {ProcessInformation|info & stdout = stdout +++ data}, Nothing, [], False)
    onErrData data info=:{ProcessInformation|stderr} _ = (Ok {ProcessInformation|info & stderr = stderr +++ data}, Nothing, [], False)
    onShareChange info _ = (Ok info, Nothing, [], False)
    onExit (ExitCode exitCode) info _ = (Ok {ProcessInformation|info & status = CompletedProcess exitCode}, Nothing) 

callInstantProcess :: !FilePath ![String] !(Maybe FilePath) -> Task Int
callInstantProcess cmd args dir = accWorldError (\world -> 'System.Process'.callProcess cmd args dir world) CallFailed 
