implementation module System.Signal

import StdEnv
import Data.Error
import System.OSError
import System._Signal

import code from "systemsignal.o"

:: SigHandler :== Int

signalInstall :: !Int !*env -> *(MaybeOSError *SigHandler, !*env)
signalInstall signum w
	# (ok, h, w) = signalInstall_ signum w
	| ok <> 0 = getLastOSError w
	= (Ok h, w)
where
	signalInstall_ :: !Int !*env -> *(!Int, !*SigHandler, !*env)
	signalInstall_ _ _ = code {
		ccall signal_install "I:VII:A"
	}

signalPoll :: !*SigHandler !*env -> *(!MaybeErrorString Bool, !*SigHandler, !*env)
signalPoll h w
	# (ok, s, h, w) = signalPoll_ h w
	| ok <> 0 = (Error "Invalid handler", h, w)
	= (Ok s, h, w)
where
	signalPoll_ :: !*SigHandler !*env -> *(!Int, !Bool, !*SigHandler, !*env)
	signalPoll_ _ _= code {
		ccall signal_poll "I:VIII:A"
	}
