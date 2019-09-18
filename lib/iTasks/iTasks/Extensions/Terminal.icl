implementation module iTasks.Extensions.Terminal

import System.Process
import System.Time
import Text
from Data.Func import $
import iTasks
import Text.Terminal.VT100

import StdArray
runProcessInteractive :: !VT100Settings !FilePath ![String] !(Maybe FilePath) -> Task Int
runProcessInteractive vt100 fp args wd =
	withShared [] \stdin->
	withShared ([], []) \stdouterr->
		    (externalProcess {tv_sec=0,tv_nsec=100000000} fp args wd (Just defaultPtyOptions) stdin stdouterr <<@ NoUserInterface)
		-|| (Hint "Output" @>> viewSharedInformation [ViewAs $ vt100render vt100 o concat o fst] stdouterr)
		-|| forever (Hint "Data to send to stdin" @>> enterInformation [] >>= \l->upd (\ls->ls ++ [l +++ "\n"]) stdin)
