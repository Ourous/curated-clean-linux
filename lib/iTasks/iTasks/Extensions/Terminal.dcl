definition module iTasks.Extensions.Terminal

from Data.Maybe import :: Maybe
from iTasks.WF.Definition import :: Task
from System.FilePath import :: FilePath
from Text.Terminal.VT100 import :: VT100Settings

runProcessInteractive :: !VT100Settings !FilePath ![String] !(Maybe FilePath) -> Task Int
