implementation module System.TTS

import StdTuple, StdOverloaded
import Data.Maybe
import System.Process

tts :: !String !*World -> *World
tts str world = say [str] world

ttsWithVoice :: !Voice !String *World -> *World
ttsWithVoice voice str world = say ["-t", toString voice, str] world

say :: ![String] !*World -> *World
say args world = snd (runProcess "/usr/bin/spd-say" args Nothing world)

instance toString Voice where
  toString Male1       = "male1"
  toString Male2       = "male2"
  toString Male3       = "male3"
  toString Female1     = "female1"
  toString Female2     = "female2"
  toString Female3     = "female3"
  toString ChildMale   = "child_male"
  toString ChildFemale = "child_female"
