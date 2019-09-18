definition module System.TTS

from StdOverloaded import class toString

:: Voice
  = Male1
  | Male2
  | Male3
  | Female1
  | Female2
  | Female3
  | ChildMale
  | ChildFemale

instance toString Voice

tts :: !String !*World -> *World

ttsWithVoice :: !Voice !String *World -> *World
