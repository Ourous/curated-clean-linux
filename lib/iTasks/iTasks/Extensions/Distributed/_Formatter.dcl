definition module iTasks.Extensions.Distributed._Formatter

import StdMaybe
import symbols_in_program

deserializeFromBase64 :: String !{#Symbol} -> a
serializeToBase64 :: a -> String
