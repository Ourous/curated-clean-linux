implementation module iTasks.Internal.Distributed.Symbols

import iTasks

import StdFile
import StdArray
import symbols_in_program
import dynamic_string
import Text.Encodings.Base64

import iTasks.Internal.SDS
import iTasks.Internal.AsyncSDS
import iTasks.Internal.Task
import iTasks.Internal.IWorld

symbolsShare :: SimpleSDSLens String
symbolsShare = sharedStore "symbols" ""

storeSymbols :: String !*IWorld -> (MaybeError TaskException Int, !*IWorld)
storeSymbols file iworld
# (symbols, iworld) = accFiles (read_symbols file) iworld
# val = base64Encode (copy_to_string symbols)
# (res, iworld) = write val symbolsShare EmptyContext iworld
| isError res = (liftError res, iworld)
= (Ok (size symbols), iworld)

accSymbols :: ({#Symbol} -> a) -> Task a | iTask a
accSymbols fun = withSymbols (return o fun)

readSymbols :: String -> {#Symbol}
readSymbols shareValue = fst (copy_from_string (base64Decode shareValue))

withSymbols :: ({#Symbol} -> Task a) -> Task a | iTask a
withSymbols taskfun = Task (readCompletely symbolsShare NoValue (unTask o taskfun o readSymbols))
