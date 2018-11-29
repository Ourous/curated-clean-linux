implementation module Clean.Parse.ModuleName

import StdBool
import StdChar
import StdClass
import StdFile
import StdList

import Data.Error
import Data.Maybe
import System.File
import System.FilePath

guessModuleName :: !FilePath !*World -> *(!MaybeError FileError (Maybe String), !*World)
guessModuleName filename w
# (s,w) = readFile filename w
| isError s = (Error (fromError s), w)
# modname = getModuleName (fromString (fromOk s))
= (Ok modname, w)

// A reasonably accurate simple scanner to get the module name from the file

getModuleName :: ![Char] -> Maybe String
getModuleName ['definition':c:cs]     | isSpace c = justModule cs
getModuleName ['implementation':c:cs] | isSpace c = justModule cs
getModuleName ['system':c:cs]         | isSpace c = justModule cs
getModuleName [c:cs]                  | isSpace c = getModuleName cs
getModuleName ['//':cs]                           = getModuleName (dropWhile ((<>) '\n') cs)
getModuleName ['/*':cs]                           = getModuleName (skipMultiLineComment cs)
getModuleName cs                                  = justModule cs

justModule :: ![Char] -> Maybe String
justModule ['module':c:cs] | isSpace c = justModuleName cs
justModule [c:cs]          | isSpace c = justModule cs
justModule ['//':cs]                   = justModule (dropWhile ((<>) '\n') cs)
justModule ['/*':cs]                   = justModule (skipMultiLineComment cs)
justModule _                           = Nothing

justModuleName :: ![Char] -> Maybe String
justModuleName cs
# (_,cs) = span isSpace cs
# (name,_) = span (\c -> c <> '/' && c <> ';' && not (isSpace c)) cs
= case name of
	[] -> Nothing
	_  -> Just (toString name)

skipMultiLineComment :: ![Char] -> [Char]
skipMultiLineComment ['*/':cs] = cs
skipMultiLineComment ['/*':cs] = skipMultiLineComment (skipMultiLineComment cs)
skipMultiLineComment [c:cs] = skipMultiLineComment cs
skipMultiLineComment [] = []
