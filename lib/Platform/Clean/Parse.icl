implementation module Clean.Parse

// NOTE: be VERY restrictive with adding imports here, because this may break
// the module when the compiler changes.

import StdFile

import Data.Error
import Data.Maybe
import System.File
import System.FilePath
from Text import class Text(endsWith), instance Text String

from hashtable import :: BoxedIdent{boxed_ident}, :: HashTable,
	:: IdentClass(IC_Module), :: QualifiedIdents(NoQualifiedIdents),
	putIdentInHashTable, set_hte_mark, newHashTable
from parse import wantModule
import syntax

readModule :: !FilePath !*World -> *(!MaybeError String (ParsedModule, HashTable), !*World)
readModule filename w
# (s,w) = readFile filename w
| isError s = (Error (toString (fromError s)), w)
# modname = getModuleName (fromString (fromOk s))
# modname = fromMaybe (takeFileName (dropExtension filename)) modname
# ht = newHashTable newHeap
# ht = set_hte_mark (if icl 1 0) ht
# (ok,f,w) = fopen filename FReadText w
| not ok = (Error ("Couldn't open " +++ filename), w)
# (mod_id, ht) = putIdentInHashTable modname (IC_Module NoQualifiedIdents) ht
# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" icl mod_id.boxed_ident NoPos True ht stderr) w
# (ok,w) = fclose f w
| not ok = (Error ("Couldn't close " +++ filename), w)
= (Ok (pm, ht), w)
where
	icl = endsWith "icl" filename

	wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
		-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
	wantModule` f s b1 i p b2 ht io fs
	# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
	= ((b1,b2,pm,ht,f),fs)

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
