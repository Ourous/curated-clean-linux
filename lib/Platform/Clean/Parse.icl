implementation module Clean.Parse

// NOTE: be VERY restrictive with adding imports here, because this may break
// the module when the compiler changes.

import Clean.Parse.ModuleName
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
# (modname,w) = guessModuleName filename w
| isError modname = (Error (toString (fromError modname)), w)
# modname = fromMaybe (takeFileName (dropExtension filename)) (fromOk modname)
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
