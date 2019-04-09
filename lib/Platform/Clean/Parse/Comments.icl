implementation module Clean.Parse.Comments

import StdArray
import StdBool
import StdChar
import StdClass
import StdFunctions
import StdInt
import StdList
import StdMisc
import StdString
import StdTuple

import Control.Monad
import Data.Error
import Data.Functor
from Data.Map import :: Map(..), newMap, put, get
import Data.Maybe
import System.File
import System.FilePath
from Text import class Text(startsWith), instance Text String

from Heap import :: Heap, :: HeapN, :: Ptr{pointer}, :: PtrN(Ptr), readPtr
from syntax import
	:: AttrVarInfo,
	:: AttrVarInfoPtr,
	:: AType,
	:: ATypeVar,
	:: BITVECT,
	:: CheckedTypeDef,
	:: ClassDef{class_ident,class_pos},
	:: ClassInstance,
	:: ClassInstanceR,
	:: CollectedDefinitions,
	:: ComponentNrAndIndex,
	:: ConsDef,
	:: DclInstanceMemberTypeAndFunction,
	:: Declaration,
	:: FileName,
	:: FunctionOrMacroIndex,
	:: FunctName,
	:: FunKind,
	:: FunSpecials,
	:: GenericDef{gen_ident,gen_pos},
	:: GenericCaseDef{gc_pos},
	:: Global,
	:: Ident{id_info,id_name},
	:: Import{import_file_position},
	:: ImportedObject,
	:: Index, :: LineNr,
	:: Module{mod_defs,mod_ident},
	:: ModuleN,
	:: Optional,
	:: ParsedConstructor{pc_cons_ident,pc_cons_pos},
	:: ParsedDefinition(..),
	:: ParsedExpr,
	:: ParsedImport,
	:: ParsedInstance{pi_pos},
	:: ParsedInstanceAndMembers{pim_pi},
	:: ParsedModule,
	:: ParsedSelector{ps_field_pos,ps_field_ident},
	:: ParsedTypeDef,
	:: Position(..),
	:: Priority,
	:: Rhs,
	:: RhsDefsOfType(..),
	:: SelectorDef,
	:: SortedQualifiedImports,
	:: STE_BoundTypeVariable,
	:: STE_Kind(..),
	:: SymbolPtr,
	:: SymbolTable,
	:: SymbolTableEntry{ste_kind},
	:: SymbolType,
	:: TypeDef{td_ident,td_pos,td_rhs},
	:: TypeRhs,
	:: TypeVarInfo,
	:: TypeVarInfoPtr,
	:: VarInfo,
	:: VarInfoPtr

scanComments :: !FilePath !*env -> *(!MaybeError FileError [CleanComment], !*env) | FileSystem env
scanComments fp w
# (s,w) = readFile fp w
| isError s = (Error (fromError s), w)
# s = fromOk s
# (cmnts,ss) = scan {defaultScanState & input=s}
= (Ok cmnts, w)

scanCommentsFile :: !*File -> *(!MaybeError FileError [CleanComment], !*File)
scanCommentsFile f
# (s,f) = readAll f
| isError s = (Error (fromError s), f)
# s = fromOk s
# (cmnts,ss) = scan {defaultScanState & input=s}
= (Ok cmnts, f)

:: ScanState =
	{ comment_level :: !Int
	, comment_idxs  :: ![(!Int,!Int,!Int)] // line, col, idx
	, ln            :: !Int
	, col           :: !Int
	, input         :: !String
	, idx           :: !Int
	}

defaultScanState :: ScanState
defaultScanState =
	{ comment_level = 0
	, comment_idxs  = []
	, ln            = 1
	, col           = 0
	, input         = ""
	, idx           = 0
	}

advance :: !ScanState -> ScanState
advance ss = {ss & col=ss.col+1, idx=ss.idx+1}

scan :: !ScanState -> (![CleanComment], !ScanState)
scan ss=:{idx}
| idx >= size ss.input = ([], ss)
| otherwise = case [ss.input.[i] \\ i <- [idx..]] of
	['\r':_]
		-> scan (advance ss)
	['\n':_]
		-> scan {ss & idx=idx+1, ln=ss.ln+1, col=0}
	['//':_] | ss.comment_level == 0
		# cmnt =
			{ line      = ss.ln
			, column    = ss.col
			, level     = Nothing
			, content   = ""
			, multiline = False
			}
		# ss = scan_to_newline ss
		# cmnt & content = ss.input % (idx+2,ss.idx-1)
		# (cmnts,ss) = scan ss
		-> ([cmnt:cmnts],ss)
	['/*':_]
		-> scan
			{ ss & idx=idx+2, col=ss.col+2
			, comment_level = ss.comment_level+1
			, comment_idxs  = [(ss.ln, ss.col, idx+2):ss.comment_idxs]
			}
	['*/':_] | ss.comment_level > 0
		# (c_ln,c_col,c_idx) = hd ss.comment_idxs
		# level = ss.comment_level
		# cmnt =
			{ line      = c_ln
			, column    = c_col
			, level     = Just level
			, content   = ss.input % (c_idx, idx-1)
			, multiline = True
			}
		# (cmnts,ss) = scan
			{ ss & idx=idx+2, col=ss.col+2
			, comment_level = ss.comment_level-1
			, comment_idxs  = tl ss.comment_idxs
			}
		# (before,after) = span (\c -> isJust c.level && fromJust c.level < level) cmnts
		-> (before ++ [cmnt:after],ss)
	['[':_] | ss.comment_level == 0
		-> scan (skip_list_literal (advance ss))
	['"':_] | ss.comment_level == 0
		-> scan (skip_string_literal '"' (advance ss))
	_
		-> scan (advance ss)

scan_to_newline :: !ScanState -> ScanState
scan_to_newline ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| c == '\n' = {ss & ln=ss.ln+1, col=0, idx=ss.idx+1}
| otherwise = scan_to_newline (advance ss)

skip_list_literal :: !ScanState -> ScanState
skip_list_literal ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| isSpace c = skip_list_literal (advance ss)
| c == '\'' = skip_string_literal '\'' (advance ss)
| otherwise = ss

skip_string_literal :: !Char !ScanState -> ScanState
skip_string_literal term ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| c == term = advance ss
| c == '\\' = skip_escape_sequence (advance ss)
| otherwise = skip_string_literal term (advance ss)
where
	skip_escape_sequence :: !ScanState -> ScanState
	skip_escape_sequence ss
	| ss.idx >= size ss.input = ss
	# [c1,c2,c3,c4:_] = [ss.input.[i] \\ i <- [ss.idx..]]
	= case c1 of
		'x'
			| isHexDigit c2
				| isHexDigit c3 -> iter 3 advance ss
				| otherwise -> twice advance ss
			| otherwise -> advance ss
		'0'
			| isOctDigit c2
				| isOctDigit c3
					| isOctDigit c4 -> iter 4 advance ss
					| otherwise -> iter 3 advance ss
				| otherwise -> twice advance ss
			| otherwise -> advance ss
		_ -> twice advance ss

:: CollectedComments :== Map CommentIndex CleanComment

:: CommentIndex = CI String Position String

instance < Position
where
	< a b = index a < index b
	where
		index (FunPos f l n) = (f,   l, n)
		index (LinePos f l)  = (f,   l, "")
		index (PreDefPos id) = ("", -1, id.id_name)
		index NoPos          = ("", -2, "")

instance < CommentIndex where < (CI a b c) (CI d e f) = (a,b,c) < (d,e,f)

putCC k v coll :== case commentIndex k of
	Nothing -> coll
	Just k  -> put k v coll

emptyCollectedComments :: CollectedComments
emptyCollectedComments = newMap

getComment :: !a !CollectedComments -> Maybe String | commentIndex a
getComment elem coll = (\cc -> cc.content) <$> (flip get coll =<< commentIndex elem)

collectComments :: ![CleanComment] !ParsedModule -> CollectedComments
collectComments comments pm
# coll = newMap
# (comments,coll) = case comments of
	[] -> ([], coll)
	[c:cs]
		| c.line <= 3 && startsWith "*" c.content -> (cs, putCC pm c coll)
		| otherwise -> (comments, coll)
# (_,_,coll) = collect comments Nothing pm.mod_defs coll
= coll

collect :: ![CleanComment] !(Maybe CleanComment) ![a] !CollectedComments -> (![CleanComment], !Maybe CleanComment, !CollectedComments) | pos, commentIndex, children a
collect cc prev [] coll = (cc, prev, coll)
collect [] (Just prev) [pd:pds] coll = ([], Nothing, putCC pd prev coll)
collect [] Nothing _ coll = ([], Nothing, coll)
collect [{content}:cs] prev pds coll | not (startsWith "*" content) = collect cs prev pds coll
collect allcmnts=:[c:cs] prev allpds=:[pd:pds] coll = case c canBelongTo pd of
	Nothing -> collect allcmnts prev pds coll
	Just True -> case prev of
		Just prev | prev.multiline && not c.multiline
			# coll = putCC pd prev coll
			# (allcmnts,prev,coll) = recurse allcmnts (Just c) (children pd) coll
			-> collect allcmnts prev pds coll
		_
			-> collect cs (Just c) allpds coll
	Just False
		# coll = case prev of
			Nothing -> coll
			Just cmnt -> putCC pd cmnt coll
		# (allcmnts,prev,coll) = recurse allcmnts Nothing (children pd) coll
		-> collect allcmnts prev pds coll
where
	// Compiler cannot figure out the overloading if we call collect from collect directly
	recurse :: ![CleanComment] !(Maybe CleanComment) !Children !CollectedComments -> (![CleanComment], !Maybe CleanComment, !CollectedComments)
	recurse cs prev (Children xs) coll = collect cs prev xs coll
collect _ _ _ _ = abort "internal error in Clean.Parse.Comments.collect\n"

:: Children = E.t: Children ![t] & pos, commentIndex, children t

class children a :: !a -> Children

instance children ParsedDefinition
where
	children pd = case pd of
		PD_Type ptd -> case ptd.td_rhs of
			ConsList cs -> Children cs
			ExtensibleConses cs -> Children cs
			MoreConses _ cs -> Children cs
			SelectorList _ _ _ ss -> Children ss
			_ -> Children (tl [pd]) // to fix the type
		PD_Class _ pds -> Children pds
		_ -> Children (tl [pd])

instance children ParsedSelector where children ps = Children (tl [ps])
instance children ParsedConstructor where children pc = Children (tl [pc])

(canBelongTo) infix :: !CleanComment !a -> Maybe Bool | pos a
(canBelongTo) {line,multiline} p = pos p >>= \p -> case p of
	FunPos _ ln _ -> Just (if multiline (>) (>=) ln line)
	LinePos _ ln  -> Just (if multiline (>) (>=) ln line)
	_             -> Nothing

class pos a :: !a -> Maybe Position

instance pos ParsedDefinition
where
	pos pd = case pd of
		PD_Function pos _ _ _ _ _ -> Just pos
		PD_NodeDef pos _ _ -> Just pos
		PD_Type ptd -> Just ptd.td_pos
		PD_TypeSpec pos _ _ _ _ -> Just pos
		PD_Class cd _ -> Just cd.class_pos
		PD_Instance piam -> Just piam.pim_pi.pi_pos
		PD_Instances [piam:_] -> Just piam.pim_pi.pi_pos
		PD_Instances [] -> Nothing
		PD_Import [pi:_] -> Just pi.import_file_position
		PD_Import [] -> Nothing
		PD_ImportedObjects _ -> Nothing
		PD_ForeignExport _ _ _ _ -> Nothing
		PD_Generic gd -> Just gd.gen_pos
		PD_GenericCase gcd _ -> Just gcd.gc_pos
		PD_Derive [gcd:_] -> Just gcd.gc_pos
		PD_Derive [] -> Nothing
		PD_Erroneous -> Nothing

instance pos ParsedSelector where pos ps = Just ps.ps_field_pos
instance pos ParsedConstructor where pos pc = Just pc.pc_cons_pos

class commentIndex a :: !a -> Maybe CommentIndex

instance commentIndex (Module a)
where
	commentIndex {mod_ident} = Just (CI "Module" NoPos mod_ident.id_name)

instance commentIndex ParsedDefinition
where
	commentIndex pd = case pd of
		PD_Function pos id is_infix args rhs kind -> Just (CI "PD_Function" pos id.id_name)
		PD_TypeSpec pos id prio type specials -> Just (CI "PD_TypeSpec" pos id.id_name)
		PD_Class cd pds -> Just (CI "PD_Class" cd.class_pos cd.class_ident.id_name)
		PD_Type ptd -> Just (CI "PD_Type" ptd.td_pos ptd.td_ident.id_name)
		PD_Generic gd -> Just (CI "PD_Generic" gd.gen_pos gd.gen_ident.id_name)
		_ -> Nothing

instance commentIndex ParsedSelector
where commentIndex ps = Just (CI "ParsedSelector" ps.ps_field_pos ps.ps_field_ident.id_name)
instance commentIndex ParsedConstructor
where commentIndex pc = Just (CI "ParsedConstructor" pc.pc_cons_pos pc.pc_cons_ident.id_name)
