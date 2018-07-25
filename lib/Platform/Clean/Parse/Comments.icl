implementation module Clean.Parse.Comments

import StdArray
import StdBool
import StdChar
import StdClass
import StdFunc
import StdInt
import StdList
import StdString
import StdTuple

import Control.Monad
import Data.Error
import Data.Functor
from Data.Map import :: Map
import qualified Data.Map as M
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
	:: CollectedDefinitions,
	:: ComponentNrAndIndex,
	:: ConsDef,
	:: Declaration,
	:: FileName,
	:: FunctionOrMacroIndex,
	:: FunctName,
	:: FunKind,
	:: FunSpecials,
	:: GenericDef{gen_pos},
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
	:: ParsedSelector{ps_field_pos,ps_selector_ident},
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
# [c1,c2:_] = [ss.input.[i] \\ i <- [idx..]]
| c1 == '\r'
	= scan (advance ss)
| c1 == '\n'
	= scan {ss & idx=idx+1, ln=ss.ln+1, col=0}
| c1 == '/' && c2 == '/' && ss.comment_level == 0
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
	= ([cmnt:cmnts],ss)
| c1 == '/' && c2 == '*'
	= scan
		{ ss & idx=idx+2, col=ss.col+2
		, comment_level = ss.comment_level+1
		, comment_idxs  = [(ss.ln, ss.col, idx+2):ss.comment_idxs]
		}
| c1 == '*' && c2 == '/' && ss.comment_level > 0
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
	= (before ++ [cmnt:after],ss)
| c1 == '['
	= scan (skip_list_literal (advance ss))
| c1 == '"'
	= scan (skip_string_literal '"' (advance ss))
| otherwise
	= scan (advance ss)

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
| c == term = ss
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

:: CollectedComments :== Map (!Int, !String) CleanComment

getCC :== 'M'.get o toCCIndex
putCC k v coll :== case index k of
	Nothing -> coll
	Just k  -> 'M'.put (toCCIndex k) v coll

toCCIndex :: !(!STE_Kind,!String) -> (!Int, !String)
toCCIndex (stek,name) = (toint stek, name)
where
	toint :: !STE_Kind -> Int
	toint kind = case kind of
		STE_FunctionOrMacro _ -> 0
		STE_DclMacroOrLocalMacroFunction _ -> 1
		STE_Type -> 2
		STE_Constructor -> 3
		STE_Selector _ -> 4
		STE_Field _ -> 5
		STE_Class -> 6
		STE_Member -> 7
		STE_Generic _ -> 8
		STE_GenericCase -> 9
		STE_GenericDeriveClass -> 10
		STE_Instance -> 11
		STE_Variable _ -> 12
		STE_TypeVariable _ -> 13
		STE_FunDepTypeVariable _ -> 14
		STE_TypeAttribute _ -> 15
		STE_BoundTypeVariable _ -> 16
		STE_Imported _ _ -> 17
		STE_DclFunction -> 18
		STE_Module _ -> 19
		STE_ClosedModule -> 20
		STE_ModuleQualifiedImports _ -> 21
		STE_Empty -> 22
		STE_DictType _ -> 23
		STE_DictCons _ -> 24
		STE_DictField _ -> 25
		STE_Called _ -> 26
		STE_ExplImpSymbol _ -> 27
		STE_ExplImpComponentNrs _ -> 28
		STE_BelongingSymbol _ -> 29
		STE_ExplImpSymbolNotImported _ _ -> 30
		STE_ImportedQualified _ _ -> 31
		STE_Hidden _ _ -> 32
		STE_UsedType _ _ -> 33
		STE_UsedQualifiedType _ _ _ -> 34
		STE_BelongingSymbolExported -> 35
		STE_BelongingSymbolForExportedSymbol -> 36
		STE_TypeExtension -> 37

emptyCollectedComments :: CollectedComments
emptyCollectedComments = 'M'.newMap

getComment :: !Ident !CollectedComments -> Maybe String
getComment {id_name,id_info={pointer=Ptr {ste_kind} _}} coll =
	(\cc -> cc.content) <$> getCC (ste_kind,id_name) coll

collectComments :: ![CleanComment] !ParsedModule -> CollectedComments
collectComments comments pm
# coll = 'M'.newMap
# (comments,coll) = case comments of
	[] -> ([], coll)
	[c:cs]
		| c.line <= 3 -> (cs, putCC pm.mod_ident c coll)
		| otherwise   -> (comments, coll)
# (_,_,coll) = collect comments Nothing pm.mod_defs coll
= coll

collect :: ![CleanComment] !(Maybe CleanComment) ![a] !CollectedComments -> (![CleanComment], !Maybe CleanComment, !CollectedComments) | pos, index, children a
collect cc prev [] coll = (cc, prev, coll)
collect [] (Just prev) [pd:pds] coll = ([], Nothing, putCC pd prev coll)
collect [] Nothing _ coll = ([], Nothing, coll)
collect [{column,multiline=True}:cs] prev pds coll | column > 0 = collect cs prev pds coll
collect [{content}:cs] prev pds coll | not (startsWith "*" content) = collect cs prev pds coll
collect allcmnts=:[c:cs] prev allpds=:[pd:pds] coll = case c canBelongTo pd of
	Nothing -> collect allcmnts prev pds coll
	Just True -> collect cs (Just c) allpds coll
	Just False -> case prev of
		Nothing -> collect allcmnts Nothing pds coll
		Just cmnt
			# coll = putCC pd cmnt coll
			# (allcmnts,prev,coll) = recurse allcmnts Nothing (children pd) coll
			-> collect allcmnts prev pds coll
where
	// Compiler cannot figure out the overloading if we call collect from collect directly
	recurse :: ![CleanComment] !(Maybe CleanComment) !Children !CollectedComments -> (![CleanComment], !Maybe CleanComment, !CollectedComments)
	recurse cs prev (Children xs) coll = collect cs prev xs coll

:: Children = E.t: Children ![t] & pos, index, children t

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
		PD_Import [pi:_] -> Just pi.import_file_position
		PD_ImportedObjects _ -> Nothing
		PD_ForeignExport _ _ _ _ -> Nothing
		PD_Generic gd -> Just gd.gen_pos
		PD_GenericCase gcd _ -> Just gcd.gc_pos
		PD_Derive [gcd:_] -> Just gcd.gc_pos
		PD_Erroneous -> Nothing

instance pos ParsedSelector where pos ps = Just ps.ps_field_pos
instance pos ParsedConstructor where pos pc = Just pc.pc_cons_pos

class index a :: !a -> Maybe (!STE_Kind,!String)

instance index Ident
where
	index {id_name,id_info={pointer=Ptr {ste_kind} _}} = Just (ste_kind,id_name)

instance index ParsedDefinition
where
	index pd = case pd of
		PD_Function pos id is_infix args rhs kind -> index id
		PD_TypeSpec pos id prio type specials -> index id
		PD_Class cd pds -> index cd.class_ident
		PD_Type ptd -> index ptd.td_ident
		_ -> Nothing

instance index ParsedSelector where index ps = index ps.ps_selector_ident
instance index ParsedConstructor where index pc = index pc.pc_cons_ident
