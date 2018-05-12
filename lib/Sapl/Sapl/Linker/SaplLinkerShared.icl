implementation module Sapl.Linker.SaplLinkerShared

import StdTuple, StdBool, StdInt, StdFile
import Data.Maybe, Text.StringAppender
from Data.Map import :: Map (..)
import qualified Data.Map as DM
from Data.Set import :: Set
import System.File, System.Directory, Data.Error
import Sapl.SaplTokenizer, Sapl.FastString

from StdList import foldl, removeDup, removeMember, filter, map, hd, tl, isEmpty
from StdArray import class Array(..), instance Array {#} Char
from Data.Set import :: Set, member, insert

instance toString LineType
where
	toString (LT_REDIRECT name) = name
	toString (LT_FUNC line _) = line
	toString (LT_MACRO line _) = line	

unpackName (TIdentifier name) = name
unpackName _ = ""

isGlobalFunction name = fst (charIndex name 1 '.') // first char can be skipped safely

// An identifier is a dependency if it contains a "."
generate_dependencies :: [Token] [String] -> [String]
generate_dependencies [TIdentifier name:ts] ds
	= if (isGlobalFunction name) (generate_dependencies ts [name:ds]) (generate_dependencies ts ds)
generate_dependencies [_:ts] ds = generate_dependencies ts ds
generate_dependencies [] ds = ds

read_modules :: [String] FuncTypeMap Warnings !*World -> (FuncTypeMap, Warnings, Maybe String, *World)
read_modules [m:ms] llmap messages world 
	= read_modules_ [m:ms] llmap messages Nothing 0 world
where 
	read_modules_ [m:ms] lmap messages startfn id world 
		# (res, world) = readFileLines m world
		| isOk res
			# (lmap, startfn, id) = foldl read_line (lmap, startfn, id) (fromOk res)
			= read_modules_ ms lmap messages startfn id world
			= read_modules_ ms lmap ["Warning: " +++ m +++ " not found.":messages] startfn id world
		
	read_modules_ [] lmap messages startfn id world = (lmap, messages, startfn, world)

read_module :: !String FuncTypeMap Warnings IdGenerator !*World -> (FuncTypeMap, IdGenerator, Warnings, *World)
read_module m lmap messages id world
	# (res, world) = readFileLines m world
	| isOk res
		# (lmap, startfn, id) = foldl read_line (lmap, Just "dummy", id) (fromOk res)
		= (lmap, id, messages, world)
		= (lmap, id, ["Warning: " +++ m +++ " not found.":messages], world)

/* three kind of lines:
 * :: test_B = test_C a1 | test_D
 * :: test__A = {a, b, c, d, e, f}
 * main a b = ...
 */

read_line (lmap, startfn, id) line 
	# ts = tokens line
	# next = tl ts
	= case hd ts of
			TTypeDef
				# type_name = unpackName (hd next)
				# next = tl next // skip type name	
				# next = tl next // skip "="													
				
				# lmap = case hd next of
							TOpenBracket # lmap = 'DM'.put type_name (LT_FUNC line DT_NO_DEPENDENCY) lmap
										 = parse_record (tl next) type_name lmap // constructors as redirects
										 
										 // For ADTs substitute type name with numeric id (only constructor names used,
										 // and the type name can be identical to one of its constructors name which is not good here)
							_			 # tid = "_"+++toString id
										 # lmap = 'DM'.put tid (LT_FUNC line DT_NO_DEPENDENCY) lmap
										 = parse_ADT next tid lmap
				= (lmap, startfn, id+1)
									
			(TIdentifier name)
				# lmap = case skip_to_definition next of
					[TAssignmentOp, (TIdentifier "StdMisc.undef"):_] // skip functions which are undefined
						= lmap				
					[TAssignmentOp: ts]
						= 'DM'.put name (LT_FUNC line (DT_NEED_PROCESS ts)) lmap
					[TCAFAssignmentOp: ts]
						= 'DM'.put name (LT_FUNC line (DT_NEED_PROCESS ts)) lmap						
					[TMacroAssignmentOp: ts]
						= 'DM'.put name (LT_MACRO (macroBody ts) (DT_NEED_PROCESS ts)) lmap						
						= lmap // something wrong with this line: skip it
				= (lmap, if (isNothing startfn && endsWith ".Start" name) (Just name) startfn, id+1)
									   
			_	= (lmap, startfn, id+1) // skip line. e.g. comment
where 
	skip_to_definition [TIdentifier _:ts] = skip_to_definition ts
	skip_to_definition [TStrictIdentifier _:ts] = skip_to_definition ts
	skip_to_definition [TTypeDef:ts] = skip_to_definition ts
	skip_to_definition ts = ts
	
	macroBody ts = toString (macroBody_ (filter macroTokens ts) newAppender)
	where
		macroBody_ [t] a = a <++ toString t		
		macroBody_ [t:ts] a = a <++ toString t <++ " "		
		macroBody_ [] a = a
		
	macroTokens (TComment _) = False
	macroTokens TEndOfLine = False	
	macroTokens _ = True	
	
// Get contructor names from ADT definition
parse_ADT [(TIdentifier name):ts] fn lmap
	= parse_ADT (skip_to_next_const ts) fn ('DM'.put name (LT_REDIRECT fn) lmap)
where 
	skip_to_next_const [TVerticalBar:ts] = ts
	skip_to_next_const [_:ts] = skip_to_next_const ts
	skip_to_next_const [] = []	

// This is an incorrect line: skip it
parse_ADT [_:ts] _ lmap
	= lmap

parse_ADT [] _ lmap = lmap

// Get contructor names from record definition
parse_record [TIdentifier name:ts] fn lmap
	= parse_record ts fn ('DM'.put name (LT_REDIRECT fn) lmap)

// Skip everything else (should be ",")
parse_record [_:ts] fn lmap 
	= parse_record ts fn lmap
	
parse_record [] _ lmap = lmap

generate_source :: !FuncTypeMap !SkipSet !(Loader st) !String !StringAppender !*World -> *(!FuncTypeMap, !SkipSet, !(Loader st), !StringAppender, !*World)
generate_source lmap skipset loader=:(lf,ls) fn a world
	# (line, lmap, ls, world) = if (member fn skipset) (Nothing,lmap,ls,world) (lf ls fn lmap world)
	= generate_source_ lmap skipset (lf,ls) fn line a world
where 
	generate_source_ lmap skipset loader fn (Just (LT_REDIRECT name)) a world
		# skipset = insert fn skipset // safe to delete because redirect can't link to macro

		// redirect always redirects to the same module, so it is safe to not
		// to try to load the module
		= generate_source_ lmap skipset loader name 
				(if (member name skipset) Nothing ('DM'.get name lmap)) a world

	generate_source_ lmap skipset (lf,ls) fn (Just (LT_FUNC line dt)) a world
		# skipset = insert fn skipset
		# deps = gendep fn dt
		# (lmap, (lf,ls), a, world) = substitute_macros lmap deps (lf,ls) line a world
						  
		= foldl (\(lmap, skipset, loader, a, world) t = generate_source lmap skipset loader t a world) 
															(lmap, skipset, (lf,ls), a, world) deps
	
	// don't delete macros
	// do nothing, macros are substituted
	generate_source_ lmap skipset loader fn (Just (LT_MACRO _ DT_NO_DEPENDENCY)) a world
		= (lmap, skipset, loader, a, world) 

	// process macro dependencies only once
	generate_source_ lmap skipset loader fn (Just (LT_MACRO body dt)) a world
		# deps = gendep fn dt
		// macro can't have macro dependency. by design.
		# (lmap, skipset, loader, a, world) = foldl (\(lmap, skipset, loader, a, world) t = generate_source lmap skipset loader t a world) 
										   (lmap, skipset, loader, a, world) deps
		= ('DM'.put fn (LT_MACRO body DT_NO_DEPENDENCY) lmap, skipset, loader, a, world)		
			
	// try to load the module								
	generate_source_ lmap skipset loader fn Nothing a world
		= (lmap, skipset, loader, a, world) 

	gendep fn DT_NO_DEPENDENCY = []
	// Remove cyclyc and duplicate dependencies
	gendep fn (DT_NEED_PROCESS ts) = removeMember fn (removeDup (generate_dependencies ts []))

// [String] : dependencies
load_dependencies :: !FuncTypeMap ![String] ![(String,Maybe LineType)] !(Loader st) *World -> (!FuncTypeMap, ![(String, Maybe LineType)], !(Loader st), !*World)
load_dependencies lmap [m:ms] mlines (lf,ls) world
	# (line, lmap, ls, world) = lf ls m lmap world 
	= load_dependencies lmap ms [(m,line):mlines] (lf,ls) world

load_dependencies lmap [] mlines loader world
	= (lmap, mlines, loader, world)

substitute_macros :: !FuncTypeMap ![String] !(Loader st) !String !StringAppender !*World  -> (!FuncTypeMap, !(Loader st), !StringAppender, !*World)
substitute_macros lmap deps loader line a world

		// deps: [name], depbodies: [(name, line)]
		# (lmap, depbodies, loader, world) = load_dependencies lmap deps [] loader world
		# macros = map (\(name, Just (LT_MACRO body _))=(name, body)) (filter is_macro depbodies)

		# a = case isEmpty macros of
					True = a <++ line
						 = substitute_macros_ line macros 0 0 a

		= (lmap, loader, a, world)
	where
		substitute_macros_ line macros base last a
			| base < (size line)
				# (start, newbase, t) = read_token base line
				= case t of
					(TIdentifier name) = case trythem name macros of
											Just body # a = a <++ line % (last, start-1) <++ body
													  =	substitute_macros_ line macros newbase newbase a
													  = substitute_macros_ line macros newbase last a
									   = substitute_macros_ line macros newbase last a
				= a <++ line % (last, size line)
				
		trythem what [(macroname, body): ms]
			| what == macroname 
				= Just body
				= trythem what ms

		trythem _ [] = Nothing

		is_macro (_,(Just (LT_MACRO _ _))) = True
		is_macro _ = False	
				 
					 	
