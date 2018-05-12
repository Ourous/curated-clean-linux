implementation module Sapl.SaplParser

import StdEnv, Data.Error
import Sapl.SaplTokenizer, Sapl.SaplStruct, Sapl.FastString
import Sapl.Transform.VarReferences

from Data.Map import :: Map
import qualified Data.Map as DM

(>>=) infixl 1
(>>=) f g = \st0 ->
		case f st0 of
			Ok (r, st1) = g r st1
			Error str		 = Error str

(>>|) infixl 1
(>>|) f g = f >>= \_ -> g

returnS r :== \s -> Ok (r,s)
returnE e :== \s -> Error e

mandatory errmsg (Just t, ts)
		= returnS (t, ts)
mandatory errmsg (Nothing, ts)
		= returnE (ts, errmsg)

incLevel a :== \s -> Ok (a, {s & ps_level = s.ps_level + 1})
decLevel a :== \s -> Ok (a, {s & ps_level = s.ps_level - 1})
getLevel :== \s -> Ok (s.ps_level, s)
addFunction name args :== \s -> Ok (name, {s & ps_functions = 'DM'.put (unpackVar name) args s.ps_functions})
addCAF name :== \s -> Ok (name, {s & ps_CAFs = 'DM'.put (unpackVar name) () s.ps_CAFs})
defaultState = {ps_level = 0, ps_constructors = 'DM'.newMap, ps_functions = 'DM'.newMap, ps_CAFs = 'DM'.newMap, ps_genFuns = []}

addConstructor name def :== \s -> Ok (name, {s & ps_constructors = 'DM'.put (unpackVar name) def s.ps_constructors})
checkConstructor name :== \s -> Ok (isJust ('DM'.get name s.ps_constructors), s)
addGenFun fun :== \s -> Ok (fun, {s & ps_genFuns = [fun:s.ps_genFuns]})

addConstructors conses = \s -> Ok (conses, {s & ps_constructors = foldl adddef s.ps_constructors conses})
where
	nr_cons = length conses
	adddef m (SaplConstructor name idx as) 
		= 'DM'.put (unpackVar name) {index = idx, nr_cons = nr_cons, nr_args = length as, args = as} m

// Add Tuple constructor if necessary
addTupleCons name | startsWith "_Tuple" name && size name > 6 =
		checkConstructor name
	>>= \b = if b (returnS ()) (addConstructor (NormalVar name 0) newdef >>| addGenFun newadt >>| returnS ())
where
	(newadt, newdef) = gendefs name

	gendefs name
		# idxpart = name % (6, size name)
		# (l,r) = case charIndex idxpart 1 '!' of
			(True, idx) = (toInt (idxpart % (0,idx-1)), toInt (idxpart % (idx+1,size idxpart)))
			(False, _)  = (toInt idxpart, 0)
		= (genadt l r, genrec l r)

	genrec nrargs s = {index = 0, nr_cons = 1, nr_args = nrargs, args = [genarg i s \\ i <- [1..nrargs]]}
	genadt nrargs s = FTADT (NormalVar name 0) [SaplConstructor (NormalVar name 0) 0 [genarg i s \\ i <- [1..nrargs]]]
	
	genarg i s | s bitand (1 << (i-1)) > 0
		= TypedVar (StrictVar "_" 0) NoType
		= TypedVar (NormalVar "_" 0) NoType	

addTupleCons _ = returnS ()

read_int [TLit (LInt lit):ts] = returnS (Just lit, ts)
read_int ts = returnS (Nothing, ts)

type [TTypeDef, TIdentifier type:ts] = returnS (Type type, ts)
type ts = returnS (NoType, ts) 

expr [TOpenParenthesis:ts] = 
				mexpr ts
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (Just t, tl ts)
										  = returnE (ts, "Missing close parenthesisx")

expr [TLit lit:ts] = returnS (Just (SLit lit), ts)

expr [TIdentifier name:ts] = 
				getLevel
			>>= \level = returnS (NormalVar name level)
			>>= \t = addTupleCons name
			>>= \_ = args_expr ts
			>>= \(as, ts) = case as of
								[] = returnS (Just (SVar t), ts)
								   = returnS (Just (SApplication (SVar t) as), ts)

expr [TSelectKeyword:ts] = 
				sexpr ts 
			>>= mandatory "Missing select expression"
			>>= \(expr, ts) = type ts			
			>>= \(ty, ts) = read_int ts			
			>>= mandatory "Missing select index"
			>>= \(idx, ts) = args_expr ts
			>>= \(as, ts) = case as of
								[] = returnS (Just (SSelect expr ty idx), ts)
								   = returnS (Just (SApplication (SSelect expr ty idx) as), ts)

expr [TUpdateKeyword:ts] = 
				sexpr ts 
			>>= mandatory "Missing update expression"
			>>= \(expr, ts) = type ts
			>>= \(ty, ts) = upd_list ts
			>>= \(upds, ts) = returnS (Just (SUpdate expr ty upds), ts)
where
	upd_list [TOpenSquareBracket:ts] = 
				update_1 ts []
			>>=	\(us, ts) = case hd ts of
						TCloseSquareBracket = returnS (us, tl ts)
								      = returnE (ts, "Missing close square bracket")
	upd_list ts = returnE (ts, "Missing open bracket")

	update_1 [TLit (LInt idx),TColon:ts] as =
			 	expr ts
		 	>>= mandatory "Missing field update expression"
		 	>>= \(expr, ts) = update_2 ts [(idx, expr):as]
	update_1 ts as = returnE (ts, "Invalid field \"update\"")
	update_2 [TComma: ts] as = update_1 ts as
	update_2 ts as = returnS (reverse as, ts)	

expr ts = returnS (Nothing, ts)

sexpr [TOpenParenthesis:ts] = 
				mexpr ts
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (Just t, tl ts)
										  = returnE (ts, "Missing close parenthesisx")

sexpr [TLit lit:ts] = returnS (Just (SLit lit), ts)

sexpr [TIdentifier name:ts] = 
				getLevel
			>>= \level = returnS (NormalVar name level)
			>>= \t = addTupleCons name
			>>= \_ = returnS (Just (SVar t), ts)

sexpr ts = returnS (Nothing, ts)

mexpr ts = expr ts >>= mandatory "Missing expression"

letdefinitions ts = letdef_1 ts []
where
	letdef_1 [TIdentifier name, TTypeDef, TIdentifier type, TAssignmentOp:ts] as = 
				getLevel
			>>= \level = body False ts 
			>>= \(t, ts) = letdef_2 ts [SaplLetDef (TypedVar (NormalVar name level) (Type type)) t:as]
	letdef_1 [TIdentifier name, TAssignmentOp:ts] as = 
				getLevel
			>>= \level = body False ts 
			>>= \(t, ts) = letdef_2 ts [SaplLetDef (TypedVar (NormalVar name level) NoType) t:as]
	letdef_1 [TStrictIdentifier name, TTypeDef, TIdentifier type, TAssignmentOp:ts] as = 
				getLevel
			>>= \level = body False ts 
			>>= \(t, ts) = letdef_2 ts [SaplLetDef (TypedVar (StrictVar name level) (Type type)) t:as]			
	letdef_1 [TStrictIdentifier name, TAssignmentOp:ts] as = 
				getLevel
			>>= \level = body False ts 
			>>= \(t, ts) = letdef_2 ts [SaplLetDef (TypedVar (StrictVar name level) NoType) t:as]
	letdef_1 ts as = returnE (ts, "Invalid \"let\" definition")
	letdef_2 [TComma: ts] as = letdef_1 ts as
	letdef_2 ts as = returnS (reverse as, ts)

body simple [TOpenParenthesis:ts] = 
				body False ts
			>>=	\(t, ts) = case hd ts of
						TCloseParenthesis = returnS (t, tl ts)
										  = returnE (ts, "Missing close parenthesis")

body simple [TLetKeyword:ts] =
				incLevel ts
			>>= \ts = letdefinitions ts
			>>= \(ds, ts) = case hd ts of
					TInKeyword = returnS (tl ts)
							   = returnE (ts, "Missing \"in\" keyword")
			>>= \ts = body False ts
			>>= \(t, ts) = returnS (SLet t ds, ts)
			>>= decLevel

body simple [TCaseKeyword:ts] = 
				body True ts		
			>>= \(expr, ts) = args_pattern ts
			>>= \(ps, ts) = if (isEmpty ps) 
							   (returnE (ts, "Missing case patterns"))
							   (returnS (SCase expr ps, ts))
	
body simple [TOpenBracket:ts] = skip ts	// ABC code: skip it	
where
	skip [TCloseBracket:ts] = returnS (SAbortBody, ts)
	skip [] = returnE ([], "Missing close bracket in ABC code definition")
	skip [t:ts] = skip ts
			
body simple ts = ((if simple sexpr expr) ts) >>= mandatory "Missing expression"

args_expr ts = args_ sexpr ts
args_pattern ts = args_ arg_pattern ts

args_ f ts = args` ts []
where
	args` ts as = f ts 
				>>= \(t, ts) = case t of
						Just r = args` ts [r:as]
							   = returnS (reverse as, ts)

arg_pattern [TOpenParenthesis:TLit lit:ts] =
			case hd ts of
				TCaseAssignmentOp = body False (tl ts)
							  = returnE (ts, "Missing select assignment operator")
		>>=	\(t, ts) = case hd ts of
				TCloseParenthesis = returnS (Just (PLit lit, t), tl ts)
							  = returnE (ts, "Missing close parenthesis3")

arg_pattern [TOpenParenthesis:TIdentifier cons:ts] =
			incLevel ts
		>>= \ts = addTupleCons cons
		>>= \_ = args ts
		>>= \(as, ts) = case hd ts of
				TCaseAssignmentOp = body False (tl ts)
							  = returnE (ts, "Missing select assignment operator")
		>>=	\(t, ts) = case hd ts of
				TCloseParenthesis = returnS (Just (mbCons as, t), tl ts)
							  = returnE (ts, "Missing close parenthesis4")
		>>= decLevel
where
	mbCons as = if (cons=="_") PDefault (PCons cons as)

arg_pattern ts = returnS (Nothing, ts)

args ts = args_ ts []
where
	args_ [TIdentifier name:ts] as = getLevel >>= \level = args_ ts [NormalVar name level:as] 
	args_ ts as = returnS (reverse as, ts)

args_annotated ts = args_ ts []
where
	args_ [TIdentifier name,TTypeDef,TIdentifier type:ts] as = getLevel >>= \level = args_ ts [TypedVar (NormalVar name level) (Type type):as]
	args_ [TIdentifier name:ts] as = getLevel >>= \level = args_ ts [TypedVar (NormalVar name level) NoType:as] 
	args_ [TStrictIdentifier name,TTypeDef,TIdentifier type:ts] as = args_ ts [TypedVar (StrictVar name 0) (Type type):as]
	args_ [TStrictIdentifier name:ts] as = args_ ts [TypedVar (StrictVar name 0) NoType:as]
	args_ ts as = returnS (reverse as, ts)

args_record ts = args_1 ts []
where
	args_1 [TIdentifier name,TTypeDef,TIdentifier type:ts] as = getLevel >>= \level = args_2 ts [TypedVar (NormalVar name level) (Type type):as]
	args_1 [TIdentifier name:ts] as = getLevel >>= \level = args_2 ts [TypedVar (NormalVar name level) NoType:as]
	args_1 [TStrictIdentifier name,TTypeDef,TIdentifier type:ts] as = getLevel >>= \level = args_2 ts [TypedVar (StrictVar name level) (Type type):as]	
	args_1 [TStrictIdentifier name:ts] as = getLevel >>= \level = args_2 ts [TypedVar (StrictVar name level) NoType:as]	
	args_1 ts as = returnE (ts, "Missing argument")
	args_2 [TComma:ts] as = args_1 ts as
	args_2 ts as = returnS (reverse as, ts)

args_adt ts = args_1 ts [] 0
where
	args_1 [TIdentifier name:ts] cs i = 
			getLevel 
		>>= \level = args_annotated ts 
		>>= \(ss,ts) = args_2 ts [SaplConstructor (NormalVar name level) i ss:cs] i
		
	args_1 ts cs _ = returnE (ts, "Missing argument")
	args_2 [TVerticalBar:ts] cs i = args_1 ts cs (i+1)
	args_2 ts cs _ = returnS (reverse cs, ts)

// record
constr [TTypeDef, TIdentifier name, TAssignmentOp, TOpenBracket: ts] =
				getLevel
			>>= \level = args_record ts
			>>= \(as, ts) = case hd ts of
						TCloseBracket = addConstructor (NormalVar name level) {index = 0, nr_cons = 1, nr_args = length as, args = as} >>= \tname = returnS (FTRecord tname as, tl ts)
									  = returnE (ts, "Missing close parenthesis3")

// ADT
constr [TTypeDef, TIdentifier name, TAssignmentOp: ts] =
				getLevel
			>>= \level = args_adt ts 
			>>= \(as, ts) = addConstructors as
			>>= \_ = returnS (FTADT (NormalVar name level) as, ts)

constr [TTypeDef:ts] = returnE (ts, "Invalid type definition")
constr ts = returnE (ts, "Not a type definition")

func [TIdentifier name, TTypeDef, TIdentifier type, TCAFAssignmentOp:ts] = typed_caf name (Type type) ts			
func [TIdentifier name, TCAFAssignmentOp:ts] = typed_caf name NoType ts
func [TIdentifier name, TTypeDef, TIdentifier type:ts] = typed_fun name (Type type) ts			
func [TIdentifier name:ts] = typed_fun name NoType ts

func ts=:[TTypeDef:_] = constr ts >>= \(f,ts) = returnS (f, ts)
func ts = returnE (ts, "Not a function or type definition")

typed_caf name type ts =
				getLevel
			>>= \level = body False ts
			>>= \(t, ts) = addCAF (NormalVar name level) >>= \tname = returnS (FTCAF (TypedVar tname type) t, ts)

typed_fun name type ts =
				getLevel
			>>= \level = args_annotated ts 
			>>= \(as, ts) = case hd ts of
					TAssignmentOp	   = returnS (True, tl ts)
					TMacroAssignmentOp = returnS (False, tl ts)					
									   = returnE (ts, "Missing assignment operator")
			>>= \(func, ts) = body False ts 
			>>= \(t, ts) = if func 
							(addFunction (NormalVar name level) as >>= \tname = returnS (FTFunc (TypedVar tname type) t as, ts))
							(addFunction (NormalVar name level) as >>= \tname = returnS (FTMacro (TypedVar tname type) t as, ts))

skip_newlines [TEndOfLine:ts] = skip_newlines ts
skip_newlines ts = returnS ts

program ts fs =
			skip_newlines ts
		>>= \ts = func ts
		>>= \(f, ts) = skip_newlines ts
		>>= \ts = if (length ts == 0) (returnS ([f:fs], ts)) (program ts [f:fs])
		
parse :: [PosToken] -> MaybeError ErrorMsg ([FuncType],ParserState)
parse pts 
	# ts = map (\(PosToken _ _ t) = t) pts
	= case (program ts []) defaultState of
				Ok ((fts, _),ps) = Ok (fixReferences (ps.ps_genFuns ++ fts),ps)
				Error (ts, msg)  = let (lp, cp) = findpos ts in Error (msg+++" at line "+++toString lp+++" before character "+++toString cp)
where
	findpos rest_ts 
		# rest_pts = drop ((length pts)-(length rest_ts)-1) pts
		= case hd rest_pts of
			PosToken lp cp _ = (lp, cp)
		
parseExpr :: [PosToken] -> MaybeError ErrorMsg (SaplTerm,ParserState)		
parseExpr pts 
	# ts = map (\(PosToken _ _ t) = t) pts
	= case (body False ts) defaultState of
				Ok ((fts, _),ps) = Ok (fts,ps)
				Error (ts, msg)  = let (lp, cp) = findpos ts in Error (msg+++" at line "+++toString lp+++" before character "+++toString cp)
where
	findpos rest_ts 
		# rest_pts = drop ((length pts)-(length rest_ts)-1) pts
		= case hd rest_pts of
			PosToken lp cp _ = (lp, cp)
		
mergeParserStates :: ParserState (Maybe ParserState) -> ParserState
mergeParserStates pst1 (Just pst2)
	= {pst1 &
	   ps_constructors = mergeMaps pst2.ps_constructors pst1.ps_constructors,
	   ps_functions    = mergeMaps pst2.ps_functions    pst1.ps_functions,
	   ps_CAFs         = mergeMaps pst2.ps_CAFs         pst1.ps_CAFs,
	   ps_genFuns	   = []}
where
	mergeMaps m1 m2 = 'DM'.putList ('DM'.toList m2) m1

mergeParserStates pst1 Nothing = pst1
