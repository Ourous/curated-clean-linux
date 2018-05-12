implementation module Sapl.Target.JS.CodeGeneratorJS

/* TODO:
 *
 * - Cyclical let definitions are not handled correctly:
 *		1. strictness should be removed from the definition which references the later one
 *		2. tail recursion optimization shouldn't be used in the function which has cyclical let definitions (scoping problem)
 *
 */

import StdEnv, Data.Maybe, Text.StringAppender, Sapl.FastString
import qualified Data.List as DL
import qualified Data.Map as DM
import Text.Unicode.Encodings.JS
import Sapl.SaplTokenizer, Sapl.SaplParser, Sapl.Target.Flavour, Sapl.Optimization.StrictnessPropagation
import Sapl.Transform.Let
import Sapl.Target.JS.Lifting
import Sapl.Transform.AddSelectors
import Sapl.Transform.TailRecursion
import StdDebug

from Data.List import elem_by, partition

:: CoderState = { cs_inbody 		:: !Maybe SaplTypedVar     // The body of the function which is being generated (not signature)
				, cs_intrfunc		:: !Maybe SaplTypedVar     // The name of the currently generated function if it is tail recursive
				, cs_inletbind		:: !Maybe SaplTypedVar     // The name of the let binding we are in
				, cs_futuredefs		:: ![SaplTypedVar]     	   // for finding out about let-rec and let bindings defined later 
				, cs_incaseexpr		:: !Bool
				, cs_current_vars 	:: ![SaplTypedVar]		 		
				, cs_constructors	:: !Map String ConstructorDef
				, cs_functions		:: !Map String [SaplTypedVar]
				, cs_CAFs			:: !Map String ()
				, cs_builtins		:: !Map String (String, Int)
				, cs_inlinefuncs	:: !Map String InlineFunDef
				, cs_trampoline     :: !Bool
				, cs_prefix         :: !String
		      	}

newState :: !Flavour !Bool !ParserState -> CoderState
newState f tramp p =
			 { cs_inbody 		= Nothing
			 , cs_intrfunc 		= Nothing
			 , cs_inletbind		= Nothing
			 , cs_futuredefs	= []
			 , cs_incaseexpr 	= False
			 , cs_current_vars 	= []
			 , cs_constructors 	= p.ps_constructors
			 , cs_functions		= p.ps_functions
			 , cs_CAFs			= p.ps_CAFs
			 , cs_builtins		= f.builtInFunctions
			 , cs_inlinefuncs	= f.inlineFunctions
			 , cs_trampoline    = tramp
			 , cs_prefix        = f.Flavour.fun_prefix
			 }

pushArgs :: !CoderState ![SaplTypedVar] -> CoderState
pushArgs s [t:ts] = pushArgs {s & cs_current_vars = [t:s.cs_current_vars]} ts
pushArgs s [] = s

condForce :: !Bool !a !StringAppender -> StringAppender | Appendable a
condForce True e a = a <++ "Sapl.feval(" <++ e <++ ")"
condForce False e a = a <++ e

force e a = condForce True e a
forceApp e a = a <++ "Sapl.fapp(" <++ e <++ ")"

// Escape identifier, except the "$eval" part if it ends like that		
escapeName :: !String !String !StringAppender -> StringAppender
escapeName prefix name a = a <++ prefix <++ toString (urlEncode` (fromString name))
where 
	// A slightly modified URL encoding scheme
	urlEncode` :: ![Char] -> [Char]
	urlEncode` []						= []
	urlEncode` e=:['$eval']				= e		
	urlEncode` [x:xs] 
	| isAlphanum x						= [x  : urlEncode` xs]
	| otherwise							= urlEncodeChar x ++ urlEncode` xs
	where
		urlEncodeChar '_'				= ['_']
		urlEncodeChar '.'				= ['_']		
		urlEncodeChar ' '				= ['+']
		urlEncodeChar '$'				= ['$']		
		urlEncodeChar x					= ['$', c1 ,c2]
		
		(c1,c2)							= charToHex x

		charToHex :: !Char -> (!Char, !Char)
		charToHex c						= (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
		where
		        i						= toInt c
		        digitToHex :: !Int -> Int
		        digitToHex d
		                | d <= 9		= d + toInt '0'
		                | otherwise		= d + toInt 'A' - 10
				   
callWrapper :: !SaplTerm !CoderState !StringAppender -> StringAppender
callWrapper t s a
	| not (inline t)
		= termCoder t s a		
	| isJust s.cs_inletbind
		= a <++ "var " <++ termCoder (fromJust s.cs_inletbind) {s & cs_futuredefs = []} <++ "=" <++ forceTermCoder t s <++ ";"
	| isJust s.cs_intrfunc && isTailRecursive (fromJust s.cs_intrfunc) t
		= forceTermCoder t s a
	| s.cs_trampoline
		= a <++ "return " <++ trampolineCoder t s <++ ";" 	
		= a <++ "return " <++ forceTermCoder t s <++ ";" 

isTailRecursive :: !SaplTypedVar !SaplTerm -> Bool
isTailRecursive var (SCase _ patterns) = any (isTailRecursive var o snd) patterns
isTailRecursive var (SApplication (SVar avar) _) = unpackVar var == unpackVar avar
isTailRecursive var (SLet body _) = isTailRecursive var body
isTailRecursive _ _ = False

strictnessMap :: !SaplType !CoderState -> Int
strictnessMap NoType _ = 0
strictnessMap (Type cons) {cs_constructors} 
	= case 'DM'.get cons cs_constructors of
		Nothing = 0
		(Just {args}) = toInt args 0
where
	toInt [] _ = 0
	toInt [TypedVar (StrictVar _ _) _:as] i = (toInt as (i+1)) bitor (2 << i)
	toInt [TypedVar (NormalVar _ _) _:as] i = toInt as (i+1)	

funcCoder :: !FuncType !CoderState !StringAppender -> StringAppender
funcCoder (FTFunc name body args) s a = normalFunc name (addSelectors body) args s a
funcCoder (FTMacro name body args) s a = normalFunc name body args s a
funcCoder (FTCAF name body) s a = encodeCAF name body s a
funcCoder (FTADT name args) s a = foldl (\a t = termCoder t s a) a args
funcCoder (FTRecord name args) s a 
	# a = a <++ constructorCoder name 0 args s
	= a <++ termCoder name s <++ ".$f=[" <++ recordFieldCoder args <++ "];"

// Only real constants can be safely encoded as a simple variable...
encodeCAF :: !SaplTypedVar !SaplTerm !CoderState !StringAppender -> StringAppender
encodeCAF name body=:(SLit _) s a
	# a = a <++ "var " <++ termCoder name s <++ " = "

	# s = {s & cs_inbody = Just name
			 , cs_current_vars = []
			 , cs_intrfunc = Nothing
			 }

	# a = termCoder body s a
	= a <++ ";"  

// ... everything else must be wrapped into an anonymous function to avoid 
// undefined references (because of variables and functions declared later)
encodeCAF name body s a
	# a = a <++ "var " <++ termCoder name s <++ " = [function (){"

	# s = {s & cs_inbody = Just name
			 , cs_current_vars = []
			 , cs_intrfunc = Nothing
			 }

	# a = a <++ callWrapper body s 

	= a <++ "},[]];";

normalFunc :: !SaplTypedVar !SaplTerm ![SaplTypedVar] !CoderState !StringAppender -> StringAppender
normalFunc name body args s a
	// Generate $eval function if any of its arguments is annotated as strict	
	# a = if (any isStrictVar args) 
				(makeStrictClosure (unpackVar name) args s a) a
				
	// Generate function signature			
	# a = a <++ "function " <++ termCoder name s
			<++ "(" <++ termArrayCoder args "," s <++ "){"
	
	// Update coder state with the new local arguments, ...
	# s = {s & cs_inbody = Just name
			 , cs_current_vars = args
			 , cs_intrfunc = if (isTailRecursive name body) (Just name) Nothing}

	// Generate body (in a while(1) if the function is tail recursive)				
	# a = if (isJust s.cs_intrfunc) (a <++ "while(1){") a
	# a = callWrapper body s a
	# a = if (isJust s.cs_intrfunc) (a <++ "}") a
	= a <++ "};"  

// The (i-1) is to be compatible with the original compiler written in JavaScript
makeStrictClosure name args s a
	= a <++ "function " 
		<++ escapeName s.cs_prefix name <++ "$eval("
		<++ joinList "," ["a"+++toString (i-1) \\ i <- [1..length args]]
		<++ "){return " <++ escapeName s.cs_prefix name <++ "("
		<++ (\a = fst (foldl strictsep (a,1) args))
		<++ ");};"
where
	strictsep (a,i) arg
		# a = condForce (isStrictVar arg) (\a -> a <++ "a" <++ toString (i-1)) a
		| i < (length args)
			= (a <++ ",", i+1)
			= (a, i)

make_app_args :: !SaplVar ![SaplTerm] !CoderState !StringAppender -> StringAppender
make_app_args func args s a 
	= case 'DM'.get (unpackVar func) s.cs_functions of
		Just func_args = a <++ maa_ func_args args 0 s
					   = a <++ maa_ [] args 0 s 
where
	// fargs: formal, aargs: actual	
	maa_ [TypedVar (StrictVar _ _) _:fargs] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a
		= a <++ forceTermCoder aa s <++ maa_ fargs aargs (i+1) s 
	maa_ [_:fargs] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a		
		= a <++ termCoder aa s <++ maa_ fargs aargs (i+1) s 
	maa_ [] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a		
		= a <++ termCoder aa s <++ maa_ [] aargs (i+1) s 
	maa_ _ [] _ _ a = a

recordFieldCoder :: ![SaplTypedVar] !StringAppender -> StringAppender
recordFieldCoder [TypedVar t _] a = a <++ "\"" <++ unpackVar t <++ "\""
recordFieldCoder [TypedVar t _:ts] a
	= a <++ "\"" <++ unpackVar t <++ "\"," <++ recordFieldCoder ts
recordFieldCoder [] a = a

termArrayCoder :: ![a] !String !CoderState !StringAppender -> StringAppender | TermCoder a
termArrayCoder [t] sep s a = termCoder t s a
termArrayCoder [t:ts] sep s a
	= a <++ termCoder t s <++ sep <++ termArrayCoder ts sep s
termArrayCoder [] _ s a = a

//----------------------------------------------------------------------------------------
// Term coder instances

class TermCoder a 
where
	termCoder 		:: !a !CoderState !StringAppender -> StringAppender
	forceTermCoder 	:: !a !CoderState !StringAppender -> StringAppender
	trampolineCoder	:: !a !CoderState !StringAppender -> StringAppender

//----------------------------------------------------------------------------------------
// Data constructor...

constructorCoder :: !SaplVar !Int ![SaplTypedVar] CoderState !StringAppender -> StringAppender

// A zero argument data constructor is a CAF
constructorCoder name id [] s a
	= a <++ "var " <++ escapeName s.cs_prefix (unpackVar name) <++ " = [" <++ id <++ ",\"" <++ unpackVar name <++ "\"];"

constructorCoder name id args s a
	// Generate $eval function if any of its arguments is annotated as strict	
	# a = if (any isStrictVar args) 
				(makeStrictClosure (unpackVar name) args s a) a

	// Original field names are not necessary, they can be shorten
	# newargs = [NormalVar ("_"+++toString i) 0 \\ i <- [1..length args]]
		
	# a = a <++ "function " <++ termCoder name s <++ "(" <++ termArrayCoder newargs "," s
			<++ "){return [" <++ id <++ "," <++ termCoder name s <++ "$n"
	# a = case length args of
		0 = a
		  = a <++ "," <++ termArrayCoder newargs "," s 
	# a = a	<++ "];};"

	= a <++ "var " <++ termCoder name s <++ "$n = \"" <++ unpackVar name <++ "\";" 

constructorInliner :: !SaplVar !ConstructorDef ![SaplTerm] !CoderState !StringAppender -> StringAppender
constructorInliner name def [] s a
	= escapeName s.cs_prefix (unpackVar name) a

constructorInliner name def args s a
	# a = a <++ "[" <++ def.index <++ "," <++ escapeName s.cs_prefix (unpackVar name) <++ "$n"
	# a = case def.nr_args of
		0 = a
		  = a <++ "," <++ argsCoder def.args args ","  {s & cs_intrfunc = Nothing}  
	= a	<++ "]"
where
	// Formal arguments, actual arguments
	argsCoder [TypedVar (NormalVar _ _) _] [t] sep s a = termCoder t s a
	argsCoder [TypedVar (StrictVar _ _) _] [t] sep s a = forceTermCoder t s a	
	argsCoder [TypedVar (NormalVar _ _) _:fs] [t:ts] sep s a 
			= a <++ termCoder t s <++ sep <++ argsCoder fs ts sep s
	argsCoder [TypedVar (StrictVar _ _) _:fs] [t:ts] sep s a 
			= a <++ forceTermCoder t s <++ sep <++ argsCoder fs ts sep s	
	argsCoder [] [] _ s a = a

instance TermCoder SaplConstructor
where
	termCoder (SaplConstructor name id args) s a = constructorCoder name id args s a
	forceTermCoder t s a = termCoder t s a
	trampolineCoder t s a = termCoder t s a

//----------------------------------------------------------------------------------------
// Literals...

instance TermCoder Literal
where
	termCoder (LString ustr) s a = a <++ "\"" <++ toJSLiteral ustr <++ "\""
	termCoder (LChar uchr) s a = a <++ "'" <++ toJSLiteral uchr <++ "'"
	termCoder (LInt int) s a = a <++ int
	termCoder (LReal real) s a = a <++ real
	termCoder (LBool True) s a = a <++ "true"
	termCoder (LBool False) s a = a <++ "false"
	forceTermCoder t s a = termCoder t s a
	trampolineCoder t s a = termCoder t s a

//----------------------------------------------------------------------------------------
// Select patterns...

get_cons_or_die s cons = maybe (abort ("Data constructor "+++cons+++" cannot be found!")) 
			  		   id
			   	       ('DM'.get cons s.cs_constructors)

splitDefaultPattern :: ![(SaplPattern, SaplTerm)] -> (![(SaplPattern, SaplTerm)], !Maybe SaplTerm)
splitDefaultPattern patterns 
	= case partition (isDefaultPattern o fst) patterns of
			([],ps) 		= (ps, Nothing)
			([(_,d)],ps) 	= (ps, Just d)
							= abort "Error: more than one default branches in a select expression"

containsUnsafeSelect :: !CoderState !SaplTerm -> Bool
containsUnsafeSelect s (SApplication _ ts) = any (containsUnsafeSelect s) ts
containsUnsafeSelect s (SCase _ ps) = isUnsafeSelect s ps || any (containsUnsafeSelect s) (map snd ps)
containsUnsafeSelect s (SLet b _) = containsUnsafeSelect s b
containsUnsafeSelect s _ = False

isUsed :: SaplTerm SaplVar -> Bool
isUsed body var = w (unpackVar var) body
where
	w vn (SVar bvar) = unpackVar bvar == vn
	w vn (SApplication bvar bargs) = w vn bvar || any (w vn) bargs
	w vn (SCase bexpr branches) = w vn bexpr || any (w vn) (map snd branches)
	w vn (SLet bexpr bdefs) = w vn bexpr || any (w vn) (map unpackBindExpr bdefs)
	w vn (SSelect bexpr _ _) = w vn bexpr
	w vn (SUpdate bexpr _ updates) = w vn bexpr || any (w vn) (map snd updates)
	w _ _ = False

isUnsafeSelect :: !CoderState ![(SaplPattern, SaplTerm)] -> Bool
isUnsafeSelect s patterns
	= case ps of
		[(PCons name _, _):_] = isNothing d && (get_cons_or_die s name).nr_cons <> length ps
		[(PLit (LBool True), _),(PLit (LBool False), _):_] = False
		[(PLit (LBool False), _),(PLit (LBool True), _):_] = False		
		_ = isNothing d
where
	(ps, d) = splitDefaultPattern patterns

instance TermCoder (SaplPattern, SaplTerm, Bool)
where
	termCoder (PDefault, body, _) s a 
		= callWrapper body s a

	termCoder (PLit lit, body, _) s a 
		= a <++ "case " <++ termCoder lit s <++ ": " <++ callWrapper body s

	termCoder (PCons cons [], body, True) s a 
		= callWrapper body s a

	termCoder (PCons cons [], body, False) s a 
		= a <++ "case " <++ toString cons_idx <++ ": " <++ callWrapper body s
	where
		cons_idx  = (get_cons_or_die s cons).index
		
	termCoder (PCons cons args, body, singleton) s a 
		# s = pushArgs s (map annotate (zip2 get_cons.args args))	

		// In the case of singleton data constructor we omit "switch/case"
		# a = case singleton of
				True = a
				_ 	 = a <++ "case " <++ toString get_cons.index <++ ": "

		# (fargs, _) = foldl (\(fs, i) a -> if (isUsed body a) ([(a,i):fs],i+1) (fs,i+1)) ([], 0) args 

		= case fargs of
			[] = a <++ callWrapper body s
			fargs = a <++ "var " <++ instargs (reverse fargs) s <++ callWrapper body s
						
	where 
		instargs [(t,i)] s a = a <++ termCoder t s <++ "=ys[" <++ i+2 <++ "];"
		instargs [(t,i):ts] s a = a <++ termCoder t s <++ "=ys[" <++ i+2 <++ "]," <++ instargs ts s
		instargs [] s a = a

		get_cons   = get_cons_or_die s cons

		annotate (TypedVar (StrictVar _ _) type, arg) = TypedVar (toStrictVar arg) type
		annotate (TypedVar _ type, arg) = TypedVar arg type

	forceTermCoder t s a = termCoder t s a
	trampolineCoder t s a = termCoder t s a

//----------------------------------------------------------------------------------------
// Variables...

instance TermCoder SaplTypedVar
where
	forceTermCoder var s a = forceTermCoder (removeTypeInfo var) s a
	trampolineCoder var s a = trampolineCoder (removeTypeInfo var) s a
	termCoder var s a = termCoder (removeTypeInfo var) s a

instance TermCoder SaplVar
where
    forceTermCoder t=:(GlobalVar name) s a = forceTermCoder (NormalVar name 0) s a 
    
	forceTermCoder t=:(NormalVar name level) s a
		// Strict let definitions, strict arguments ...
		| any (eqStrictVar name) s.cs_current_vars
			= a <++ termCoder t s
			
		| isJust mbConstructor && constructor.nr_args == 0
			= constructorInliner t constructor [] s a		

		| isCAF 
			= force (escapeName s.cs_prefix name) a

		| isJust function_args && (length (fromJust function_args) == 0)
			= condForce s.cs_trampoline (\a -> a <++ escapeName s.cs_prefix name <++ "()") a
			= force (termCoder t s) a
	where
		mbConstructor = 'DM'.get name s.cs_constructors
		constructor = fromJust mbConstructor
		function_args = 'DM'.get name s.cs_functions	
		isCAF = isJust ('DM'.get name s.cs_CAFs)

	forceTermCoder (StrictVar name level) s a = forceTermCoder (NormalVar name level) s a

    trampolineCoder t=:(GlobalVar name) s a = forceTermCoder (NormalVar name 0) s a

	trampolineCoder t=:(NormalVar name _) s a
		| isJust mbConstructor && constructor.nr_args == 0
			= constructorInliner t constructor [] s a
			= a <++ termCoder t s
	where
		mbConstructor = 'DM'.get name s.cs_constructors
		constructor = fromJust mbConstructor

	trampolineCoder (StrictVar name level) s a = trampolineCoder (NormalVar name level) s a

    termCoder t=:(GlobalVar name) s a = forceTermCoder (NormalVar name 0) s a

	termCoder t=:(NormalVar name level) s a
		| isJust s.cs_inbody && not isLocalVar && isJust mbConstructor && constructor.nr_args == 0
			= constructorInliner t constructor [] s a	
				
		// custom data constructors can be inlined even at non-strict position
		| isJust mbInlineFun && inlineFun.InlineFunDef.data_cons && inlineFun.InlineFunDef.arity == 0
			= a <++ "(" <++ inlineFun.fun (\t a = termCoder t s a) (\t a = forceTermCoder t s a) [] <++ ")"
				
		| isJust s.cs_inbody && not isLocalVar && isJust mbCAF
			= a <++ escapeName s.cs_prefix name
			
		| isJust s.cs_inbody && not isLocalVar && isStrictFunction
			= a <++ escapeName s.cs_prefix name <++ "$eval"	

			// else (TODO: probably bogus in tail-recursion...)
			| any (eqVarByNameLevel t) (map removeTypeInfo s.cs_futuredefs)
				= a <++ "[function(){return " <++ force var_name <++ ";},[]]"

			// else: use the defined name if its a built-in function, otherwise its a variable...
			// no prefix for built-in functions
			= a <++ (maybe var_name (escapeName "" o fst) ('DM'.get name s.cs_builtins))			  
	where
		mbInlineFun = 'DM'.get name s.cs_inlinefuncs
		inlineFun = fromJust mbInlineFun
		mbConstructor = 'DM'.get name s.cs_constructors
		constructor = fromJust mbConstructor
		mbCAF = 'DM'.get name s.cs_CAFs

		// TODO: doc
		findLocalVar [TypedVar (NormalVar cn level) _:cs] = if (cn == name) level (findLocalVar cs)
		findLocalVar [TypedVar (StrictVar cn level) _:cs] = if (cn == name) level (findLocalVar cs)
		findLocalVar [] = 0
		isLocalVar = elem_by eqVarByName t (map removeTypeInfo s.cs_current_vars) //isMember t s.cs_current_vars
		
		isFunction = isJust ('DM'.get t s.cs_functions)		
		isStrictFunction = a || b
		where
			a = maybe False (any isStrictVar) ('DM'.get name s.cs_functions)
			b = maybe False (\{args} -> any isStrictVar args) ('DM'.get name s.cs_constructors)

		var_name a # decl_level = findLocalVar s.cs_current_vars
				   = case decl_level of
						0 = a <++ escapeName s.cs_prefix name
						  = a <++ escapeName s.cs_prefix name <++ "_" <++ decl_level

	termCoder (StrictVar name level) s a = termCoder (NormalVar name level) s a

//----------------------------------------------------------------------------------------
// Let definitions...

/*
 * A let definition is not the spine of the function, avoid tail recursion optimization:
 * {s & cs_intrfunc = Nothing}
 */
letDefCoder :: ![SaplLetDef] !Bool !CoderState !StringAppender -> StringAppender
letDefCoder [t] needsvar s a | inline (unpackBindExpr t) 
		= a <++ if needsvar "var " "," <++ termCoder t {s & cs_intrfunc = Nothing, cs_futuredefs=[toNormalVar (unpackBindVar t)]} <++ ";\n "
		= a <++ if needsvar "" ";\n" <++ termCoder t {s & cs_intrfunc = Nothing, cs_futuredefs=[toNormalVar (unpackBindVar t)]} <++ ";\n "
		
letDefCoder all=:[t:ts] needsvar s a | inline (unpackBindExpr t) 
	= a <++ if needsvar "var " "," <++ termCoder t {s & cs_intrfunc = Nothing, cs_futuredefs=fvs} 
		<++ letDefCoder ts False {s & cs_current_vars=[unpackBindVar t: s.cs_current_vars]}
where
	fvs = map (toNormalVar o unpackBindVar) all

letDefCoder all=:[t:ts] needsvar s a
	= a <++ if needsvar "" ";\n" <++ termCoder t {s & cs_intrfunc = Nothing, cs_futuredefs=fvs} <++ ";\n"
		<++ letDefCoder ts True {s & cs_current_vars=[unpackBindVar t: s.cs_current_vars]}
where
	fvs = map (toNormalVar o unpackBindVar) all

letDefCoder [] _ _ a = a

isDependent :: ![SaplVar] !SaplTerm -> Bool 
isDependent vs (SApplication (SVar f) as) = any (isDependent vs) [SVar f:as]
isDependent vs (SVar v) = elem_by eqVarByNameLevel v vs
isDependent _ _ = False

instance TermCoder SaplLetDef
where
	termCoder (SaplLetDef name body) s a | inline body
		= a <++ termCoder name {s & cs_futuredefs = []} <++ "=" 
			<++ (if (isStrictVar name) forceTermCoder termCoder) body s

	termCoder (SaplLetDef name body) s a
		= a <++ (if (isStrictVar name) forceTermCoder termCoder) body {s & cs_inletbind = Just name}
			
	forceTermCoder t s a = termCoder t s a
	trampolineCoder t s a = termCoder t s a

//----------------------------------------------------------------------------------------
// Expressions...

instance TermCoder SaplTerm
where
	// Generate code that forces the evaluation of the given term
	forceTermCoder :: !SaplTerm !CoderState !StringAppender -> StringAppender
	forceTermCoder t=:(SVar var) s a = forceTermCoder var s a
	
	forceTermCoder t=:(SApplication (SVar name) args) s a
		| isJust mbConstructor && constructor.nr_args == length args
			= constructorInliner name constructor args s a

		| isJust mbFunction && functionArity == length args

			= case (isJust s.cs_intrfunc && isTailRecursive (fromJust s.cs_intrfunc) t) of
				
				// It is posible that a tail recursive call has the same function as its
				// argument. In this case, the deeper call cannot be handled as tail recursive!
				True 	= a <++ make_tr_app args {s & cs_intrfunc = Nothing}	
				_		= condForce s.cs_trampoline 
							(\a -> a <++ func_name <++ "(" <++ make_app_args name args {s & cs_intrfunc = Nothing} <++ ")") a
				
		// more arguments than needed: split it
		| isJust mbFunction && functionArity < length args
			= forceApp (\a -> a <++ forceTermCoder (SApplication (SVar name) (take functionArity args)) s <++ ",[" 
							 <++ termArrayCoder (drop functionArity args) "," {s & cs_intrfunc = Nothing} <++ "]") a
				
		| isJust mbInlineFun && inlineFun.InlineFunDef.arity == length args
			= a <++ "(" <++ inlineFun.fun 
				(\t a = termCoder t {s & cs_intrfunc = Nothing} a)
				(\t a = forceTermCoder t {s & cs_intrfunc = Nothing} a) args <++ ")"

		// more arguments than needed: split it
		| isJust mbInlineFun && inlineFun.InlineFunDef.arity < length args
			= forceApp (\a -> a <++ inlineFun.fun 
										(\t a = termCoder t {s & cs_intrfunc = Nothing} a)
										(\t a = forceTermCoder t {s & cs_intrfunc = Nothing} a) (take inlineFun.InlineFunDef.arity args) <++ ",[" 
							 <++ termArrayCoder (drop inlineFun.InlineFunDef.arity args) "," {s & cs_intrfunc = Nothing} <++ "]") a
							 
		// BINs return no thunk, there is no need for feval even in trampolining
		// no prefix for built-in functions
		| isJust builtin && (snd (fromJust builtin)) == length args
			= a <++ escapeName "" (fst (fromJust builtin)) <++ "(" <++ make_app_args name args {s & cs_intrfunc = Nothing} <++ ")"

		// E.g.: in higher order functions application to argument
		| isNothing mbFunction && isNothing builtin
			= forceApp (\a -> a <++ termCoder name s <++ ",[" <++ termArrayCoder args "," s <++ "]") a

		// Otherwise: partial function application 
			= a <++ termCoder t s
			
	where
		func_name a = a <++ escapeName s.cs_prefix (unpackVar name) // skip level information

		mbConstructor = 'DM'.get (unpackVar name) s.cs_constructors
		constructor = fromJust mbConstructor
		mbInlineFun = 'DM'.get (unpackVar name) s.cs_inlinefuncs
		inlineFun = fromJust mbInlineFun
		mbFunction = 'DM'.get (unpackVar name) s.cs_functions
		functionArgs = fromJust mbFunction
		functionArity = length functionArgs

		tr_function_args = fromJust ('DM'.get (unpackVar (fromJust s.cs_intrfunc)) s.cs_functions)
		builtin = 'DM'.get (unpackVar name) s.cs_builtins

		make_tr_app args s a
			# setters = filter (not o isSame) (zip2 tr_function_args args)
			= case sortSetters setters of
				Nothing
					# (tr_function_args, args) = unzip setters	
					= a <++ "var " <++ mta_1 tr_function_args args 0 s <++ ";" 
						<++ mta_2 tr_function_args 0 s <++ "continue; /* 1 */"
				// Reverse topological order is probably safe
				(Just ordered) = a <++ gen_setters (reverse ordered) s <++ "continue; /* 2 */"
				
		where
			mta_1 [TypedVar (StrictVar _ _) _:fargs] [aa:aargs] i s a 
				# a = if (i>0) (a <++ ",") a
				= a <++ "t" <++ i <++ "=" <++ forceTermCoder aa s <++ mta_1 fargs aargs (i+1) s 
			mta_1 [_:fargs] [aa:aargs] i s a 
				# a = if (i>0) (a <++ ",") a		
				= a <++ "t" <++ i <++ "=" <++ termCoder aa s <++ mta_1 fargs aargs (i+1) s 
			mta_1 [] _ i s a = a

			mta_2 [fa:fargs] i s a 
				= a <++ escapeName s.cs_prefix (unpackVar fa) <++ "=t" <++ i <++ ";" <++ mta_2 fargs (i+1) s // skip level information for TR!
			mta_2 [] i s a = a

			isSame (TypedVar var1 _, SVar var2) = unpackVar var1 == unpackVar var2
			isSame _ = False

			gen_setters [(TypedVar (StrictVar vn _) _,expr):ss] s a 
				= a <++ escapeName s.cs_prefix vn <++ "=" <++ forceTermCoder expr s <++ ";" <++ gen_setters ss s
			gen_setters [(TypedVar (NormalVar vn _) _,expr):ss] s a 
				= a <++ escapeName s.cs_prefix vn <++ "=" <++ termCoder expr s <++ ";" <++ gen_setters ss s
			gen_setters [] s a = a

	forceTermCoder (SApplication sel=:(SSelect _ _ _) args) s a
		= a <++ "Sapl.fapp(" <++ forceTermCoder sel s <++ ",["  
				<++ termArrayCoder args "," s
				<++ "])"

	forceTermCoder t=:(SSelect expr type idx) s a 
		| isStrict idx 
			= a <++ forceTermCoder expr {s & cs_intrfunc = Nothing} <++ "[" <++ idx + 2 <++ "]"
			= a <++ "Sapl.feval(" <++ forceTermCoder expr {s & cs_intrfunc = Nothing} <++ "[" <++ idx + 2 <++ "])"
	where
		strictness = strictnessMap type s
		isStrict idx = (strictness bitand (2 << idx)) > 0
	
	// It is always in a strict let bind
	forceTermCoder t=:(SUpdate expr type updates) s a
      = a <++ "var " <++ termCoder var {s & cs_inletbind = Nothing, cs_futuredefs = []} <++ "=" <++ forceTermCoder expr {s & cs_inletbind = Nothing} <++ ".slice(0);" <++ genUpd updates;
	where
		var = fromJust s.cs_inletbind
	
		strictness = strictnessMap type s
		isStrict idx = (strictness bitand (2 << idx)) > 0
	
		genUpd [] a = a
		genUpd [(idx, expr):us] a 
			= a <++ termCoder var {s & cs_inletbind = Nothing, cs_futuredefs = []} <++ "[" <++ idx + 2 <++ "]=" <++ 
			(if (isStrict idx) forceTermCoder termCoder) expr {s & cs_inletbind = Nothing} <++ ";" <++ genUpd us

	forceTermCoder t s a = termCoder t s a

	// During trampolining, in only very special cases the expressions are forced in tail call
	trampolineCoder :: !SaplTerm !CoderState !StringAppender -> StringAppender
	trampolineCoder t=:(SVar var) s a = trampolineCoder var s a
		
	trampolineCoder t=:(SApplication (SVar name) args) s a
		| isJust mbConstructor && constructor.nr_args == length args
			= constructorInliner name constructor args s a	

		| isJust mbInlineFun && inlineFun.InlineFunDef.arity == length args
			= a <++ "(" <++ inlineFun.fun 
				(\t a = termCoder t {s & cs_intrfunc = Nothing} a)
				(\t a = forceTermCoder t {s & cs_intrfunc = Nothing} a) args <++ ")"

			= a <++ termCoder t s
	where
		mbConstructor = 'DM'.get (unpackVar name) s.cs_constructors
		constructor = fromJust mbConstructor
		mbInlineFun = 'DM'.get (unpackVar name) s.cs_inlinefuncs
		inlineFun = fromJust mbInlineFun

	trampolineCoder t s a = termCoder t s a

	termCoder :: !SaplTerm !CoderState !StringAppender -> StringAppender
	termCoder t=:(SVar var) s a = termCoder var s a
		
	termCoder t=:(SSelect expr type idx) s a
		| isStrict idx
			= a <++ "[Sapl.sselect,[" <++ termCoder expr {s & cs_intrfunc = Nothing} <++ ", " <++ idx + 2 <++ "]]"
			= a <++ "[Sapl.select,[" <++ termCoder expr {s & cs_intrfunc = Nothing} <++ ", " <++ idx + 2 <++ "]]"			
	where
		strictness = strictnessMap type s
		isStrict idx = (strictness bitand (2 << idx)) > 0

	// Should not happen, at this point "update" is always at strict position
	termCoder t=:(SUpdate _ _ _) s a
      = a <++ "/* UPD */"

	termCoder t=:(SCase expr patterns) s a | any (isConsPattern o fst) patterns
		# a = a <++ "var ys=" <++ forceTermCoder expr {s & cs_intrfunc = Nothing} <++  ";"
		= if (containsUnsafeSelect s t) (unsafe a) (safe a)
	where 
		isSingleton cons = (get_cons_or_die s cons).nr_cons == 1
		addSwitch e a = a <++ "switch(ys[0]){" <++ e <++ "};"
		(ps, d) = splitDefaultPattern patterns	

		// Something is very wrong with type inference here
		
		ups :: [(SaplPattern, SaplTerm, Bool)]
		ups = map (\(p,b)=(p,b,False)) ps

		defp :: SaplTerm Bool -> (SaplPattern, SaplTerm, Bool)
		defp d b = (PDefault,d,b)

		cp :: SaplPattern SaplTerm Bool -> (SaplPattern, SaplTerm, Bool)
		cp p d b = (p,d,b)

		unsafe a
			# a = addSwitch (termArrayCoder ups "" {s & cs_incaseexpr = True}) a
			= case d of
				(Just d) = a <++ termCoder (defp d False) s <++ ";"
						 = a <++ (if s.cs_incaseexpr "break;" "throw \"nomatch\";")
						 
		safe a
			# a = case patterns of
					[(p,body)] = if (isSingleton (fromJust (unpackConsName p))) 
										(termCoder (cp p body True) s a)
										(addSwitch (termCoder (cp p body False) s) a)
							   = addSwitch (termArrayCoder ups "" s) a
			= case d of
				(Just d) = a <++ termCoder (defp d False) s <++ ";"
						 = a

	termCoder t=:(SCase expr [(PLit (LBool True), true_expr),(PLit (LBool False), false_expr)]) s a
		= termCodeIf expr true_expr false_expr s a

	termCoder t=:(SCase expr [(PLit (LBool False), false_expr),(PLit (LBool True), true_expr)]) s a
		= termCodeIf expr true_expr false_expr s a
	
	termCoder t=:(SCase expr patterns) s a
	    # a = a <++ "switch(" <++ forceTermCoder expr {s & cs_intrfunc = Nothing} <++ "){" 
	            <++ termArrayCoder (map (\(p,b)=(p,b,False)) ps) "" {s & cs_incaseexpr = True} <++ "};"
		= case d of
			(Just d) = a <++ termCoder (PDefault,d,False) s <++ ";"
					 = a <++ (if s.cs_incaseexpr "break;" "throw \"nomatch\";")			
	where
		(ps, d) = splitDefaultPattern patterns

	termCoder (SApplication (SVar name) args) s a
		// It's only safe if there is no immediate evaluation
		| isJust mbConstructor && constructor.nr_args == length args && not (any isStrictVar constructor.args)
			= constructorInliner name constructor args s a	
		
		// custom data constructors can be inlined even at non-strict position
		| isJust mbInlineFun && inlineFun.InlineFunDef.data_cons && inlineFun.InlineFunDef.arity == length args
			= a <++ "(" <++ inlineFun.fun 
				(\t a = termCoder t {s & cs_intrfunc = Nothing} a)
				(\t a = forceTermCoder t {s & cs_intrfunc = Nothing} a) args <++ ")"
					
			= a <++ "[" <++ termCoder name s <++ ",[" 
				<++ termArrayCoder args "," s
				<++ "]]"
	where
		mbConstructor = 'DM'.get (unpackVar name) s.cs_constructors
		constructor = fromJust mbConstructor
		func_name name a = a <++ escapeName s.cs_prefix (unpackVar name) // skip level information

		mbInlineFun = 'DM'.get (unpackVar name) s.cs_inlinefuncs
		inlineFun = fromJust mbInlineFun

    // Dynamic application: fun part is always strict
	termCoder (SApplication sel=:(SSelect _ _ _) args) s a
		= a <++ "[" <++ forceTermCoder sel s <++ ",["  
				<++ termArrayCoder args "," s
				<++ "]]"

	termCoder (SLit lit) s a = termCoder lit s a

	/* Let definitions can be cross references to each other.
	 * If a let definition has reference to an other which is not yet declared
	 * (or recursive) the referenced variable must be wrap into a closure.
	 * cs_inletdef contains all the remaining let definitions (letDefCoder 
	 * removes the elements step by step)
	 */
	termCoder (SLet body defs) s a
		# s = pushArgs s defnames
		= a <++ letDefCoder newdefs True s
			<++ callWrapper body {s & cs_current_vars = defnames ++ s.cs_current_vars} <++ ";"
	where
		newdefs = case sortBindings defs of
						Just ds = ds
						Nothing = defs
						//Nothing = abort ("Cycle in let definitions is detected in function "+++toString (fromJust s.cs_inbody)+++"\n") // This is not supported currently

		defnames = map unpackBindVar newdefs

termCodeIf cond_expr true_expr false_expr s a 
	| inline cond_expr && inline true_expr && inline false_expr && 
	  not (isJust s.cs_intrfunc && (isTailRecursive (fromJust s.cs_intrfunc) true_expr || isTailRecursive (fromJust s.cs_intrfunc) false_expr))
	    = a <++ "(" <++ forceTermCoder cond_expr {s & cs_intrfunc = Nothing} <++ "?" 
	        <++ forceTermCoder true_expr {s & cs_incaseexpr = True} <++ ":" <++ forceTermCoder false_expr {s & cs_incaseexpr = True} <++ ")"

termCodeIf cond_expr texpr fexpr s a
	    = a <++ "if(" <++ forceTermCoder cond_expr {s & cs_intrfunc = Nothing} <++ "){" 
	        <++ callWrapper texpr {s & cs_incaseexpr = True} <++ "}else{" <++ callWrapper fexpr {s & cs_incaseexpr = True} <++ "}"
	
generateJS :: !Flavour !Bool !String !(Maybe ParserState) -> MaybeErrorString (StringAppender, ParserState)
generateJS f tramp saplsrc mbPst
	# pts = tokensWithPositions saplsrc
	= case parse pts of
		Ok (funcs, s) # newpst = mergeParserStates s mbPst
					  # (funcs, newpst) = if (isSet f "enableStrictnessPropagation") (doStrictnessPropagation newpst (isStrictArgFlavour f) funcs) (funcs, newpst)
					  # state = newState f tramp newpst
					  # a = newAppender <++ "\"use strict\";"
					  # a = a <++ "/*Trampoline: "
					  # a = if tramp (a <++ "ON") (a <++ "OFF")
					  
					  // Lift + generated update functions
					  # (funcs, genfuns) = foldl (upd (isStrictArgFlavour f newpst)) ([], 'DM'.newMap) funcs
					  # funcs = reverse funcs ++ 'DM'.elems genfuns 
					  
					  # a = foldl (\a curr = a <++ funcCoder curr state) (a <++ "*/") funcs
					  = Ok (a, newpst)
		Error msg = Error msg	
where		
  upd :: (String Int Int -> Bool) ([FuncType], Map String FuncType) FuncType -> ([FuncType], Map String FuncType)
  upd sf (nfs, genfuns) fun 
  	= let (nfun, ngenfuns) = prepareFun sf fun genfuns in ([nfun:nfs], 'DM'.union genfuns ngenfuns)
  		
exprGenerateJS :: !Flavour !Bool !String !(Maybe ParserState) !StringAppender -> (MaybeErrorString (String, StringAppender, ParserState))
exprGenerateJS f tramp saplsrc mbPst out
	# pts = tokensWithPositions saplsrc
	= case parseExpr pts of
		Ok (body, s) # newpst = mergeParserStates s mbPst
					 # state = newState f tramp newpst
					 
					  // Lift + generated update functions. TODO: do not skip generated functions
					  # (body, _) = prepareExpr (isStrictArgFlavour f newpst) body 'DM'.newMap
					 
					 # a = termCoder body {state & cs_inbody=Just (TypedVar (NormalVar "__dummy" 0) NoType)} newAppender
					 # out = foldl (\a curr = a <++ funcCoder curr state) out s.ps_genFuns
					 = Ok (toString a, out, newpst)
		Error msg = Error msg
		
