implementation module _SystemDynamic

import StdEnv

// macros to remove ugly underscores

TypeFixedVar
	:==	_TypeFixedVar
TypeEmpty
	:==	_TypeEmpty
:: _UnificationEnvironment
	:==	UnificationEnvironment

// sanity check for closed types
checkClosedType :: TypeCode -> TypeCode
checkClosedType type_code
	| is_valid_type_scheme type_code
		=	type_code
	where
		is_valid_type_scheme :: TypeCode -> Bool
		// type schemes can only occur at the top level (rank 1)
		is_valid_type_scheme (TypeScheme n type)
			| n <= 0
				=	fatal "checkClosedType"
						("invalid number of type vars " +++  toString n)
			| [TypeVar i \\ i <- [0..n-1]] <> FV type
				=	fatal "checkClosedType" "invalid range of type vars"
			// otherwise
				=	is_valid_type type

		is_valid_type :: TypeCode -> Bool
		is_valid_type (TypeVar n)
			=	True
		is_valid_type (TypeCons _)
			=	True
		is_valid_type (TypeApp t a)
			=	is_valid_type t && is_valid_type a
		is_valid_type (TypeUnique t)
			=	is_valid_type t
		is_valid_type (_TypeFixedVar _)
			=	fatal "checkClosedType" "unexpected _TypeFixedVar"
		is_valid_type _TypeEmpty
			=	fatal "checkClosedType" "unexpected _TypeEmpty"

		FV :: TypeCode -> [TypeCode]
		FV alpha=:(TypeVar _)
			=	[alpha]
		FV (TypeCons _)
			=	[]
		FV (TypeApp t arg)
			=	removeDup (FV t ++ FV arg)
		FV (TypeUnique t)
			=	FV t

concatStrings :: [{#Char}] -> .{#Char}
concatStrings l
	=	updateS 0 l (_createArray (sum [size s \\ s <- l]))
	where
		updateS :: !Int [{#Char}] *{#Char} -> *{#Char}
		updateS i [] s
			=	s
		updateS i [h : t] s
			=	updateS (i + size h) t {s & [pos] = c \\ c <-: h & pos <- [i..]}

// this only works for closed types
instance toString TypeCode where
	toString type
		=	concatStrings (show False type)
	where
		show :: Bool TypeCode -> [{#Char}]
		show pars (TypeScheme n type)
			=	parentheses pars (["A."] ++ showTypeVars [0 .. n-1]
												++ [": " : show False type])
			where
				showTypeVars tvs
					=	sepBy " " (map showTypeVar tvs)
		show _ (TypeVar tv)
			=	[showTypeVar tv]
		show _ (TypeCons cons)
			=	[toString cons]
		show pars (TypeUnique type)
			=	["*" : show pars type]	
		show pars t
			=	show_app pars (reverse (uncurry_rev t))
			where
				uncurry_rev :: TypeCode -> [TypeCode]
				uncurry_rev (TypeApp t a)
					=	[a : uncurry_rev t]
				uncurry_rev t
					=	[t]

		show_app :: Bool [TypeCode] -> [{#Char}]
		show_app pars [TypeCons cons : args]
			| cons == TypeCodeConstructor_Arrow && length args == 2
				=	parentheses pars (show True (args !! 0)
							++ [" -> "] ++ show False (args !! 1))
			# (arity, pre, post)
				=	is_special cons
			| arity == length args
				=	[pre]
				++	flatten (sepBy [", "] (map (show False) args))
				++	[post]
		show_app pars l
			= parentheses pars (flatten (sepBy [" "] (map (show True) l)))

		parentheses :: Bool [{#Char}] -> [{#Char}]
		parentheses pars l
			| pars
				=	["("] ++ l ++ [")"]
			// otherwise
				=	l

		is_special :: TypeCodeConstructor -> (!Int, !{#Char}, !{#Char})
		is_special cons
			| not (typeCodeConstructorIsPredefined cons)
			|| cons == TypeCodeConstructorInt
			|| cons == TypeCodeConstructorChar
			|| cons == TypeCodeConstructorReal
			|| cons == TypeCodeConstructorBool
			|| cons == TypeCodeConstructorDynamic
			|| cons == TypeCodeConstructorFile
			|| cons == TypeCodeConstructorWorld
			|| cons == TypeCodeConstructor_Arrow
				=	(-1, "", "")
			| cons == TypeCodeConstructor_List
				=	(1, "[", "]")
			| cons == TypeCodeConstructor_StrictList
				=	(1, "[!", "]")
			| cons == TypeCodeConstructor_UnboxedList
				=	(1, "[#", "!]")
			| cons == TypeCodeConstructor_TailStrictList
				=	(1, "[", "!]")
			| cons == TypeCodeConstructor_StrictTailStrictList
				=	(1, "[!", "!]")
			| cons == TypeCodeConstructor_UnboxedTailStrictList
				=	(1, "[#", "!]")
			| cons == TypeCodeConstructor_LazyArray
				=	(1, "{", "}")
			| cons == TypeCodeConstructor_StrictArray
				=	(1, "{!", "}")
			| cons == TypeCodeConstructor_UnboxedArray
				=	(1, "{#", "}")
			=	is_tuple_cons 2 32 cons
			where
				is_tuple_cons i n cons
					| i > n
						=	(-1, "", "")
					| cons == TypeCodeConstructor_Tuple i
						=	(i, "(", ")")
					// otherwise
						=	is_tuple_cons (i+1) n cons

sepBy :: a [a] -> [a]
sepBy _ []
	=	[]
sepBy _ [x]
	=	[x]
sepBy sep [h : t]
	=	[h, sep : sepBy sep t]

showTypeVar :: Int -> {#Char}
showTypeVar tv
	| tv < 26
		=	toString (toChar (toInt 'a' + tv))
		=	"tv" +++ toString (tv - 26)

instance == TypeCode where
	(==) (TypeScheme n1 t1) (TypeScheme n2 t2)
		=	n1 == n2 && t1 == t2
	(==) (TypeApp t1 arg1) (TypeApp t2 arg2)
		=	t1 == t2 && arg1 == arg2
	(==) (TypeVar a) (TypeVar b)
		=	a == b
	(==) (TypeCons a) (TypeCons b)
		=	a == b
	(==) (TypeUnique a) (TypeUnique b)
		=	a == b
	(==) TypeEmpty TypeEmpty
		=	True
	(==) _ _
		=	False

/*	----------------------------------------------------------------------------
	Unification functions
----------------------------------------------------------------------------- */

_initial_unification_environment :: !Int !Int -> *_UnificationEnvironment
_initial_unification_environment n_type_pattern_vars n_type_vars
	=	initial_unification_environment n_type_pattern_vars n_type_vars

_bind_global_type_pattern_var :: !TypeCode !TypeCode
		!*_UnificationEnvironment -> *_UnificationEnvironment
_bind_global_type_pattern_var var (_type) subst
	=	bind_global_type_pattern_var var _type subst

_unify :: !_UnificationEnvironment !TypeCode !TypeCode
									-> (!Bool, _UnificationEnvironment)
_unify subst t1 t2
	=	unify_types subst t1 t2

_normalise :: !_UnificationEnvironment !TypeCode -> TypeCode
_normalise subst t
	=	normalise subst t

fatal :: {#Char} {#Char} -> .a
fatal function_name message
	=	abort ("_SystemDynamic, " +++ function_name +++ ": " +++ message)

valueAndType :: !Dynamic -> (a, TypeCode)
valueAndType d
	= code {
		pop_a	0
	}

valueAndEvaluatedType :: !Dynamic -> (a, !TypeCode)
valueAndEvaluatedType d
	=	valueAndType d

// the intermediate function valueAndEvaluatedType is used to prevent a
// jmp_eval_upd (which would create a _ind indirection in the returned node)
typeCodeOfDynamic :: !Dynamic -> TypeCode
typeCodeOfDynamic d
	=	snd (valueAndEvaluatedType d)

/*	----------------------------------------------------------------------------
	Efficient unification

	A reasonably efficient unifier.
----------------------------------------------------------------------------- */

:: UnificationEnvironment =
	{	subst	:: !.{!TypeCode}
	,	fixed	:: !Int
	,	global_tpvs :: ![Int]
	}

initial_unification_environment :: !Int !Int -> *UnificationEnvironment
initial_unification_environment n fixed
	=	{subst = createArray n TypeEmpty, fixed = fixed, global_tpvs = []}

bind_global_type_pattern_var :: !TypeCode !TypeCode !*UnificationEnvironment
													-> *UnificationEnvironment
bind_global_type_pattern_var tpv ts=:(TypeScheme n _) unify_env=:{fixed}
	# (unify_env, type)
		=	fresh_type FixTypeVars unify_env ts
	=	bind_global_type_pattern_var tpv type unify_env
bind_global_type_pattern_var (TypeVar n) type unify_env
	// sanity check ...
	| unify_env.subst.[n] <> TypeEmpty
		=	fatal "bind_global_type_pattern_var" "already bound"
	// ... sanity check
	=	{unify_env & subst.[n] = type, global_tpvs = [n:unify_env.global_tpvs]}

unify_types :: !UnificationEnvironment !TypeCode !TypeCode
											-> (!Bool, !UnificationEnvironment)
unify_types unify_env t1 t2
	# (unify_env, t1)
		=	fresh_type Don`tFixTypeVars unify_env t1
	# (unified, subst)
		=	unify t1 t2 unify_env.subst
	# unify_env
		=	{unify_env & subst = subst}
	| unified
		=	(proper t2 unify_env, unify_env)
	// otherwise
		=	(False, unify_env)

:: NormaliseState =
	{	normalised_vars			:: .{!TypeCode}
	,	normalised_fixed_vars	:: .{!TypeCode}
	,	free_var_count			:: !Int
	}

normalise :: !UnificationEnvironment !TypeCode -> TypeCode
normalise unify_env (TypeScheme _ type)
	=	normalise unify_env type
normalise unify_env type
	# ns
		=	{	normalised_vars = createArray (size unify_env.subst) TypeEmpty
			,	normalised_fixed_vars = createArray unify_env.fixed TypeEmpty
			,	free_var_count = 0
			}
	# (_, type, {free_var_count})
		=	normalise_type unify_env.subst type ns
	| free_var_count > 0
		=	TypeScheme free_var_count type
	// otherwise
		=	type

normalise_type :: {!TypeCode} TypeCode *NormaliseState
										-> (Bool, TypeCode, *NormaliseState)
normalise_type subst (TypeVar i) ns
	# (t, ns)
		=	normalise_type_var subst i ns
	=	(True, t, ns)
normalise_type subst (TypeFixedVar i) ns
	# (t, ns)
		=	normalise_fixed_type_var i ns
	=	(True, t, ns)
normalise_type subst tc=:(TypeCons _) ns
	=	(False, tc, ns)
normalise_type subst ta=:(TypeApp t arg) ns
	# (rt, t`, ns)
		=	normalise_type subst t ns
	# (ra, arg`, ns)
		=	normalise_type subst arg ns
	| rt || ra
		=	(True, TypeApp t` arg`, ns)
	// otherwise
		=	(False, ta, ns)
normalise_type subst tc=:(TypeUnique type) ns
	# (rt, type`, ns)
		=	normalise_type subst type ns
	| rt
		=	(True, TypeUnique type`, ns)
	// otherwise
		=	(False, tc, ns)

normalise_type_var :: {!TypeCode} Int *NormaliseState
												-> (TypeCode, *NormaliseState)
normalise_type_var subst i ns=:{normalised_vars, free_var_count}
	# (normalised_var, normalised_vars)
		=	normalised_vars![i] 
	# ns
		=	{ns & normalised_vars = normalised_vars}
	| normalised_var == TypeEmpty
		// the substitution for this variable is not normalised yet
		# s
			=	subst.[i] 
		| s == TypeEmpty
			// this is the first encounter of an unbound type variable
			# (normalised_var, ns)
				=	fresh_normalised_var ns
			=	(normalised_var, {ns & normalised_vars.[i] = normalised_var})
		// otherwise
			# (_, normalised_var, ns)
				=	normalise_type subst s ns
			=	(normalised_var, {ns & normalised_vars.[i] = normalised_var})
	// otherwise
		// the substitution is already normalised		
		=	(normalised_var, ns)

normalise_fixed_type_var :: Int *NormaliseState -> (TypeCode, *NormaliseState)
normalise_fixed_type_var i ns=:{normalised_fixed_vars, free_var_count}
	# (normalised_var, normalised_fixed_vars)
		=	normalised_fixed_vars![i]
	# ns
		=	{ns & normalised_fixed_vars = normalised_fixed_vars}
	| normalised_var == TypeEmpty
		// this is the first encounter of an fixed type variable
		# (normalised_var, ns)
			=	fresh_normalised_var ns
		=	(normalised_var, {ns & normalised_fixed_vars.[i] = normalised_var})
	// otherwise
		// the substitution is already normalised		
		=	(normalised_var, ns)

fresh_normalised_var :: *NormaliseState -> (TypeCode, *NormaliseState)
fresh_normalised_var ns=:{free_var_count}
	=	(TypeVar free_var_count, {ns & free_var_count = free_var_count + 1})

isIndirection :: TypeCode -> Bool
isIndirection TypeEmpty
	=	False
isIndirection type
	=	True

// FIXME: this is a quick hack, try to think of something better
proper :: TypeCode UnificationEnvironment -> Bool
proper type unify_env
	# (_, p)
		=	binds_to_fixed type unify_env.subst
							(createArray (size unify_env.subst) '?')
	=	length [1 \\ i <- [0..size p-1] | p.[i] == 'Y'
								&& not (isMember i unify_env.global_tpvs)] == 0

binds_to_fixed :: TypeCode {!TypeCode} *{#Char} -> (!Bool, !*{#Char})
binds_to_fixed (TypeFixedVar _) subst p
	=	(True, p)
binds_to_fixed tv1=:(TypeVar tv_number) subst p
	# (c, p)
		=	p![tv_number]
	| c == 'Y'
		=	(True, p)
	| c == 'N'
		=	(False, p)
	| c == '?'
		# (fixed, p)
			=	binds_to_fixed subst.[tv_number] subst p
		=	(fixed, {p & [tv_number] = if fixed 'Y' 'N'})
binds_to_fixed (TypeCons _) subst p
	=	(False, p)
binds_to_fixed (TypeApp t arg) subst p
	# (fixed_t, p)
		=	binds_to_fixed t subst p
	# (fixed_arg, p)
		=	binds_to_fixed arg subst p
	=	(fixed_t || fixed_arg, p)
binds_to_fixed (TypeUnique type) subst p
	=	binds_to_fixed type subst p
binds_to_fixed TypeEmpty _ p
	=	(False, p)

occurs :: {!TypeCode} Int TypeCode -> Bool
occurs substs tv_number1 (TypeVar tv_number2)
	=	occurs_var substs tv_number1 tv_number2
occurs _ _ (TypeFixedVar _)
	=	False
occurs _ _ (TypeCons _)
	=	False
occurs substs tv_number (TypeApp t arg)
	=	any (occurs substs tv_number) [t, arg]
occurs _ tv_number TypeEmpty
	=	False
occurs subst tv_number (TypeUnique t)
	=	occurs subst tv_number t
occurs _ _ ts=:(TypeScheme _ _)
	=	fatal "occurs" ("unexpected type scheme " +++ toString ts)

occurs_var :: {!TypeCode} Int Int -> Bool
occurs_var substs tv_number1 tv_number2
	| tv_number1 == tv_number2
		=	True
		=	occurs substs tv_number1 substs.[tv_number2]

unify :: TypeCode TypeCode *{!TypeCode} -> (Bool, *{!TypeCode})
unify (TypeVar tv_number1) t2 subst
	# (t1, subst)
		=	subst![tv_number1]
	| isIndirection t1
		=	unify t1 t2 subst
	// otherwise
		=	unifyVar tv_number1 t2 subst
	where
			unifyVar :: Int TypeCode *{!TypeCode} -> (Bool, *{!TypeCode})
			unifyVar tv_number1 tv2=:(TypeVar tv_number2) subst
				# (t2, subst)
					=	subst![tv_number2]
				| isIndirection t2
					=	unifyVar tv_number1 t2 subst
				| tv_number1 == tv_number2
					=	(True, subst)
				// otherwise
					=	(True, {subst & [tv_number1] = tv2})
			unifyVar tv_number1 t2 subst
				| occurs subst tv_number1 t2
					=	(False, subst)
				// otherwise
					=	(True, {subst & [tv_number1] = t2})
unify t1 t2=:(TypeVar _) subst
	=	unify t2 t1 subst
unify (TypeFixedVar tv_number1) (TypeFixedVar tv_number2) subst
	=	(tv_number1 == tv_number2, subst)
unify (TypeFixedVar _) _ subst
	=	(False, subst)
unify _ (TypeFixedVar _) subst
	=	(False, subst)
unify (TypeCons cons1) (TypeCons cons2) subst
	| cons1 == cons2
		=	(True, subst)
	// otherwise
		=	(False, subst)
unify (TypeApp t1 arg1) (TypeApp t2 arg2) subst
	# (unified_ts, subst)
		=	unify t1 t2 subst
	| unified_ts
		# (unified_args, subst)
			=	unify arg1 arg2 subst
		| unified_args
			=	(True, subst)
		// otherwise
			=	(False, subst)
	// otherwise
		=	(False, subst)
unify (TypeUnique type1) (TypeUnique type2) subst
	=	unify type1 type2 subst
unify _ _ subst
	=	(False, subst)

FixTypeVars
	:==	True
Don`tFixTypeVars
	:==	False

fresh_type :: Bool UnificationEnvironment TypeCode
										-> (*UnificationEnvironment, TypeCode)
fresh_type fix unify_env (TypeScheme n type)
	# shift
		=	if fix unify_env.fixed (size unify_env.subst)
	# type
		=	shift_type_vars fix shift type
	| fix
		=	({copy_substitutions unify_env & fixed = unify_env.fixed + n}, type)
	// otherwise
		=	(extend_substitutions n unify_env, type)
fresh_type _ unify_env type
	=	(copy_substitutions unify_env, type)

copy_substitutions :: UnificationEnvironment -> *UnificationEnvironment
copy_substitutions unify_env
	=	{unify_env & subst = {e \\ e <-: unify_env.subst}}

extend_substitutions :: Int UnificationEnvironment -> *UnificationEnvironment
extend_substitutions n unify_env
	=	{	unify_env
		&	subst
				=	{	createArray (size unify_env.subst + n) TypeEmpty
					&	[i] = s \\	s <-: unify_env.subst & i <- [0..]
					}
		}

shift_type_vars :: Bool Int TypeCode -> TypeCode
shift_type_vars fix shift t
	| (shift == 0 && not fix) || not r
		=	t
		=	t`
	where
		(r, t`)
			=	shift_vars fix shift t

shift_vars :: Bool Int TypeCode -> (Bool, TypeCode)
shift_vars fix shift (TypeVar i)
	=	(True, (if fix TypeFixedVar TypeVar) (i+shift))
shift_vars _ n tc=:(TypeCons _)
	=	(False, tc)
shift_vars fix n ta=:(TypeApp t arg)
	# (rt, t`)
		=	shift_vars fix n t
	# (ra, arg`)
		=	shift_vars fix n arg
	| rt || ra	
		=	(True, TypeApp t` arg`)
	// otherwise
		=	(False, ta)
shift_vars fix n tu=:(TypeUnique t)
	# (rt, t`)
		=	shift_vars fix n t
	| rt
		=	(True, TypeUnique t`)
	// otherwise
		=	(False, tu)

/*	----------------------------------------------------------------------------
	TypeCodeConstructor
	
	Each type is represented at run-time by a unique data constructor. The
	compiler generates an extra type for each user defined type. For example,
	the type
	
		:: List a = Nil | Cons (List a)

	is accompanied by the type

		:: TC;List a = TC;List (List a)

	This module contains similar definitions for Clean's predefined types.

	The dynamic linker will use the same descriptor for functions of equivalent
	types, so comparing to type constructors can now be done by comparing the
	descriptors of their type functions.
----------------------------------------------------------------------------- */

:: TypeCodeConstructor = E.a: { _tcc_cons :: !a }

descriptorToString :: !Int -> {#Char}
descriptorToString desc
	= code {
	.d 0 1 i
		jsr DtoAC
	.o 1 0
	}

getDescriptor :: !a -> Int
getDescriptor _
	= code {
		pushD_a	0
		pop_a	1
	}

:: TC_Int = TC_Int
:: TC_Char = TC_Char
:: TC_Real = TC_Real
:: TC_Bool = TC_Bool
:: TC_Dynamic = TC_Dynamic
:: TC_File = TC_File
:: TC_World = TC_World

:: TC__Arrow = TC__Arrow

:: TC__List a = TC__List
:: TC__StrictList = TC__StrictList
:: TC__UnboxedList = TC__UnboxedList
:: TC__TailStrictList = TC__TailStrictList
:: TC__StrictTailStrictList = TC__StrictTailStrictList
:: TC__UnboxedTailStrictList = TC__UnboxedTailStrictList

:: TC__Tuple2 = TC__Tuple2
:: TC__Tuple3 = TC__Tuple3
:: TC__Tuple4 = TC__Tuple4
:: TC__Tuple5 = TC__Tuple5
:: TC__Tuple6 = TC__Tuple6
:: TC__Tuple7 = TC__Tuple7
:: TC__Tuple8 = TC__Tuple8
:: TC__Tuple9 = TC__Tuple9
:: TC__Tuple10 = TC__Tuple10
:: TC__Tuple11 = TC__Tuple11
:: TC__Tuple12 = TC__Tuple12
:: TC__Tuple13 = TC__Tuple13
:: TC__Tuple14 = TC__Tuple14
:: TC__Tuple15 = TC__Tuple15
:: TC__Tuple16 = TC__Tuple16
:: TC__Tuple17 = TC__Tuple17
:: TC__Tuple18 = TC__Tuple18
:: TC__Tuple19 = TC__Tuple19
:: TC__Tuple20 = TC__Tuple20
:: TC__Tuple21 = TC__Tuple21
:: TC__Tuple22 = TC__Tuple22
:: TC__Tuple23 = TC__Tuple23
:: TC__Tuple24 = TC__Tuple24
:: TC__Tuple25 = TC__Tuple25
:: TC__Tuple26 = TC__Tuple26
:: TC__Tuple27 = TC__Tuple27
:: TC__Tuple28 = TC__Tuple28
:: TC__Tuple29 = TC__Tuple29
:: TC__Tuple30 = TC__Tuple30
:: TC__Tuple31 = TC__Tuple31
:: TC__Tuple32 = TC__Tuple32

:: TC__LazyArray = TC__LazyArray
:: TC__StrictArray = TC__StrictArray
:: TC__UnboxedArray = TC__UnboxedArray

// compare two type code constructors: compare their descriptors
instance == TypeCodeConstructor where
	(==) a b
		=	getDescriptor a._tcc_cons == getDescriptor b._tcc_cons

// test for predefined type constructor, use the name of the type to
// recognise predefined types (otherwise we'd have to check for all
// predefined constructors)
typeCodeConstructorIsPredefined :: !TypeCodeConstructor -> Bool
typeCodeConstructorIsPredefined tc
		=	prefix tc == "TC_"
		where
			prefix :: !TypeCodeConstructor -> {#Char}
			prefix {_tcc_cons}
				=	descriptorToString (getDescriptor _tcc_cons) % (0, 2)

// convert a type code constructor to a string: get its name and strip the
// first three characters (either "TD;" for user defined types or "TD_"
// for predefined types)
instance toString TypeCodeConstructor where
	toString {_tcc_cons}
		=	descName % (3, size descName - 1)
		where
			descName
				=	descriptorToString (getDescriptor _tcc_cons)

predefinedTypeCodeContructor :: !Int !a -> TypeCodeConstructor
predefinedTypeCodeContructor arity cons
	=	{ _tcc_cons = cons }

getTupleTypeConstructor arity
	:==	tupleTypeConstructors.[arity - 2]

tupleTypeConstructors :: {!TypeCodeConstructor}
tupleTypeConstructors
	=:
	{	predefinedTypeCodeContructor 2 TC__Tuple2
	,	predefinedTypeCodeContructor 3 TC__Tuple3
	,	predefinedTypeCodeContructor 4 TC__Tuple4
	,	predefinedTypeCodeContructor 5 TC__Tuple5
	,	predefinedTypeCodeContructor 6 TC__Tuple6
	,	predefinedTypeCodeContructor 7 TC__Tuple7
	,	predefinedTypeCodeContructor 8 TC__Tuple8
	,	predefinedTypeCodeContructor 9 TC__Tuple9
	,	predefinedTypeCodeContructor 10 TC__Tuple10
	,	predefinedTypeCodeContructor 11 TC__Tuple11
	,	predefinedTypeCodeContructor 12 TC__Tuple12
	,	predefinedTypeCodeContructor 13 TC__Tuple13
	,	predefinedTypeCodeContructor 14 TC__Tuple14
	,	predefinedTypeCodeContructor 15 TC__Tuple15
	,	predefinedTypeCodeContructor 16 TC__Tuple16
	,	predefinedTypeCodeContructor 17 TC__Tuple17
	,	predefinedTypeCodeContructor 18 TC__Tuple18
	,	predefinedTypeCodeContructor 19 TC__Tuple19
	,	predefinedTypeCodeContructor 20 TC__Tuple20
	,	predefinedTypeCodeContructor 21 TC__Tuple21
	,	predefinedTypeCodeContructor 22 TC__Tuple22
	,	predefinedTypeCodeContructor 23 TC__Tuple23
	,	predefinedTypeCodeContructor 24 TC__Tuple24
	,	predefinedTypeCodeContructor 25 TC__Tuple25
	,	predefinedTypeCodeContructor 26 TC__Tuple26
	,	predefinedTypeCodeContructor 27 TC__Tuple27
	,	predefinedTypeCodeContructor 28 TC__Tuple28
	,	predefinedTypeCodeContructor 29 TC__Tuple29
	,	predefinedTypeCodeContructor 30 TC__Tuple30
	,	predefinedTypeCodeContructor 31 TC__Tuple31
	,	predefinedTypeCodeContructor 32 TC__Tuple32
	}

TypeCodeConstructorInt :: TypeCodeConstructor
TypeCodeConstructorInt
	=	predefinedTypeCodeContructor 0 TC_Int

TypeCodeConstructorChar :: TypeCodeConstructor
TypeCodeConstructorChar
	=	predefinedTypeCodeContructor 0 TC_Char

TypeCodeConstructorReal :: TypeCodeConstructor
TypeCodeConstructorReal
	=	predefinedTypeCodeContructor 0 TC_Real

TypeCodeConstructorBool :: TypeCodeConstructor
TypeCodeConstructorBool
	=	predefinedTypeCodeContructor 0 TC_Bool

TypeCodeConstructorDynamic :: TypeCodeConstructor
TypeCodeConstructorDynamic
	=	predefinedTypeCodeContructor 0 TC_Dynamic

TypeCodeConstructorFile :: TypeCodeConstructor
TypeCodeConstructorFile
	=	predefinedTypeCodeContructor 0 TC_File

TypeCodeConstructorWorld :: TypeCodeConstructor
TypeCodeConstructorWorld
	=	predefinedTypeCodeContructor 0 TC_World

TypeCodeConstructor_Arrow :: TypeCodeConstructor
TypeCodeConstructor_Arrow
	=	predefinedTypeCodeContructor 2 TC__Arrow

TypeCodeConstructor_List :: TypeCodeConstructor
TypeCodeConstructor_List
	=	predefinedTypeCodeContructor 1 TC__List

TypeCodeConstructor_StrictList :: TypeCodeConstructor
TypeCodeConstructor_StrictList
	=	predefinedTypeCodeContructor 1 TC__StrictList

TypeCodeConstructor_UnboxedList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedList
	=	predefinedTypeCodeContructor 1 TC__UnboxedList

TypeCodeConstructor_TailStrictList :: TypeCodeConstructor
TypeCodeConstructor_TailStrictList
	=	predefinedTypeCodeContructor 1 TC__TailStrictList

TypeCodeConstructor_StrictTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_StrictTailStrictList
	=	predefinedTypeCodeContructor 1 TC__StrictTailStrictList

TypeCodeConstructor_UnboxedTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedTailStrictList
	=	predefinedTypeCodeContructor 1 TC__UnboxedTailStrictList

TypeCodeConstructor_Tuple :: !Int -> TypeCodeConstructor
TypeCodeConstructor_Tuple arity
	| arity < 2 || arity > 32
		=	fatal "TypeCodeConstructor_Tuple" ("illegal tuple arity (" +++ toString arity +++ ")")
		=	getTupleTypeConstructor arity

TypeCodeConstructor_LazyArray :: TypeCodeConstructor
TypeCodeConstructor_LazyArray
	=	predefinedTypeCodeContructor 1 TC__LazyArray

TypeCodeConstructor_StrictArray :: TypeCodeConstructor
TypeCodeConstructor_StrictArray
	=	predefinedTypeCodeContructor 1 TC__StrictArray

TypeCodeConstructor_UnboxedArray :: TypeCodeConstructor
TypeCodeConstructor_UnboxedArray
	=	predefinedTypeCodeContructor 1 TC__UnboxedArray

_to_TypeCodeConstructor :: !(Bool -> a) -> TypeCodeConstructor
_to_TypeCodeConstructor def
	=	{ _tcc_cons = def }
