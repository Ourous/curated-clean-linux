definition module _SystemDynamic

from StdOverloaded import class toString, class ==

// dynamics consist of a value with a type code
:: DynamicTemp	= E.a:
	{	_value	:: a
	,	_type	:: TypeCode
	}

// representation of type codes
:: TypeCode
	=	TypeScheme !Int TypeCode		// Int = # variables
	|	TypeVar !Int					// Int = variable number
	|	TypeCons !TypeCodeConstructor
	|	TypeApp !TypeCode !TypeCode
	|	TypeUnique !TypeCode
	|	_TypeFixedVar !Int				// used internally between compiler and
										// unification algorithm
	|	_TypeEmpty						// used internally during unification

instance toString TypeCode
instance == TypeCode

// representation of type constructor codes
:: TypeCodeConstructor = E.a: { _tcc_cons :: !a }

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

:: TC__Unit = TC__Unit

instance toString TypeCodeConstructor
instance == TypeCodeConstructor
typeCodeOfDynamic :: !Dynamic -> TypeCode

// type code constructors for predefined types
TypeCodeConstructorInt :: TypeCodeConstructor
TypeCodeConstructorChar :: TypeCodeConstructor
TypeCodeConstructorReal :: TypeCodeConstructor
TypeCodeConstructorBool :: TypeCodeConstructor
TypeCodeConstructorDynamic :: TypeCodeConstructor
TypeCodeConstructorFile :: TypeCodeConstructor
TypeCodeConstructorWorld :: TypeCodeConstructor
TypeCodeConstructor_Arrow :: TypeCodeConstructor
TypeCodeConstructor_List :: TypeCodeConstructor
TypeCodeConstructor_StrictList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedList :: TypeCodeConstructor
TypeCodeConstructor_TailStrictList :: TypeCodeConstructor
TypeCodeConstructor_StrictTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_Tuple :: !Int -> TypeCodeConstructor	// arity: 2 .. 32
TypeCodeConstructor_LazyArray :: TypeCodeConstructor
TypeCodeConstructor_StrictArray :: TypeCodeConstructor
TypeCodeConstructor_UnboxedArray :: TypeCodeConstructor

// unification functions

:: _UnificationEnvironment

_initial_unification_environment :: !Int !Int -> *_UnificationEnvironment
_bind_global_type_pattern_var :: !TypeCode !TypeCode !*_UnificationEnvironment -> *_UnificationEnvironment
// first type arg is the actual type, second is the type pattern
_unify :: !_UnificationEnvironment !TypeCode !TypeCode -> (!Bool, _UnificationEnvironment)
_normalise :: !_UnificationEnvironment !TypeCode -> TypeCode

typeCodeConstructorIsPredefined :: !TypeCodeConstructor -> Bool

_to_TypeCodeConstructor :: !(Bool -> a) -> TypeCodeConstructor
