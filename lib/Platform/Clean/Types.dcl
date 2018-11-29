definition module Clean.Types

/**
 * Definitions of Clean types.
 */

from StdOverloaded import class ==
from Data.Maybe import :: Maybe

/**
 * The type of a function.
 */
:: Type
	= Type !String ![Type]              //* Concrete type with arguments
	| Func ![Type] !Type !TypeContext   //* A function with parameters, a result and class context (no uniqueness unequalities yet)
	| Var !TypeVar                      //* A type variable
	| Cons !TypeVar ![Type]             //* A constructor variable with arguments
	| Uniq !Type                        //* A unique type
	| Forall ![Type] !Type !TypeContext //* Universally quantified variables
	| Arrow !(Maybe Type)               //* `(->)` and `((->) t)`
	| Strict !Type                      //* A type annotated for strictness

/**
 * A type variable.
 * @representation The name of the variable
 */
:: TypeVar :== String

/**
 * An assignment of a type to a type variable.
 * @representation A tuple of the variable and the type
 */
:: TVAssignment :== (!TypeVar, !Type)

/**
 * A unifier of a left type and a right type.
 */
:: Unifier
	= { assignments   :: ![UnifyingAssignment] //* The assignments
	  , used_synonyms :: ![TypeDef]            //* Type synonyms used in the unification
	  }

:: UnifyingAssignment
	= RightToLeft !TVAssignment
	| LeftToRight !TVAssignment

/**
 * A type context.
 */
:: TypeContext :== [TypeRestriction]

/**
 * A restriction on a type.
 */
:: TypeRestriction
	= Instance !String ![Type]
	| Derivation !String !Type

/**
 * The kind of a Clean type.
 */
:: Kind
	= KStar
	| KArrow infixr 1 !Kind !Kind

/**
 * A Clean type definition.
 */
:: TypeDef
	= { td_name :: !String     //* The name of the type
	  , td_uniq :: !Bool       //* Whether the type is unique
	  , td_args :: ![Type]     //* Var or Uniq Var; arguments
	  , td_rhs  :: !TypeDefRhs //* The right-hand side
	  }

/**
 * The right-hand side of a type definition.
 */
:: TypeDefRhs
	= TDRCons !Bool ![Constructor]
		//* A list of constructors. The boolean indicates if the type is extensible
	| TDRNewType !Constructor         //* A newtype
	| TDRMoreConses ![Constructor]    //* More constructors for an extensible ADT
	| TDRRecord !String ![TypeVar] ![RecordField]
		//* A record with its internal identifier, existentially quantified variables and fields
	| TDRSynonym !Type                //* A type synonym
	| TDRAbstract !(Maybe TypeDefRhs) //* An abstract type
	| TDRAbstractSynonym !Type        //* An abstract type synonym

/**
 * The constructor of an algebraic data type.
 */
:: Constructor
	= { cons_name     :: !String         //* The name of the constructor
	  , cons_args     :: ![Type]         //* The arguments of the constructor
	  , cons_exi_vars :: ![TypeVar]      //* Existentially quantified variables
	  , cons_context  :: !TypeContext    //* The class context of the constructor
	  , cons_priority :: !Maybe Priority //* Priority, if this is an infix constructor
	  }

/**
 * Priority of an infix function.
 */
:: Priority
	= LeftAssoc !Int  //* Left-associative operator with precedence
	| RightAssoc !Int //* Right-associative operator with precedence
	| NoAssoc !Int    //* Infix operator with precedence but no explicit associativity

/**
 * A record field.
 */
:: RecordField
	= { rf_name :: !String //* The name of the field
	  , rf_type :: !Type   //* The type of the field
	  }

instance == Type
instance == TypeRestriction

class toType a :: !a -> Type
class toTypeVar a :: !a -> TypeVar

class toTypeContext a :: !a -> TypeContext

class toTypeDef a :: !a -> TypeDef
class toTypeDefRhs a :: !a -> TypeDefRhs
class toConstructor a :: !a -> Constructor
class toMaybePriority a :: !a -> Maybe Priority
class toRecordField a :: !a -> RecordField

/**
 * A list of subtypes of a type. For functions this includes parameters and the
 * results, but not the class context or for `a b -> c` the subtype `b -> c`.
 * The resulting list does not include duplicates.
 */
subtypes :: !Type -> [Type]

/**
 * All type restrictions that occur in this type, also further down the data
 * structure.
 */
allRestrictions :: !Type -> [TypeRestriction]

/**
 * A list of type variables used in a type.
 */
allVars :: (Type -> [TypeVar])

/**
 * A list of all the variables that are quantified universally in a (sub)type.
 */
allUniversalVars :: !Type -> [TypeVar]

/**
 * `True` iff a type is a `Var`.
 */
isVar :: !Type -> Bool

/**
 * The type variable of a `Var` type.
 * Generates a run-time error if the type is not a `Var`.
 */
fromVar :: !Type -> TypeVar

/**
 * The type variable of a `Var` or `Cons` type, or a unique version of those.
 * Generates a run-time error for other types.
 */
fromVarLenient :: !Type -> TypeVar

/**
 * `True` iff a type is a `Cons`.
 */
isCons :: !Type -> Bool

/**
 * Check for a `Cons` with a certain name.
 *
 * @param The name to match
 * @param The type
 * @result True iff the type is a Cons and its name matches the first parameter
 */
isCons` :: TypeVar !Type -> Bool

/**
 * Check for a Var or Cons with a certain name.
 *
 * @param The name to match
 * @param The type
 * @result `True` iff the type is a `Var` or `Cons` and its name matches the
 *   first parameter
 */
isVarOrCons` :: TypeVar !Type -> Bool

/**
 * Check if a type is of the `Type` constructor.
 */
isType :: !Type -> Bool

/**
 * Check if a type is of the `Func` constructor.
 */
isFunc :: !Type -> Bool

/**
 * Check if a type is unique.
 */
isUniq :: !Type -> Bool

/**
 * Check if a type is of the `Forall` constructor.
 */
isForall :: !Type -> Bool

/**
 * Remove the `Forall` constructor from a type.
 * Generates a run-time error if the type is of another constructor.
 */
fromForall :: !Type -> Type

/**
 * Check if a type is an arrow.
 */
isArrow :: !Type -> Bool

/**
 * Remove the `Arrow` constructor from a type.
 * Generates a run-time error if the type is of another constructor.
 */
fromArrow :: !Type -> Maybe Type

/**
 * Get the {{`TVAssignment`}} from a {{`UnifyingAssignment`}}.
 */
fromUnifyingAssignment :: !UnifyingAssignment -> TVAssignment

/**
 * The arity of {{`Type`}}, {{`Func`}}, {{`Var`}} and {{`Cons`}} types.
 * Generates a run-time error for {{`Uniq`}}, {{`Forall`}} and {{`Arrow`}}.
 */
arity :: !Type -> Int

/**
 * Remove all type contexts (including those of universally quantified types)
 * from a type.
 */
removeTypeContexts :: !Type -> Type

/**
 * The constructors of an algebraic data type, as functions.
 *
 * @param The type definition
 * @result A list of tuples of the name, type and infix priority of the constructors
 */
constructorsToFunctions :: !TypeDef -> [(String,Type,Maybe Priority)]

/**
 * The record fields of an algebraic data type, as functions.
 *
 * @param The type definition
 * @result A list of tuples of the name and type of the record fields
 */
recordsToFunctions :: !TypeDef -> [(String,Type)]

/**
 * Wrapper around the {{`td_name`}} field of the {{`TypeDef`}} record.
 */
td_name :: !TypeDef -> String

/**
 * Wrapper around the {{`td_uniq`}} field of the {{`TypeDef`}} record.
 */
td_uniq :: !TypeDef -> Bool

/**
 * Wrapper around the {{`td_rhs`}} field of the {{`TypeDef`}} record.
 */
td_rhs :: !TypeDef -> TypeDefRhs

/**
 * Wrapper to create a {{`TypeDef`}} record.
 */
typedef :: !String !Bool ![Type] !TypeDefRhs -> TypeDef

/**
 * Wrapper to create a {{`Constructor`}} record.
 */
constructor :: !String ![Type] ![TypeVar] !TypeContext !(Maybe Priority) -> Constructor

/**
 * Wrapper to create a {{`RecordField`}} record.
 */
recordfield :: !String !Type -> RecordField

/**
 * Alternative {{`removeDup`}} for {{`TypeDefs`}}, only considering the
 * {{`td_name`}} field.
 */
removeDupTypedefs :: ![TypeDef] -> [TypeDef]

/**
 * All context restrictions in a {{`TypeDefRhs`}}.
 */
typeRhsRestrictions :: !TypeDefRhs -> [TypeRestriction]
