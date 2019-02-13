definition module compare_types

import syntax, compare_constructor

::	CompareValue :== Int
Smaller :== -1
Greater	:== 1
Equal	:== 0

class (=<) infix 4 a :: !a !a -> CompareValue

instance =< Int, Expression, {# Char}, Ident, [a] | =< a, BasicType

instance =< Type, SymbIdent

instance == BasicType, TypeVar, AttributeVar, AttrInequality, TypeSymbIdent, DefinedSymbol, 
			TypeContext, BasicValue, FunKind, (Global a) | == a, Priority, Assoc, Type, 
			ConsVariable, SignClassification, TypeCons, TCClass

instance < MemberDef

smallerOrEqual :: !Type !Type -> CompareValue
