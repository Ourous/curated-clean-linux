definition module Clean.PrettyPrint.Definition

from syntax import :: ParsedDefinition, :: Type, :: AType, :: ATypeVar, :: TypeContext

from Clean.PrettyPrint.Util import class print

instance print ParsedDefinition, Type, AType, ATypeVar, TypeContext
