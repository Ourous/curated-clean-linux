definition module Clean.Types.CoclTransform

/**
 * Functions to
 *
 * - transform types in the Clean compiler into types in {{`Clean.Types`}};
 * - derive types for very simple expressions from the compiler AST.
 */

from Clean.Types import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField,
	class toTypeContext, class toMaybePriority
import qualified Clean.Types as T
from Data.Maybe import :: Maybe

// Clean compiler frontend
import qualified syntax

instance toType 'syntax'.SymbolType
instance toType 'syntax'.Type

instance toTypeVar 'syntax'.TypeVar

instance toTypeContext ['syntax'.TypeContext]
instance toTypeContext 'syntax'.TypeContext

instance toTypeDef 'syntax'.ParsedTypeDef
instance toTypeDefRhs 'syntax'.RhsDefsOfType
instance toConstructor 'syntax'.ParsedConstructor
instance toRecordField 'syntax'.ParsedSelector
instance toMaybePriority 'syntax'.Priority

pdType :: !'syntax'.ParsedDefinition -> Maybe 'T'.Type
