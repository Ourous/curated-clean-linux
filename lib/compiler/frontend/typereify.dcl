definition module typereify

from general import ::Optional
from syntax import
	::Ident, ::FunDef, ::IndexRange, ::TypeHeaps,
	::SymbolTable, ::SymbolTableEntry, ::Heap,
	::CommonDefsR,::DclInstanceMemberTypeAndFunction,
	::DclModule, ::CommonDefs, ::CheckedTypeDef, ::TypeDef, ::TypeRhs, ::ClassDef, ::VarHeap, ::VarInfo
from predef import
	::PredefinedSymbols, ::PredefinedSymbol

addDclTypeFunctions :: !Int !*{#DclModule} !*PredefinedSymbols !*VarHeap !*SymbolTable
						-> (!*{#DclModule},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)

addIclTypeFunctions :: !Int !Int !*{#FunDef} !*{#CheckedTypeDef} !*{#ClassDef} !*PredefinedSymbols !*VarHeap !*SymbolTable
				 -> (!IndexRange,!*{#FunDef},!*{#CheckedTypeDef},!*{#ClassDef},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)

buildTypeFunctions :: !Int !*{#FunDef} !{#CommonDefs} !*PredefinedSymbols !*VarHeap !*TypeHeaps
									  -> (!*{#FunDef},!*PredefinedSymbols,!*VarHeap,!*TypeHeaps)
