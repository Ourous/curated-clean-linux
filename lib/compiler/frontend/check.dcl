definition module check

import syntax, transform, checksupport, typesupport, predef

checkModule :: !ScannedModule !IndexRange ![FunDef] !Bool !Bool !Int !(Optional ScannedModule) ![ScannedModule] !{#DclModule} !*{#*{#FunDef}} !*PredefinedSymbols !*SymbolTable !*File !*Heaps
	-> (!Bool, *IclModule, *{# DclModule}, *{! Group}, !*{#*{#FunDef}},!Int, !*Heaps, !*PredefinedSymbols, !*SymbolTable, *File, [String])

checkFunctions :: !Index !Level !Index !Index !Int !*{#FunDef} !*ExpressionInfo !*Heaps !*CheckState
											   -> (!*{#FunDef},!*ExpressionInfo,!*Heaps,!*CheckState)

checkDclMacros :: !Index !Level !Index !Index !*ExpressionInfo !*Heaps !*CheckState
										  -> (!*ExpressionInfo,!*Heaps,!*CheckState)

checkForeignExportedFunctionTypes :: ![ForeignExport] !*ErrorAdmin !p:PredefinedSymbols !*{#FunDef}
												  -> (!*ErrorAdmin,!p:PredefinedSymbols,!*{#FunDef})

determineTypeOfMemberInstance :: !SymbolType ![TypeVar] !InstanceType !Specials !*TypeHeaps !u:(Optional (v:{#DclModule}, w:{#CheckedTypeDef}, Index)) !*ErrorAdmin
												 -> (!SymbolType, !FunSpecials, !*TypeHeaps,!u: Optional (v:{#DclModule}, w:{#CheckedTypeDef}), !*ErrorAdmin)

arrayFunOffsetToPD_IndexTable :: !w:{# MemberDef} !v:{# PredefinedSymbol} -> (!{# Index}, !x:{#MemberDef}, !v:{#PredefinedSymbol}) , [w<=x]

makeElemTypeOfArrayFunctionStrict :: !SymbolType !Index !{# Index} -> SymbolType

initializeContextVariables :: ![TypeContext] !*VarHeap ->  (![TypeContext], !*VarHeap)
