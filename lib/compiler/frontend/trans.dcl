definition module trans

import StdEnv
import syntax,classify,predef

:: FusionOptions = { compile_with_fusion :: !Bool, generic_fusion :: !Bool, strip_unused :: !Bool }

transformGroups :: !CleanupInfo !Int !Int !Int !Int !*{!Component} !*{!ConsClasses}
					!{#CommonDefs} !{#{#FunType}} !*TypeDefInfos !{#DclModule} !FusionOptions
											 !*{#FunDef} !*ImportedTypes !*VarHeap !*TypeHeaps !*ExpressionHeap !*File !*PredefinedSymbols
	-> (!*{!Component},!ImportedConstructors,!*{#FunDef},!*ImportedTypes,!*VarHeap,!*TypeHeaps,!*ExpressionHeap,!*File,!*PredefinedSymbols)
