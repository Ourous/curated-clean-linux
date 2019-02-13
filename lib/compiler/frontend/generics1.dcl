definition module generics1

import checksupport
from transform import ::Group

convertGenerics :: 
		!Int 
		!NumberSet
		!{#CommonDefs} 
		!{!Group} 
		!*{# FunDef} 
		!*TypeDefInfos 
		!*Heaps 
		!*HashTable 
		!*PredefinedSymbols 
		!u:{# DclModule}
		!*{#*{#FunDef}}
		!*ErrorAdmin
	-> (  !{#CommonDefs}
		, !{!Group}
		, !*{# FunDef}
		, !*TypeDefInfos
		, !*Heaps
		, !*HashTable
		, !*PredefinedSymbols
		, !u:{# DclModule}
		, !*{#*{#FunDef}}
		, !*ErrorAdmin
		)
