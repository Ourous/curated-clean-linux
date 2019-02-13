definition module refmark

import syntax, checksupport, unitype

makeSharedReferencesNonUnique :: ![Int] !u:{# FunDef} !*Coercions !w:{! Type} !v:TypeDefInfos !*VarHeap !*ExpressionHeap !*ErrorAdmin
	-> (!u:{# FunDef}, !*Coercions, !w:{! Type},  !v:TypeDefInfos, !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
