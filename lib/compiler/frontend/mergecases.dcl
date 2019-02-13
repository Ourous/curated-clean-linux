/*
	module owner: Ronny Wichers Schreur
*/
definition module mergecases

import syntax, checksupport

mergeCases :: !(!Expression, !Position) ![(!Expression, !Position)] !*VarHeap !*ExpressionHeap !*ErrorAdmin
			-> *(!(!Expression, !Position), !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
