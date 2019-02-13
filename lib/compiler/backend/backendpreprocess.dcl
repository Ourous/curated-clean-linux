/*
	module owner: Ronny Wichers Schreur
*/
definition module backendpreprocess

import checksupport

// assign sequence numbers to all variables in the syntax tree

backEndPreprocess :: !Ident ![Index] !IclModule !*VarHeap -> *VarHeap
