/*
	module owner: Ronny Wichers Schreur
*/
definition module backendinterface

import frontend

backEndInterface :: !{#Char} [{#Char}] !ListTypesOption !{#Char} !PredefinedSymbols !FrontEndSyntaxTree !Int
							  !*VarHeap !*TypeVarHeap !*AttrVarHeap !*File !*File
					-> (!Bool,!*VarHeap,!*TypeVarHeap,!*AttrVarHeap,!*File,!*File)
