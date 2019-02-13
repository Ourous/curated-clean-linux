/*
	module owner: Ronny Wichers Schreur
*/
module cocl

import coclmain

Start :: *World -> *World
Start world
	=	coclMain [] world
