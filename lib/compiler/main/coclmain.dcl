/*
	module owner: Ronny Wichers Schreur
*/
definition module coclmain

/*
	The coclmain library

	includes
		compile
		backend (needs dynamic library backend.dll)
		ArgEnv
		Version
		set_return_code

	uses
		StdEnv
		compiler
*/

coclMain :: ![{#Char}] !*World -> *World
// testArgs world
