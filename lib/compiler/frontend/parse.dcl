definition module parse

import syntax, hashtable, scanner, predef

::	*ParseErrorAdmin = 
	{	pea_file	:: !*File
	,	pea_ok		:: !Bool
	}

cWantIclFile :== True	
cWantDclFile :== False	

wantModule :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
	-> (!Bool,!Bool,!ParsedModule, !*HashTable, !*File, !*Files)

moduleCouldNotBeImportedError :: !Bool !Ident !Position !*File -> *File
