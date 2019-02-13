definition module postparse

import StdEnv

import syntax, parse, predef

scanModule :: !ParsedModule ![Ident] !Bool !Bool !*HashTable !*File !SearchPaths (ModTimeFunction *Files) !*Files
	-> (!Bool, !ScannedModule, !IndexRange, ![FunDef], !Optional ScannedModule, ![ScannedModule],!Int,!*HashTable, !*File, !*Files)
