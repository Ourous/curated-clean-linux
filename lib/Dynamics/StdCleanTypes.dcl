definition module StdCleanTypes

from _SystemDynamic import :: TypeCode
from StdOverloaded import class toString

:: CTTypeRHS
	=	CTAlgType [CTConsDef]
	|	CTRecordType [CTFieldDef]
	|	CTSynType
	|	CTPredefined

:: CTConsDef =
	{	cd_cons :: !CTCons
	,	cd_args :: ![TypeCode]
	,	cd_exist :: !Int
	// ,	cd_fixity :: !CTFixity
	}

:: CTFixity
	=	CTFixNone
	|	CTFixLeft
	|	CTFixRight

:: CTFieldDef =
	{	fd_name :: !{#Char}
	,	fd_exist :: !Int
	,	fd_type :: !TypeCode
	}

:: CTTypeDef =
	{	td_name :: !{#Char}
	,	td_arity :: !Int
	,	td_unq :: !Bool
	,	td_rhs :: !CTTypeRHS
	}

:: CTCons
CTToCons :: !a -> CTCons
instance toString CTCons
