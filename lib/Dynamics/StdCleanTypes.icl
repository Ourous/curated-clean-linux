implementation module StdCleanTypes

from _SystemDynamic import :: TypeCode
from StdOverloaded import class toString

:: CTCons =
	E.a:
	{	cons :: !a
	}

cast :: !.a -> .b
cast a
	= code
		{
			pop_a 0
		}

CTToCons :: !a -> CTCons
CTToCons a
	=	{	cons = cast a
		}
	
getDescriptor :: !a -> Int
getDescriptor _
	=	code
	{
			pushD_a	0
			pop_a	1
	}

instance toString CTCons where
	toString {cons}
		=	descriptorIDtoString (getDescriptor cons)
//	where
descriptorIDtoString :: !Int -> {#Char}
descriptorIDtoString id
	=	code
	{
	.d 0 1 i
		jsr DtoAC
	.o 1 0
	}
