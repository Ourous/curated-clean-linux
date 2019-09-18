definition module _SystemEnumStrict;

from StdClass import class Enum (..), class IncDec,class +(+),class -(-),class zero(zero),class one(one),class Ord (..),class < (<),<=,inc;
from StdBool import not;

import _SystemStrictLists;

_from_s  :: !a -> .[!a]  | IncDec a			special { a=Int };
_from_ts :: !a -> .[a!]  | IncDec a			special { a=Int };
_from_sts:: !a -> .[!a!] | IncDec a			special { a=Int };
_from_u  :: !a -> .[#a]  | IncDec,UList a	special { a=Int };
_from_uts:: !a -> .[#a!] | IncDec,UTSList a	special { a=Int };
_from_o  :: a -> .(l a) | IncDec a & List l a
	special {
		l=[],a=Int;
		l=[!],a=Int;
		l=[ !],a=Int;
		l=[!!],a=Int;
		l=[#],a=Int;
		l=[#!],a=Int;
	};

_from_to_s   :: !a !a -> .[!a]  | Enum a			special { a=Int };
_from_to_ts  :: !a !a -> .[a!]  | Enum a			special { a=Int };
_from_to_sts :: !a !a -> .[!a!] | Enum a			special { a=Int };
_from_to_u   :: !a !a -> .[#a]  | Enum,UList a		special { a=Int };
_from_to_uts :: !a !a -> .[#a!] | Enum,UTSList a	special { a=Int };
_from_to_o   :: !a !a -> .(l a) | Enum a & List l a
	special {
		l=[],a=Int;
		l=[!],a=Int;
		l=[ !],a=Int;
		l=[!!],a=Int;
		l=[#],a=Int;
		l=[#!],a=Int;
	};

_from_then_s   :: !a a -> .[!a]   | Enum a			special { a=Int };
_from_then_ts  :: !a !a -> .[a!]  | Enum a			special { a=Int };
_from_then_sts :: !a !a -> .[!a!] | Enum a			special { a=Int };
_from_then_u   :: !a a -> .[#a]   | Enum,UList a	special { a=Int };
_from_then_uts :: !a !a -> .[#a!] | Enum,UTSList a	special { a=Int };
_from_then_o   :: a a -> .(l a)   | Enum a & List l a
	special {
		l=[],a=Int;
		l=[!],a=Int;
		l=[ !],a=Int;
		l=[!!],a=Int;
		l=[#],a=Int;
		l=[#!],a=Int;
	};

_from_then_to_s   :: !a !a !a -> .[!a]  | Enum a			special { a=Int };
_from_then_to_ts  :: !a !a !a -> .[a!]  | Enum a			special { a=Int };
_from_then_to_sts :: !a !a !a -> .[!a!] | Enum a			special { a=Int };
_from_then_to_u   :: !a !a !a -> .[#a]  | Enum,UList a		special { a=Int };
_from_then_to_uts :: !a !a !a -> .[#a!] | Enum,UTSList a	special { a=Int };
_from_then_to_o   :: !a !a !a -> .(l a) | Enum a & List l a
	special {
		l=[],a=Int;
		l=[!],a=Int;
		l=[ !],a=Int;
		l=[!!],a=Int;
		l=[#],a=Int;
		l=[#!],a=Int;
	};
