implementation module _SystemEnumStrict;

import _SystemStrictLists;

from StdClass import class Enum (..), class IncDec (..), inc, dec, class Ord (..),class + (..),class - (..),class < (..),class zero (..),class one (..),<=,>=;
from StdBool import not;

import StdInt;

_from_s :: !a -> .[!a] | IncDec a;
_from_s n = [|n : _from_s (inc n)];

_from_ts :: !a -> .[a!] | IncDec a;
_from_ts n = [|n : _from_ts (inc n)];

_from_sts :: !a -> .[!a!] | IncDec a;
_from_sts n = [|n : _from_sts (inc n)];

_from_u :: !a -> .[#a] | IncDec,UList a;
_from_u n = [|n : _from_u (inc n)];

_from_uts :: !a -> .[#a!] | IncDec,UTSList a;
_from_uts n = [|n : _from_uts (inc n)];

_from_o  :: a -> .(l a) | IncDec a & List l a;
_from_o n = [|n : _from_o (inc n)];


_from_to_s :: !a !a -> .[!a] | Enum a;
_from_to_s n e
	| n <= e
		= [|n : _from_to_s (inc n) e];
		= [|];

_from_to_ts :: !a !a -> .[a!] | Enum a;
_from_to_ts n e
	| n <= e
		= [|n : _from_to_ts (inc n) e];
		= [|];

_from_to_sts :: !a !a -> .[!a!] | Enum a;
_from_to_sts n e
	| n <= e
		= [|n : _from_to_sts (inc n) e];
		= [|];

_from_to_u :: !a !a -> .[#a] | Enum,UList a;
_from_to_u n e
	| n <= e
		= [|n : _from_to_u (inc n) e];
		= [|];

_from_to_uts :: !a !a -> .[#a!] | Enum,UTSList a;
_from_to_uts n e
	| n <= e
		= [|n : _from_to_uts (inc n) e];
		= [|];

_from_to_o :: !a !a -> .(l a) | Enum a & List l a;
_from_to_o n e
	| n <= e
		= [|n : _from_to_o (inc n) e];
		= [|];


_from_by n s = [|n : _from_by (n+s) s];

_from_then_s :: !a a -> .[!a] | Enum a;
_from_then_s n1 n2 = [|n1 : _from_by n2 (n2-n1)];

_from_then_ts :: !a !a -> .[a!] | Enum a;
_from_then_ts n1 n2 = [|n1 : _from_by n2 (n2-n1)];

_from_then_sts :: !a !a -> .[!a!] | Enum a;
_from_then_sts n1 n2 = [|n1 : _from_by n2 (n2-n1)];

_from_then_u :: !a a -> .[#a] | Enum,UList a;
_from_then_u n1 n2 = [|n1 : _from_by n2 (n2-n1)];

_from_then_uts :: !a !a -> .[#a!] | Enum,UTSList a;
_from_then_uts n1 n2 = [|n1 : _from_by n2 (n2-n1)];

_from_then_o :: a a -> .(l a) | Enum a & List l a;
_from_then_o n1 n2 = [|n1 : _from_by n2 (n2-n1)];


_from_by_to :: !a !a !a -> .(l a) | Enum a & List l a;
_from_by_to n s e
	| n<=e
		= [|n : _from_by_to (n+s) s e];
		= [|];

_from_by_down_to :: !a !a !a -> .(l a) | Enum a & List l a;
_from_by_down_to n s e
	| n>=e
		= [|n : _from_by_down_to (n+s) s e];
		= [|];

_from_then_to_s :: !a !a !a -> .[!a] | Enum a;
_from_then_to_s n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;

_from_then_to_ts :: !a !a !a -> .[a!] | Enum a;
_from_then_to_ts n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;

_from_then_to_sts :: !a !a !a -> .[!a!] | Enum a;
_from_then_to_sts n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;

_from_then_to_u :: !a !a !a -> .[#a] | Enum,UList a;
_from_then_to_u n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;

_from_then_to_uts :: !a !a !a -> .[#a!] | Enum,UTSList a;
_from_then_to_uts n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;

_from_then_to_o :: !a !a !a -> .(l a) | Enum a & List l a;
_from_then_to_o n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e;
		= _from_by_down_to n1 (n2-n1) e;
