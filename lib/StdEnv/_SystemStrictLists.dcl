definition module _SystemStrictLists;

class List .l e where {
	_cons :: .e u:(l .e) -> u:(l .e);
	_decons :: !u:(l .e) -> (.e,u:(l .e));
};

class UList e where {
	_cons_u :: !.e u:[#.e] -> u:[#.e];
	_decons_u :: !u:[#.e] -> (!.e,u:[#.e]);
};

class UTSList e where {
	_cons_uts :: !.e !u:[#.e!] -> u:[#.e!];
	_decons_uts :: !u:[#.e!] -> (!.e,!u:[#.e!]);
};

_nil :: u:(l .e) | List l e , [u<=e];
_nil_u :: .[#.e] | UList e;
_nil_uts :: .[#.e!] | UTSList e;

instance List [] a;
instance List [!] a;
instance List [ !] a;
instance List [!!] a;

instance List [#] a | UList a;
instance List [#!] a | UTSList a;

instance UList a;

instance UList Int;
instance UList Real;
instance UList Char;
instance UList Bool;
instance UList File;
instance UList {.a};
instance UList {!.a};
instance UList {#.a};

instance UTSList a;

instance UTSList Int;
instance UTSList Real;
instance UTSList Char;
instance UTSList Bool;
instance UTSList File;
instance UTSList {.a};
instance UTSList {!.a};
instance UTSList {#.a};
