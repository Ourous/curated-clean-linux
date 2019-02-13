definition module coredump

import transform

dumpCore :: !*{! Group} Int [IndexRange] !.IclModule !.DclModule !*{# FunDef} {!ConsClasses} .Int .Int *Files -> *(!*{! Group},!*{# FunDef},!*Files)
