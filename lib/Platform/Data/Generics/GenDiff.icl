implementation module Data.Generics.GenDiff

import StdBool
from StdFunc import flip, o
import StdGeneric
import StdInt
import StdList
import StdString

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Generics.GenPrint
import Data.List
import Data.Maybe
from Text import class Text(concat), instance Text String

instance zero Diff where zero = {status=Common, value="", children=[]}

instance == DiffStatus
where
	== Common  Common  = True
	== Changed Changed = True
	== Added   Added   = True
	== Removed Removed = True
	== _       _       = False

setStatus :: DiffStatus Diff -> Diff
setStatus s d = {d & status=s, children=map (setStatus s) d.children}

generic gDiff a :: a a -> [Diff]
gDiff{|Int|} x y = eqDiff x y
gDiff{|Char|} x y = eqDiff x y
gDiff{|Bool|} x y = eqDiff x y
gDiff{|Real|} x y = eqDiff x y
gDiff{|String|} x y = eqDiff x y
gDiff{|UNIT|} UNIT UNIT = []
gDiff{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 ++ fy y1 y2
gDiff{|OBJECT|} fx (OBJECT x) (OBJECT y)  = fx x y
gDiff{|CONS of d|} fx (CONS x) (CONS y)   = let children = fx x y in
	[{ status   = if (all (\d -> d.status=:Common) children) Common Changed
	 , value    = d.gcd_name
	 , children = children
	 }]
gDiff{|RECORD of d|} fx (RECORD x) (RECORD y) = let children = fx x y in
	[{ status   = if (all (\d -> d.status=:Common) children) Common Changed
	 , value    = d.grd_name
	 , children = children
	 }]
gDiff{|FIELD of d|} fx (FIELD x) (FIELD y) = let children = fx x y in
	[{ status   = if (all (\d -> d.status=:Common) children) Common Changed
	 , value    = d.gfd_name +++ "="
	 , children = children
	 }]
gDiff{|EITHER|} fl fr (LEFT x)  (LEFT y)  = fl x y
gDiff{|EITHER|} fl fr (RIGHT x) (RIGHT y) = fr x y
gDiff{|EITHER|} fl fr (LEFT x)  (RIGHT y) = map (setStatus Removed) (fl x x) ++ map (setStatus Added) (fr y y)
gDiff{|EITHER|} fl fr (RIGHT x) (LEFT y)  = map (setStatus Removed) (fr x x) ++ map (setStatus Added) (fl y y)

eqDiff :: a a -> [Diff] | ==, gPrint{|*|} a
eqDiff x y
| x == y =
	[ {status=Common,  value=printToString x, children=[]}
	]
| otherwise =
	[ {status=Removed, value=printToString x, children=[]}
	, {status=Added,   value=printToString y, children=[]}
	]

derive gDiff [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

:: PrState =
	{ indent :: !Int
	, output :: ![String]
	}

print :: a -> State PrState () | toString a
print s = modify \st -> {st & output=[toString s:st.output]}

newline :: DiffStatus -> State PrState ()
newline ds =
	gets (\s -> s.indent) >>=
	print o flip repeatn '\t' >>|
	print head >>|
	print "\n"
where
	head = case ds of
		Common  -> " "
		Changed -> "\033[0;33m~"
		Added   -> "\033[0;32m+"
		Removed -> "\033[0;31m-"

indent :: DiffStatus (State PrState a) -> State PrState ()
indent ds st =
	modify (\st -> {st & indent=st.indent+1}) >>|
	st >>|
	newline ds >>|
	modify (\st -> {st & indent=st.indent-1})

diffToConsole :: [Diff] -> String
diffToConsole ds = concat (dropWhile isSpace (execState (display False diff) {indent= -1,output=[]}).output)
where
	diff = {status=Common, value="", children=ds}

	display :: Bool Diff -> State PrState ()
	display p d =
		print reset >>|
		if p` (print ")" >>| newline d.status) (pure ()) >>|
		print color >>|
		sequence [indent c.status (display True c) \\ c <- reverse d.children] >>|
		print reset >>|
		print d.value >>|
		print (if p` "(" "") >>|
		print color
	where
		color = case d.status of
			Common  -> reset
			Changed -> "\033[0;33m"
			Added   -> "\033[0;32m"
			Removed -> "\033[0;31m"
		reset = "\033[0m"
		p` = p && not (isEmpty d.children)

	isSpace :: String -> Bool
	isSpace "\033[0m" = True
	isSpace "\n" = True
	isSpace "" = True
	isSpace _ = False
