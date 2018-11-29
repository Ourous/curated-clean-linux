implementation module Data.GenDiff

import StdEnv

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Functor
import Text.GenPrint
import Data.List
from Data.Map import :: Map, findWithDefault, fromList
import Data.Maybe
from Text import class Text(concat), instance Text String, <+

instance zero Diff where zero = {status=Common, value="", children=[]}

instance == DiffStatus
where
	== Common    Common    = True
	== Changed   Changed   = True
	== OnlyRight OnlyRight = True
	== OnlyLeft  OnlyLeft  = True
	== _         _         = False

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
gDiff{|EITHER|} fl fr (LEFT x)  (RIGHT y) = map (setStatus OnlyLeft) (fl x x) ++ map (setStatus OnlyRight) (fr y y)
gDiff{|EITHER|} fl fr (RIGHT x) (LEFT y)  = map (setStatus OnlyLeft) (fr x x) ++ map (setStatus OnlyRight) (fl y y)

eqDiff :: a a -> [Diff] | ==, gPrint{|*|} a
eqDiff x y
| x == y =
	[ {status=Common,  value=printToString x, children=[]}
	]
| otherwise =
	[ {status=OnlyLeft,  value=printToString x, children=[]}
	, {status=OnlyRight, value=printToString y, children=[]}
	]

derive gDiff [], [!], [ !], [!!]

gDiff{|{}|}  fx xs ys = [{diff & value="_Array"}  \\ diff <- gDiff{|*->*|} fx [x \\ x <-: xs] [y \\ y <-: ys]]
gDiff{|{!}|} fx xs ys = [{diff & value="_!Array"} \\ diff <- gDiff{|*->*|} fx [x \\ x <-: xs] [y \\ y <-: ys]]

derive gDiff (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

:: PrState =
	{ indent :: !Int
	, output :: ![String]
	, inlist :: !Bool
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
		Common    -> "\033[0m "
		Changed   -> "\033[0;33m~"
		OnlyRight -> "\033[0;32m>"
		OnlyLeft  -> "\033[0;31m<"

indent :: (State PrState a) -> State PrState ()
indent print =
	modify (\st -> {st & indent=st.indent+1}) >>|
	print >>|
	modify (\st -> {st & indent=st.indent-1})

diffToConsole :: [Diff] -> String
diffToConsole ds = concat (dropSpace (execState (display diff) {indent= -1,output=["\033[0m"],inlist=False}).output)
where
	diff = {status=Common, value="", children=ds}

	display :: Diff -> State PrState ()
	display d =
		gets (\st -> st.inlist) >>= \inlist ->
		case inlist && isMember d.value ["_Cons","_Nil"] of
			True ->
				sequence [display c \\ c <- reverse d.children] >>|
				print "\033[0m"
			False ->
				modify (\st -> {st & inlist=d.value == "_Cons"}) >>|
				sequence [indent (display c) \\ c <- reverse d.children] >>|
				modify (\st -> {st & inlist=inlist}) >>|
				print (findWithDefault d.value d.value constructors) >>|
				newline d.status
	where
		constructors = fromList
			[ ("_Nil", "[]")
			, ("_Cons", "[]")
			: [("_Tuple" <+ i, "(" <+ repeatn (i-1) ',' <+ ")") \\ i <- [2..32]]
			]

	dropSpace :: [String] -> [String]
	dropSpace org=:["\033[0m ":ss] = case dropWhile ((==) "") ss of
		["\n":ss] -> dropSpace ss
		_         -> org
	dropSpace ["\n":ss] = dropSpace ss
	dropSpace ss = ss
