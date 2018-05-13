implementation module Text.Terminal.VT100

import _SystemArray
from StdFunc import o, flip
from Data.Func import $
from Text import class Text(split,join,concat), instance Text String

from Data.Map import :: Map
import qualified Data.Map as DM

import StdList, StdBool, StdOverloaded
import StdDebug

import Data.List
import Data.Tuple
import Control.Applicative

import Text.HTML

instance zero VT100Settings where
	zero =
		{ cols = 80
		, rows = 24
		, tabstop = 8
		, cssmap = 'DM'.fromList
			[(4, ("text-decoration", "underline"))
			,(30, ("color", "black"))
			,(31, ("color", "red"))
			,(32, ("color", "green"))
			,(33, ("color", "yellow"))
			,(34, ("color", "blue"))
			,(35, ("color", "magenta"))
			,(36, ("color", "cyan"))
			,(37, ("color", "white"))
			,(39, ("color", "black"))
			,(40, ("background-color", "black"))
			,(41, ("background-color", "red"))
			,(42, ("background-color", "green"))
			,(43, ("background-color", "yellow"))
			,(44, ("background-color", "blue"))
			,(45, ("background-color", "magenta"))
			,(46, ("background-color", "cyan"))
			,(47, ("background-color", "white"))
			,(49, ("background-color", "white"))
			]
		}

:: Cell :== (Map String String, Char)
:: *Screen :== *{*{Cell}}

vt100render :: VT100Settings -> (String -> HtmlTag)
vt100render s = TtTag [] o render o (\c->rvt {createArray (s.cols+1) ('DM'.newMap, ' ')\\_<-[0..s.rows+1]} 0 0 'DM'.newMap c) o fromString
where
	shift :: !*{*{Cell}} -> *{*{Cell}}
	shift arr = loop (s.rows-1) arr $ createArray (s.cols+1) ('DM'.newMap, ' ')
	where
		loop :: !Int !*{*{Cell}} !*{Cell} -> *{*{Cell}}
		loop -1 arr _ = arr
		loop n arr new
		# (old,arr) = replace arr n new
		= loop (n-1) arr old

	rvt :: Screen Int Int (Map String String) [Char] -> Screen
	rvt cells x y st [] = cells
	rvt cells x y st [c:cs]
		| y < 0 = rvt cells x 0 st [c:cs]
		| x < 0 = rvt cells (s.cols - 1) (y - 1) st [c:cs]
		| x >= s.cols = rvt cells (x rem s.cols) (y + (x / s.cols)) st [c:cs]
		| y >= s.rows = rvt (shift cells) x (y-1) st [c:cs]
		| c == '\b' = rvt cells (x-1) y st cs
		| c == '\t' = rvt cells x y st (repeatn (s.tabstop - x rem s.tabstop) ' ' ++ cs)
		| c == '\n' = rvt cells 0 (y + 1) st cs
		| c <> '\x1B' = rvt {cells & [y,x] = (st, c)} (x + 1) y st cs
		=  case cs of
			['7':cs] = trace_n "sc not supported" $ rvt cells x y st cs
			['8':cs] = trace_n "rc not supported" $ rvt cells x y st cs
			['[':cs] = case cs of
				['H':cs] = rvt cells 0 0 st cs
				['?','2','5','l':cs] = trace_n "civis not supported" $ rvt cells x y st cs
				['?','2','5','h':cs] = trace_n "cvvis not supported" $ rvt cells x y st cs
				['K':cs] = trace_n "el not supported" $ rvt cells x y st cs
				['0':'K':cs] = trace_n "el not supported" $ rvt cells x y st cs
				['1':'K':cs] = trace_n "el1 not supported" $ rvt cells x y st cs
				['2':'K':cs] = trace_n "e12 not supported" $ rvt cells x y st cs
				cs = case uptom ['m','H','f','h'] cs of
					('m', ['0'], cs) = rvt cells x y 'DM'.newMap cs
					('m', codes, cs) = rvt cells x y ('DM'.unions [st:map (style o toInt) $ split ";" $ toString codes]) cs
					('H', codes, cs) = trace_n "Curser movement not supported" $ rvt cells x y st cs
					('f', codes, cs) = trace_n "Curser movement not supported" $ rvt cells x y st cs
					(c, _, cs) = trace_n ("Escape: " +++ toString c) $ rvt cells x y st cs
			_ = trace_n "Unsupported escape" $ rvt cells x y st cs

	render :: Screen -> [HtmlTag]
	render cells = combine $ flatten $ intersperse [BrTag []] [[SpanTag [StyleAttr $ concat $ map (\(k,v)->k +++ ":" +++ v +++ ";") $ 'DM'.toList s] [Text $ toString c]
				\\(s, c)<-:row]\\row<-:cells]

	combine :: [HtmlTag] -> [HtmlTag]
	combine [] = []
	combine [SpanTag attr c,b=:BrTag _:cs] = combine [SpanTag attr (c ++ [b]):cs]
	combine [t1=:(SpanTag [StyleAttr a1] c1),t2=:(SpanTag [StyleAttr a2] c2):cs]
		| a1 == a2 = combine [SpanTag [StyleAttr a1] (c1 ++ c2):cs]
		= [t1:combine [t2:cs]]
	combine [SpanTag a cs:ts] = [SpanTag a $ combine cs:combine ts]
	combine [Text a,Text b:cs] = combine [Text $ a +++ b:cs]
	combine [c:cs] = [c:combine cs]
	

	uptom :: [Char] [Char] -> (Char, [Char], [Char])
	uptom m [c:cs]
		| isMember c m = (c, [], cs)
		| isDigit c || c == ';' = appSnd3 (\cc->[c:cc]) $ uptom m cs
		= ('-', [], cs)

	style :: (Int -> Map String String)
	style = 'DM'.fromList o maybeToList o flip 'DM'.get s.cssmap
