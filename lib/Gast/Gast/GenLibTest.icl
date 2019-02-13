implementation module Gast.GenLibTest

/*
	GAST: A Generic Automatic Software Test-system
	
	Gast.GenLibTest: library for generic testing: showing and comparing values

	Pieter Koopman, 2004
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, Data.GenEq

(@@)infixl 2 :: !(a->b) a -> b
(@@) f x = f x

(@@!)infixl 2 :: !(a->b) !a -> b
(@@!) f x = f x

//--- show ---//
genShow{|Int|}  			sep p x				rest = [toString x: rest]
genShow{|Char|} 			sep p x				rest = ["'",showChar x,"'": rest]
genShow{|Bool|} 			sep p x				rest = [toString x: rest]
genShow{|Real|} 			sep p x				rest = [toString x: rest]
genShow{|String|}			sep p s				rest = ["\"",s,"\"":rest] 
genShow{|UNIT|} 			sep p _				rest = rest
genShow{|PAIR|} 	fx fy	sep p (PAIR x y)	rest
//	| p
//		= ["(":fx sep p x [sep: fy sep p y [")":rest]]]
		= fx sep p x [sep: fy sep p y rest]
//genShow{|PAIR|} 	fx fy	sep p (PAIR x y)	rest = fx sep True x [sep: fy sep True y rest]
genShow{|EITHER|}	fl fr	sep p (LEFT x)		rest = fl sep p x rest
genShow{|EITHER|}	fl fr	sep p (RIGHT x)		rest = fr sep p x rest
genShow{|OBJECT|}	f    	sep p (OBJECT x)	rest = f sep p x rest
genShow{|(->)|} 	fa fr	sep p f				rest = ["<function>": rest]
genShow{|{}|}		fx		sep p xs			rest = ["{" :showList fx [x\\x<-:xs] ["}":rest]]
genShow{|{!}|}		fx		sep p xs			rest = ["{!":showList fx [x\\x<-:xs] ["}":rest]]
//genShow{|{#}|}		fx		sep p xs			rest = ["{#":showList fx [x\\x<-:xs] ["}":rest]]
genShow{|[]|}		f		sep p xs			rest = ["[" :showList f xs [ "]":rest]]
genShow{|[!]|}		f		sep p xs			rest = ["[!":showList f xs [ "]":rest]]
genShow{|[ !]|}		f		sep p xs			rest = ["[" :showList f xs ["!]":rest]]
genShow{|[!!]|}		f		sep p xs			rest = ["[!":showList f xs ["!]":rest]]
genShow{|(,)|}		f1 f2	sep p (x1,x2)		rest = ["(":f1 sep False x1 [",":f2 sep False x2 [")":rest]]]
genShow{|(,,)|}		f1 f2 f3 sep p (x1,x2,x3)	rest = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [")":rest]]]]
genShow{|(,,,)|}	f1 f2 f3 f4 sep p (x1,x2,x3,x4) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [")":rest]]]]]
genShow{|(,,,,)|}	f1 f2 f3 f4 f5 sep p (x1,x2,x3,x4,x5) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [")":rest]]]]]]
genShow{|(,,,,,)|}	f1 f2 f3 f4 f5 f6 sep p (x1,x2,x3,x4,x5, x6) rest = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [",":f6 sep False x6 [")":rest]]]]]]]
genShow{|(,,,,,,)|}f1 f2 f3 f4 f5 f6 f7 sep p (x1,x2,x3,x4,x5,x6,x7) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [",":f6 sep False x6 [",":f7 sep False x7 [")":rest]]]]]]]]
genShow{|(,,,,,,,)|}f1 f2 f3 f4 f5 f6 f7 f8 sep p (x1,x2,x3,x4,x5,x6,x7,x8) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [",":f6 sep False x6 [",":f7 sep False x7 [",":f8 sep False x8 [")":rest]]]]]]]]]
genShow{|(,,,,,,,,)|}f1 f2 f3 f4 f5 f6 f7 f8 f9 sep p (x1,x2,x3,x4,x5,x6,x7,x8,x9) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [",":f6 sep False x6 [",":f7 sep False x7 [",":f8 sep False x8 [",":f9 sep False x9 [")":rest]]]]]]]]]]
genShow{|(,,,,,,,,,)|}f1 f2 f3 f4 f5 f6 f7 f8 f9 f0 sep p (x1,x2,x3,x4,x5,x6,x7,x8,x9,x0) rest
 = ["(":f1 sep False x1 [",":f2 sep False x2 [",":f3 sep False x3 [",":f4 sep False x4 [",":f5 sep False x5 [",":f6 sep False x6 [",":f7 sep False x7 [",":f8 sep False x8 [",":f9 sep False x9 [",":f0 sep False x0 [")":rest]]]]]]]]]]]
genShow{|CONS of {gcd_name, gcd_arity}|} fx sep p (CONS x) rest
	| gcd_arity == 0
		= [gcd_name: rest]
	| otherwise
		| p // parentheses needed
//			= ["(",gcd_name," ":fx " " False x [")":rest]]
//			= [gcd_name," ":fx " " False x rest]
			= ["(",gcd_name," ":fx " " True x [")":rest]]
			= [gcd_name," ":fx " " True x rest]

genShow{|RECORD of {grd_name}|} fx sep p (RECORD x) rest
	= ["{",{grd_name.[i]\\i<-[0..size grd_name-1]},"|":fx "," False x ["}":rest]]

genShow{|FIELD of {gfd_name}|} fx sep p (FIELD x) rest
	= [gfd_name,"=":fx sep False x rest]

showChar :: Char -> String
showChar c
 = case c of
	'\n' = "\\n"
	'\t' = "\\t"
	'\r' = "\\r"
	'\b' = "\\b"
	'\'' = "\\'"
	c = toString c

showList :: (String Bool a -> ([String] -> [String])) !(l a) [String] -> [String] | List l a
showList f [|]		rest = rest
showList f [|x]		rest = f "" False x rest
showList f [|x:xs]	rest = f "" False x [",":showList f xs rest]

show :: !a -> [String] | genShow{|*|} a
show x = genShow{|*|} "" False x []

show1 :: !a -> String | genShow{|*|} a
show1 x = glue (genShow{|*|} "" False x [])
where
	glue :: [String] -> String
	glue [] = ""
	glue [x:xs]
		= case xs of
			[] = x
			   = x+++glue xs

//--- comparision ---//
/*
instance < Bool
where
	(<) True  b = not b
	(<) False _ = False
*/
generic gLess a  :: a a -> Bool

gLess{|UNIT|} _ _ 								= False
gLess{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2)	= fx x1 x2 || (not (fx x2 x1) && fy y1 y2) // x1<x2 || (x1==x2) && y1<y2
gLess{|EITHER|} fl fr (LEFT x)   (LEFT y) 		= fl x y
gLess{|EITHER|} fl fr (RIGHT x)  (RIGHT y)		= fr x y
gLess{|EITHER|} fl fr (LEFT x)   (RIGHT y) 		= True
gLess{|EITHER|} fl fr (RIGHT x)  (LEFT y)		= False
gLess{|CONS|}   f     (CONS x)   (CONS y) 		= f x y
gLess{|OBJECT|} f     (OBJECT x) (OBJECT y) 	= f x y
gLess{|FIELD|}  f     (FIELD x)  (FIELD y) 		= f x y
gLess{|RECORD|} f     (RECORD x) (RECORD y)     = f x y
gLess{|Int|} 	x y 							= x < y
gLess{|Char|} x y 								= x < y
gLess{|Bool|} False y 							= y
gLess{|Bool|} x y 								= False
gLess{|Real|} x y 								= x < y
gLess{|String|} x y 							= x < y

derive gLess [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,)

(-<-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(-<-) x y = gLess{|*|} x y

(->-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(->-) x y = gLess{|*|} y x

(-<=) infix 4 :: !a !a -> Bool | gLess{|*|} a
(-<=) x y = not (gLess{|*|} y x)

(=>-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(=>-) x y = not (gLess{|*|} x y)

