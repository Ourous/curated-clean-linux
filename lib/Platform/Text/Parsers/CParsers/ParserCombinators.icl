implementation module Text.Parsers.CParsers.ParserCombinators

// ****************************************************************************************
//	Concurrent Clean Standard Library Module
//	Copyright 1998, 1999, 2002 HILT bv & University of Nijmegen, The Netherlands
// ****************************************************************************************

import StdInt, StdBool, StdOverloaded, StdList, StdFunc, StdString, StdMisc
import Control.Applicative, Data.Maybe, Data.Functor

:: Parser s r :== [s] -> ParsResult s r
:: ParsResult s r :== [([s],r)]

:: CParser s r t  = CParser ((SucCont s r t) (XorCont s t) (AltCont s t) -> Parser s t)
:: SucCont s r t :== r (XorCont s t) (AltCont s t) -> Parser s t
:: XorCont s t   :== (AltCont s t) -> ParsResult s t
:: AltCont s t   :== ParsResult s t

//instance Applicative (CParser s r) where
  //pure x = yield x
  //(<*>) fab fa = undef //(flip (<&>)) fab fa

fail :: CParser s r t
fail = CParser (\_ xc ac _ -> xc ac)

yield :: r -> CParser s r t
yield x = CParser (\sc -> sc x)

cut :: (CParser s r t) -> CParser s r t
cut (CParser p) = CParser (\sc _ ac -> p sc id ac)

symbol :: s -> CParser s s t | == s
symbol s = CParser psymbol
where
	psymbol sc xc ac [x:ss] | x==s = sc s xc ac ss
	psymbol _  xc ac _             = xc ac

token :: [s] -> CParser s [s] t | ==s
token t = CParser p
where p sc xc ac ss
		| t == head = sc t xc ac tail
		| otherwise = xc ac
		where (head,tail) = splitAt (length t) ss

satisfy :: (s->Bool) -> CParser s s t
satisfy f = CParser p
where
	p sc xc ac [s:ss] | f s = sc s xc ac ss
	p sc xc ac _            = xc ac

eof :: CParser s Int t
eof = CParser p
where p sc xc ac [] = sc 42 xc ac []
      p sc xc ac _  = xc ac

unCP (CParser cp) = cp

begin :: !(CParser s t t) -> Parser s t
begin (CParser p) = p (\x xc ac ss -> [(ss,x):xc ac]) id []

begin1 :: !(CParser s t t) -> Parser s t
begin1 (CParser p) = p (\x _ _ ss -> [(ss,x)]) id []

(<|>) infixr 4 :: (CParser s r t) (CParser s r t) -> CParser s r t
(<|>) (CParser p1) (CParser p2) = CParser (\sc xc ac ss -> p1 sc id (p2 sc xc ac ss) ss)

(<!>) infixr 4 :: (CParser s r t) (CParser s r t) -> CParser s r t
(<!>) (CParser p1) (CParser p2) = CParser (\sc xc ac ss -> p1 (\x xc2 -> sc x xc) (\ac3 -> p2 sc xc ac3 ss) ac ss)

(<&>) infixr 6 :: (CParser s u t) (u -> CParser s v t) -> CParser s v t
(<&>) (CParser p1) f2p2 = CParser (\sc -> p1 (\t -> unCP (f2p2 t) sc))

(<&) infixr 6 :: (CParser s u t) (CParser s v t) -> CParser s u t
(<&) p1 p2 = p1 <&> \u -> p2 <@ const u

(&>) infixr 6 :: (CParser s u t) (CParser s v t) -> CParser s v t
(&>) p1 p2 = p1 <&> const p2

(<:&>) infixr 6 :: (CParser s u t) (CParser s [u] t) -> CParser s [u] t
(<:&>) p1 p2 = p1 <&> \h -> p2 <@ \t -> [h:t]

(<++>) infixr 6 :: (CParser s [u] t) (CParser s [u] t) -> CParser s [u] t
(<++>) p1 p2 = p1 <&> \l -> p2 <@ \m -> l++m

(<!&>) infixr 6 :: (CParser s u t) (u -> CParser s v t) -> CParser s v t
(<!&>) (CParser p1) f2p2 = CParser (\sc -> p1 (\t ac2 -> unCP (f2p2 t) sc id))
// Cut inside <!>, if p1 succeeds, next alt is removed

(<@) infixl 5 :: (CParser s r t) (r->u) -> CParser s u t
(<@) (CParser p) f = CParser (\sc -> p (sc o f))

<*?> :: (CParser s r t) -> CParser s [r] t
<*?> p = (p <&> \r -> <*?> p <@ \rs -> [r:rs])
     <|> yield []

<*> :: (CParser s r t) -> CParser s [r] t
//<*> p = (     p        <!&> \r  -> 
//                 <*> p <@     \rs -> [r:rs])
//          <!> yield []
// code below is semantically equivalent to combinator expression above 
<*> p = ClistP p []

ClistP :: (CParser s r t) [r] -> CParser s [r] t
ClistP (CParser p) l
// =			(p <!&> \r -> ClistP p [r:l])
//	<!>	yield (reverse l)
// code below is semantically equivalent to combinator expression above 
 = CParser (clp l)
where
	clp l sc xc ac ss = p
              (\r _ -> clp [r:l] (\x _ -> sc x xc) id)
              (\ac3 -> sc (reverse l) xc ac3 ss)
              ac ss

<+> :: (CParser s r t) -> CParser s [r] t
//<+> p = p <&> \r -> <*> p <@ \rs -> [r:rs]
// code below is semantically equivalent to combinator expression above 
<+> p = p <&> \r -> ClistP p [r]

<+?> :: (CParser s r t) -> CParser s [r] t
<+?> p = p <&> \r -> <*?> p <@ \rs -> [r:rs]

<?> :: (CParser s r t) -> CParser s [r] t // Eager version, if p succeeds use it!
<?> p = (p <@ \r -> [r]) <!> yield []

<?@> :: (CParser s r t) (r->u) u -> CParser s u t
<?@> p f u = (p <@ \r -> f r) <!> yield u

<??> :: (CParser s r t) -> CParser s [r] t // Lazy version
<??> p = (p <@ \r -> [r]) <|> yield []

<??@> :: (CParser s r t) (r->u) u -> CParser s u t
<??@> p f u = (p <@ \r -> f r) <|> yield u

(>?<) infix 9 :: (CParser s r t) (r -> Bool) -> (CParser s r t) // condition on parsed item
(>?<) (CParser p) f = CParser (\sc -> p (\r xc2 ac2 ss2 -> if (f r) (sc r xc2 ac2 ss2) (xc2 ac2)))

(@>) infix 7 :: (r->u) (CParser s r t) -> CParser s u t
(@>) f (CParser p) = CParser (\sc -> p (sc o f))

(+&+) infixl 6 :: (CParser s (u->v) t) (CParser s u t) -> CParser s v t
(+&+) (CParser p1) (CParser p2) = CParser (\sc -> p1 (\f -> p2 (sc o f)))

(-&+) infixl 8 :: (CParser s v t) (CParser s u t) -> CParser s u t
(-&+) (CParser p1) (CParser p2) = CParser (\sc -> p1 (\_ -> p2 sc))

(+&-) infixl 6 :: (CParser s v t) (CParser s u t) -> CParser s v t
(+&-) (CParser p1) (CParser p2) = CParser (\sc -> p1 (\v -> p2 (\_ -> sc v)))

digit :: CParser Char Int x
digit = satisfy isDigit <@ digitToInt

nat :: CParser Char Int t
nat = <+> digit <@ foldl (\x y -> x*10 + y) 0

int :: CParser Char Int t
int =      symbol '-' <!&> (\_ -> nat <@ ~)
       <!> nat

identifier :: CParser Char String t
identifier = satisfy isAlpha <&> \c -> <*> (satisfy isAlphanum) <@ \r -> toString [c:r]

class isWhite s :: !s -> Bool
instance isWhite Char where isWhite c = isSpace c

sp :: (CParser s r t) -> CParser s r t | isWhite s
sp (CParser p) = CParser (\sc xc ac ss -> p sc xc ac (dropWhile isWhite ss))

spsymbol :: s -> CParser s s t | ==, isWhite s
spsymbol s = sp (symbol s)

sptoken :: [s] -> CParser s [s] t | ==, isWhite s
sptoken t = sp (token t)

parseSequence :: (CParser a b c) (CParser a d c) -> CParser a [b] c
parseSequence p1 p2 = p1 <:&> (<*> (p2 &> p1))
//	accept one or more 'b''s sepparated by 'd''s; accumulate 'b''s in a list

parseChainLeft :: (CParser a b c) (CParser a (b b -> b) c) -> CParser a b c
parseChainLeft p1 p2 = p1 <&> chainl
where
	chainl x =		p2 <&> (\op -> p1 <&> (\y -> chainl (op x y)))
				<!>	yield x
// p1 parses elements that are given to the function found by p2. Left associative.

parseChainRight :: (CParser a b c) (CParser a (b b -> b) c) -> CParser a b c
parseChainRight p1 p2 = p1 <&> chainr
where
	chainr x =		p2 <&> (\op -> p1 <&> (\y -> chainr y <@ op x))
				<!>	yield x
// p1 parses elements that are given to the function found by p2. Right associative.

choice :: ![CParser a b c] -> CParser a b c // Apply the first parser from the list that succeeds
choice l = foldl (<!>) fail l

