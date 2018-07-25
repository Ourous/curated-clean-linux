definition module Text.Parsers.CParsers.ParserCombinators

// ****************************************************************************************
//	Concurrent Clean Standard Library Module
//	Copyright 1998, 1999, 2002 HILT bv & University of Nijmegen, The Netherlands
// ****************************************************************************************

import StdString, StdOverloaded, StdChar

:: Parser s r :== [s] -> ParsResult s r
:: ParsResult s r :== [([s],r)]

:: CParser s r t = CParser ((SucCont s r t) (XorCont s t) (AltCont s t) -> Parser s t)
:: SucCont s r t :== r (XorCont s t) (AltCont s t) -> Parser s t			// internal continuation
:: XorCont s t   :== (AltCont s t) -> ParsResult s t						// internal continuation
:: AltCont s t   :== ParsResult s t											// internal continuation

begin	:: !(CParser s t t) -> Parser s t									// to initiate a Cparser
begin1	:: !(CParser s t t) -> Parser s t									// to initiate a Cparser; only first result

fail	:: CParser s r t													// always fails
yield	:: r -> CParser s r t												// always succeed with given item
symbol	:: s -> CParser s s t | == s										// parse symbol
token	:: [s] -> CParser s [s] t | ==s										// parse list of symbols
satisfy	:: (s->Bool) -> CParser s s t										// first input satisfies condition?
eof		:: CParser s Int t													// End of input stream?

(<|>)	infixr 4 :: (CParser s r t) (CParser s r t) -> CParser s r t		// inclusive or
(<&>)	infixr 6 :: (CParser s u t) (u -> CParser s v t) -> CParser s v t	// and
(<@)	infixl 5 :: (CParser s r t) (r->u) -> CParser s u t					// postfix function application
<*>		:: (CParser s r t) -> CParser s [r] t								// zero or more times
<+>		:: (CParser s r t) -> CParser s [r] t								// one or more times

(<&)	infixr 6 :: (CParser s u t) (CParser s v t) -> CParser s u t		// only left of and
(&>)	infixr 6 :: (CParser s u t) (CParser s v t) -> CParser s v t		// only right of and
(<:&>)	infixr 6 :: (CParser s u t) (CParser s [u] t) -> CParser s [u] t	// cons results
(<++>)	infixr 6 :: (CParser s [u] t) (CParser s [u] t) -> CParser s [u] t	// append results

(<!>)	infixr 4 :: (CParser s r t) (CParser s r t) -> CParser s r t		// exclusive or
(<!&>)	infixr 6 :: (CParser s u t) (u -> CParser s v t) -> CParser s v t	// and with cut
<+?>	:: (CParser s r t) -> CParser s [r] t								// zero or more times, all
<*?>	:: (CParser s r t) -> CParser s [r] t								// one or more times. all
cut		:: (CParser s r t) -> CParser s r t									// cut exor branch

<?> 	:: (CParser s r t) -> CParser s [r] t								// Eager version, if p succeeds use it!
<?@>	:: (CParser s r t) (r->u) u -> CParser s u t						// if p succeeds apply f, otherwise yield u
<??>	:: (CParser s r t) -> CParser s [r] t								// Lazy version of <?>
<??@>	:: (CParser s r t) (r->u) u -> CParser s u t						// lazy version of <?@>
(>?<)	infix 9 :: (CParser s r t) (r -> Bool) -> (CParser s r t)			// condition on parsed item

digit		:: CParser Char Int t
nat			:: CParser Char Int t
int			:: CParser Char Int t
identifier	:: CParser Char String t

class isWhite s :: !s -> Bool
instance isWhite Char

sp			:: (CParser s r t) -> CParser s r t	| isWhite s					// skip spaces
spsymbol	:: s -> CParser s s t				| ==, isWhite s				// symbol after skipping spaces
sptoken		:: [s] -> CParser s [s] t			| ==, isWhite s				// token after skiping spaces

//AvW parseSeqence	:: (CParser a b c) (CParser a d c) -> CParser a [b] c		//	accept one or more 'b''s sepparated by 'd''s; accumulate 'b''s in a list
parseSequence	:: (CParser a b c) (CParser a d c) -> CParser a [b] c		//	accept one or more 'b''s sepparated by 'd''s; accumulate 'b''s in a list
parseChainLeft	:: (CParser a b c) (CParser a (b b -> b) c) -> CParser a b c// p1 parses elements that are given to the function found by p2. Left associative.
parseChainRight	:: (CParser a b c) (CParser a (b b -> b) c) -> CParser a b c// p1 parses elements that are given to the function found by p2. Right associative.

choice :: ![CParser a b c] -> CParser a b c									// Apply the first parser from the list that succeeds

/*
It is also possible to use prefix functions instead of the ordinary postfix functions.
conditionPrefix
 =		(\c t e -> IF c t e) // using Currying just the constructor IF will do also
	@>	sptoken ['IF']
	-&+	condition
	-&+	sptoken ['THEN']
	+&+	expression
	-&+	sptoken ['ELSE']
	+&+	expression
	+&-	sptoken ['END']

instead of

conditionPostfix
 =	sptoken ['IF']		 &>	
	condition			<&>	\c -> 
	sptoken ['THEN']	 &>
	expression			<&>	\t ->
	sptoken ['ELSE']	 &>
	expression			<&	
	sptoken ['END']		<@	\e ->
	IF c t e

with

condition :: CParser Char Bool a
condition = sptoken ['True'] <@ k True <!> sptoken ['False'] <@ k True

:: Exp = IF Bool Int Int

The choice is mostly a matter of taste. These combinators do not allow context sensitive parsers.
*/
(@>)  infix  7 :: (r->u) (CParser s r t) -> CParser s u t					// Prefix apply function to parse result
(+&+) infixl 6 :: (CParser s (u->v) t) (CParser s u t) -> CParser s v t		// And opertor to be used with @>
(-&+)  infixl 8 :: (CParser s v t) (CParser s u t) -> CParser s u t			// And operator for @>; only right result
(+&-)  infixl 6 :: (CParser s v t) (CParser s u t) -> CParser s v t			// And operator for @>; only left result

//AvW export == Char
//export isWhite Char
