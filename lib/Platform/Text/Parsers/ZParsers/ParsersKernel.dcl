definition module Text.Parsers.ZParsers.ParsersKernel
from StdEnv import class Eq, class toString, class ==
from Data.Maybe import :: Maybe(..)

from Control.Applicative import class Applicative, class Alternative, class *>
from Control.Monad import class Monad
from Data.Functor import class Functor

// abstract type for use in getParsable and setParsable only
:: Parsable s
:: Parser s t r

// ERROR-PROCESSING:

:: SymbolType	= Whole String			// e.g. "letter", "word", "sentence","paragraph"
				| Slice String Int		// when atMost has cut off some
:: SymbolTypes		:== [SymbolType]	// in increasingly coarser units

:: SugPosition	= At Int | EndAt Int

instance == SugPosition

instance Functor (Parser s t)

instance Applicative (Parser s t)

instance Alternative (Parser s t)

class Splittable f where
  getNonPure :: (f a) -> Maybe (f a)
  getPure :: !(f a) -> Maybe a

instance Splittable (Parser s t)

instance Monad (Parser s t)

:: Gram f a = Gram [PAlt f a] (Maybe a)

:: PAlt f a
  =  E.b: Seq (f (b -> a)) (Gram f b)
  |  E.b: Bind (f b) (b -> Gram f a)

mkG :: (f a) -> Gram f a | Splittable f & Functor f

instance Functor (Gram f) | Functor f

instance Functor (PAlt f) | Functor f

(<<||>) infixl 4 :: !(Gram f (b -> a)) (Gram f b) -> Gram f a | Functor f

(<||>) infixl 4 :: !(Gram f (b -> a)) !(Gram f b) -> Gram f a | Functor f

instance Applicative (Gram f) | Functor f

instance Alternative (Gram f) | Functor f

instance Monad (Gram f) | Functor f

mkP :: !(Gram f a) -> f a | Monad f & Applicative f & Alternative f

sepBy :: !(Gram f a) (f b) -> f a | Monad, Applicative, Alternative, *> f

insertSep :: (f b) !(Gram f a) -> Gram f a | Monad, Applicative, Alternative, *> f

gmList :: !(Gram f a) -> Gram f [a] | Functor f

// PARSER CONSTRUCTORS:

// this parser always fails
fail			:: Parser s t r

// the resuling parser always succeeds, consumes no input and produces the given result
yield			:: r -> Parser s t r

// accepts any symbol and produces that as a result
anySymbol		:: Parser s t s

// resulting parser accepts a given symbol
symbol			:: s -> Parser s t s | == s

// resulting parser accepts a symbol if it satisfies the condition
satisfy			:: (s -> Bool) -> Parser s t s

// parser produces the current input-pointer as a result. consumes no input
getPosition		:: Parser s t [Int]

// resulting parser moves the input pointer a given number positions forward
advancePosition :: !Int -> Parser s t r

// parser produces the whole remaining input structure as a result, which is to be
// used in setParsable only. this is forced by (Parsable s) being an abstract type
getParsable 	:: Parser s t (Parsable s)

// repositions the input to any position
setParsable 	:: (Parsable s) -> Parser s t r

// resulting parser accepts the given list of symbols
token			:: [s] -> Parser s t [s] | == s

// parser succeeds if there is no remaining input (on the given hierarchic level)
epsilon			:: Parser s t r
			
// PARSER COMBINATORS:

// or-combinator tries both alternatives non-deterministically
//(<|>)	infixr 4 :: (Parser s t r) (Parser s t r)		-> Parser s t r

// or-combinator tries the second alternative only if the first one fails
(<!>)	infixr 4 :: !(Parser s t r) !(Parser s t r)		-> Parser s t r

// monadic sequential-combinator
(<&>)	infixr 6 :: !(Parser s t u) (u -> Parser s t v)	-> Parser s t v

// arrow-style sequential-combinator
(<++>)	infixl 6 :: !(Parser s t (r->u)) !(Parser s t r)	-> Parser s t u

/*	p1 <&>  p2 <!> p3 and
	p1 <++> p2 <!> p3 share the following behavior:
	
	a) if p1 succeeds and p2 succeeds, p3 is not tried
	b) if p1 succeeds and p2 fails, p3 is tried
	c) if p1 fails, p2 is not tried and p3 is tried
	
	p1 <&->  p2 <-!> p3 and
	p1 <++-> p2 <-!> p3 modify line b) of the above behavior:
	
	b) if p1 succeeds and p2 fails, p3 is NOT tried
*/

class Orr c
where	(<-!>) infixr 4 :: !(c s r u t) !(Parser s t r) -> Parser s t r

:: MonadicSeq s r u t = (<&->) infixr 6 (Parser s t u) (u -> Parser s t r)
instance Orr MonadicSeq

:: ArrowSeq s u r t = (<++->) infixl 6 (Parser s t (r->u)) (Parser s t r)
instance Orr ArrowSeq

// PARSER TRANSFORMERS:

// makes a parser non-deterministic: returns only the first result
first				:: !(Parser s t r) -> Parser s t r

// resulting parser applies the check to the recognized item and falis if the check fails
(checkIf) infix 7	:: !(Parser s t r) (r -> Bool) -> Parser s t r

// resulting parser applies the check to the recognized item and fails if the check fails
// the string is an error message carried over to the resulting parser. NOT TESTED YET
//(checkExplain) infix 7 :: (Parser s t r) (r -> Maybe String) -> Parser s t r

// resulting parser rewinds the input to where it started
rewind				:: (Parser s t r) -> Parser s t r

// resulting parser drops all symbols from the input that satisfy the check
dropCheck			:: (s -> Bool) !(Parser s t r) -> Parser s t r

// resulting parser will consume at most the given number of symbols and fials if more would be needed
atMost				:: !Int !(Parser s t r) -> Parser s t r

// PARSER TRANSFORMERS THAT LEAVE AND RE-ENTER THE REALM OF CONTINUATIONS:

/*	drill turns a parser that would consume say Char's as its symbols into a parser that consumes lists
	of Char's as is symbols. so if the input is [['my'],['number:'],['54365']] the following parser will
	recognize this, independent of the actual number and deliver that number as a string of digits:
	
	\_ _ n -> n @>		// forget about this: it serves to drop the two words and retain the number
	symbol ['my'] <++> symbol ['number:'] <++> drill (<!+> digit)

	The String below represents the name of a subsymbol, to be used in error messages.
*/ 
drill			:: (Parser s r r) String -> Parser [s] t r

// sortResultBy and minResultBy take a 'less' function
// if (less r1 r2) then r1 appears first in the output (else r2)
sortResultBy	:: (r r -> Bool)  !(Parser s r r) -> Parser s t r

//delivers all minimum results non-deterministically
minResultBy 	:: (r r -> Bool) !(Parser s r r) -> Parser s t r

//delivers all longest results non-deterministically
longest			:: !(Parser s r r) -> Parser s t r

// FOR ERROR REPORTING:

/*	gives a name (a hypothesis level) to the parser, for instance a parser that recognizes if-statements could be
	given the name "if-statement" which will then be used in error messages. it is advisable to avoid string literals
	and use named constants whose values be defined in a separate language module, so one could easily change to a
	different language to state the error messages in
*/
(:>)	infixl 8 :: String !(Parser s t r) -> Parser s t r

// gives a name (a hypothesis level) to the second parser in the <&> combinator
(:=>)	infixl 8 :: (r -> String) (r -> Parser s t r) -> (r -> Parser s t r)

// FOR APPLYING A PARSER TO AN INPUT

:: Result r	= Err SymbolTypes (Rose (String,[SugPosition])) [SugPosition] | Succ [r]
	//	The hypothesis paths and the SugPositions are from course to detailed.

// this data structure is used to collect error information
:: RoseNode a = RoseLeaf | RoseTwig a (Rose a)
	// RoseLeaf indicates where a twig ends, so the following are indeed different
	// [RoseTwig 1 [RoseTwig 3 [RoseLeaf,RoseTwig 4 [RoseLeaf]]]]
	// this contains a path [1,3] and a path [1,3,4] (both are closed by a RoseLeaf)
	
:: Rose a :== [RoseNode a]

// actually apply a parser to a list of symbols. the two strings are names for
// the whole input (e.g. "command line") and one symbol (e.g. "character")
parse :: !(Parser s r r) [s] String String -> Result r
