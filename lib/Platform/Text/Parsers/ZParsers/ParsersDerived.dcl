definition module Text.Parsers.ZParsers.ParsersDerived
from Text.Parsers.ZParsers.ParsersKernel import :: Parser, yield, <&>, <++>
from StdEnv import class Eq, class ==, const

// PARSER COMBINATORS:

// and-combinator that only retains the left hand side result
(<&)	infixr 6	:: !(Parser s t r) (Parser s t r`) -> Parser s t r

// and-combinator that only retains the right hand side result
(&>)	infixr 6 // :: (Parser s t r) (Parser s t r`) -> Parser s t r`
(&>) p1 p2 :== p1 <&> const p2

// and combinator that combines both results in a tuple
(<&&>)  infixr 6	:: !(Parser s t r) (Parser s t u) -> Parser s t (r,u)

// and combinator that combines a parser for an element and one for a list of such elements
(<:&>)	infixr 6	:: !(Parser s t r) (Parser s t [r]) -> Parser s t [r]

// and combinator that combines a parser for an element and one for a list of such elements
// to append something to the result. useful with the <++> combinator
(<:&:>) infixr 6	:: !(Parser s t r) (Parser s t ([r]->[r])) -> Parser s t ([r]->[r])

// count n p parses n occurrences of p. If n is smaller or equal to zero, the parser equals to return []. 
// Returns a list of n values returned by p. 
count :: !Int (Parser s t r) -> (Parser s t [r])

// PARSER TRANSFORMERS:

// takes any number of elements non-deterministically 
<.*>			:: !(Parser s t r) -> Parser s t [r]

// takes any number of elements non-deterministically,
// to append something to the result.  useful with the <++> combinator 
<*:>		:: !(Parser s t r) -> Parser s t ([r]->[r])

// takes one element or more non-deterministically,
<+>			:: !(Parser s t r) -> Parser s t [r]

// takes one element or more non-deterministically,
// to append something to the result.  useful with the <++> combinator 
<+:> 		:: !(Parser s t r) -> Parser s t ([r]->[r])

// takes all elements it can get. zero is okay 
<!*>		:: (Parser s t r) -> Parser s t [r]

// takes all elements it can get. zero is okay 
// to append something to the result.  useful with the <++> combinator 
<!*:> 		:: (Parser s t r) -> Parser s t ([r]->[r]) // to append something to the result

// takes all elements it can get. at least one required
<!+>		:: !(Parser s t r) -> Parser s t [r]

// takes all elements it can get. at least one required
// to append something to the result.  useful with the <++> combinator 
<!+:> 		:: !(Parser s t r) -> Parser s t ([r]->[r]) // to append something to the result

// takes an element or not, non-deterministically
<?>			:: !(Parser s t r) (r -> u) u -> Parser s t u

// takes an element if it is there
<!?>		:: !(Parser s t r) (r -> u) u -> Parser s t u

// prepends a function to the <++> combinator
(@>)		infix 7 //	:: (r -> r`) (Parser s t r) -> Parser s t r`
(@>)		f p :== yield f <++> p

// applies a function to a parse-result
(<@)		infixl 5 :: !(Parser s t r) (r ->r`) -> Parser s t r`

// moves to a point from where the input parser succeeds and produces as a result all the input skipped
grazeTo		:: (Parser s t r) -> Parser s t [s]

// moves to a point from where the input parser succeeds and produces as a result all the input skipped,
// and then moves past the recoginized item
grazeOver	:: !(Parser s t r) -> Parser s t [s]

// grazeTo deterministically: a later backtrack will not move further forward
grazeOnce	:: (Parser s t r) -> Parser s t [s]

// grazeTo with undefined result
skipTo		:: (Parser s t r) -> Parser s t u

// grazeOver with undefined result
skipOver	:: !(Parser s t r) -> Parser s t u

// grazeOnce with undefined result
skipOnce	:: (Parser s t r) -> Parser s t u

// apply first parser. on success rewind to the start, move forward a number of symbols that
// may depend on the first parser's result. then apply the second parser
scrape		:: (Parser s t r) (r ->Int) (r -> Parser s t v) -> Parser s t v
