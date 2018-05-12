implementation module Text.Parsers.ZParsers.ParsersDerived
import Text.Parsers.ZParsers.ParsersKernel
from StdEnv import o, abort, id
from StdEnv import const, instance == Char
import StdClass, StdInt

import Control.Monad, Control.Applicative

// PARSER COMBINATORS:

(<&) infixr 6 :: !(Parser s t r) (Parser s t r`) -> Parser s t r
(<&) p1 p2 = p1 <&> \r1 -> p2 <@ const r1

(&>) infixr 6 // :: (Parser s t r) (Parser s t r`) -> Parser s t r`
(&>) p1 p2 :== p1 <&> const p2

(<&&>) infixr 6	:: !(Parser s t r) (Parser s t u) -> Parser s t (r,u)
(<&&>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> (r1,r2)

(<:&>) infixr 6	:: !(Parser s t r) (Parser s t [r]) -> Parser s t [r]
(<:&>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> [r1:r2]

(<:&:>) infixr 6	:: !(Parser s t r) (Parser s t ([r]->[r])) -> Parser s t ([r]->[r])
(<:&:>) p1 p2 = p1 <&> \r1 -> p2 <@ \r2 -> \rest -> [r1:r2 rest]

count :: !Int (Parser s t r) -> (Parser s t [r])
count n p | n <= 0 = pure []
count n p = sequence [p \\ i<-[1..n]]

// PARSER TRANSFORMERS:

<.*> :: !(Parser s t r) -> Parser s t [r]
<.*> p = (p <:&> <.*> p) <|> yield []

<*:> :: !(Parser s t r) -> Parser s t ([r]->[r])
<*:> p = (p <:&:> <*:> p) <|> yield id

<+> :: !(Parser s t r) -> Parser s t [r]
<+> p = p <:&> <.*> p

<+:> :: !(Parser s t r) -> Parser s t ([r]->[r])
<+:> p = p <:&:> <*:> p

<!*> :: (Parser s t r) -> Parser s t [r]
<!*> p = (p <&-> \r -> <!*> p <@ \rs -> [r:rs]) <-!> yield []

<!*:> :: (Parser s t r) -> Parser s t ([r]->[r])
<!*:> p = (p <&-> \r -> <!*:> p <@ \rs -> \rest -> [r:rs rest]) <-!> yield id

<!+> :: !(Parser s t r) -> Parser s t [r]
<!+> p = p <:&> <!*> p

<!+:> :: !(Parser s t r) -> Parser s t ([r]->[r])
<!+:> p = p <:&:> <!+:> p

<?> :: !(Parser s t r) (r -> u) u -> Parser s t u
<?> p f c = p <@ f <!> yield c

<!?> :: !(Parser s t r) (r -> u) u -> Parser s t u
<!?> p f c = first (<?> p f c)

(@>) infix 7 //	:: (r -> r`) (Parser s t r) -> Parser s t r`
(@>) f p :== yield f <++> p

(<@) infixl 5 :: !(Parser s t r) (r ->r`) -> Parser s t r`
(<@) p f = p <&> yield o f

(<=@) infixl 5 :: (u -> Parser s t r) (r ->r`) -> (u -> Parser s t r`)
(<=@) wp f = \u -> (wp u) <@ f

/* grazeTo by itself is NOT so useful. grazeOnce might be. If grazeTo finds a delimiter but the
   following component of the parser fails, one alternative of grazeTo is to continue scanning to
   the end of the input, going for another delimiter, and then reporting an unexpected end of input.
   And that error will 'win'. Not so helpful generally.*/ 

grazeTo :: (Parser s t r) -> Parser s t [s]
grazeTo until = p
where	p =	rewind until <@ const [] <|> anySymbol <:&> p

grazeOver :: !(Parser s t r) -> Parser s t [s]
grazeOver until = p
where	p = until <@ const [] <|> anySymbol <:&> p

/*	grazeOnce should be used with care. Usually it should be enclosed in atMost, so it will not
	scan through to the end of the whole input. */

grazeOnce :: (Parser s t r) -> Parser s t [s]
grazeOnce until = first (grazeTo until)

skipTo :: (Parser s t r) -> Parser s t u
skipTo until = p
where	p =  rewind until <@ const u <|> anySymbol &> p
		u = abort "result of rewind-parser constructor accessed in skipTo"
		
skipOver :: !(Parser s t r) -> Parser s t u
skipOver until = p
where	p = until <@ const u <|> anySymbol &> p
		u = abort "result of until-parser accessed in skipOver"
		
skipOnce :: (Parser s t r) -> Parser s t u
skipOnce until = first (skipTo until)

scrape :: (Parser s t r) (r ->Int) (r -> Parser s t v) -> Parser s t v
scrape p1 adv wp2 = getParsable <&> \pb -> p1 <&>
					\r1 -> setParsable pb &> advancePosition (adv r1) &> wp2 r1
