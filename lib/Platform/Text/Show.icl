implementation module Text.Show

import StdArray, StdBool, StdFunc, StdOverloaded, StdClass, StdMisc, StdInt, StdChar
import StdString
import Data.Maybe
import  Data.List
import Data.Monoid
import qualified Data.Foldable
import Text

// | The @shows@ functions return a function that prepends the
// output 'String' to an existing 'String'.  This allows constant-time
// concatenation of results using function composition.
:: ShowS :== String -> String

// | Conversion of values to readable 'String's.
//
// Derived instances of 'Show' have the following properties, which
// are compatible with derived instances of 'Text.Read.Read':
//
// * The result of 'show' is a syntactically correct Haskell
//   expression containing only constants, given the fixity
//   declarations in force at the point where the type is declared.
//   It contains only the constructor names defined in the data type,
//   parentheses, and spaces.  When labelled constructor fields are
//   used, braces, commas, field names, and equal signs are also used.
//
// * If the constructor is defined to be an infix operator, then
//   'showsPrec' will produce infix applications of the constructor.
//
// * the representation will be enclosed in parentheses if the
//   precedence of the top-level constructor in @x@ is less than @d@
//   (associativity is ignored).  Thus, if @d@ is @0@ then the result
//   is never surrounded in parentheses; if @d@ is @11@ it is always
//   surrounded in parentheses, unless it is an atomic expression.
//
// * If the constructor is defined using record syntax, then 'show'
//   will produce the record-syntax form, with the fields given in the
//   same order as the original declaration.
//
// For example, given the declarations
//
// > infixr 5 :^:
// > data Tree a =  Leaf a  |  Tree a :^: Tree a
//
// the derived instance of 'Show' is equivalent to
//
// > instance (Show a) => Show (Tree a) where
// >
// >        showsPrec d (Leaf m) = showParen (d > app_prec) $
// >             showString "Leaf " . showsPrec (app_prec+1) m
// >          where app_prec = 10
// >
// >        showsPrec d (u :^: v) = showParen (d > up_prec) $
// >             showsPrec (up_prec+1) u .
// >             showString " :^: "      .
// >             showsPrec (up_prec+1) v
// >          where up_prec = 5
//
// Note that right-associativity of @:^:@ is ignored.  For example,
//
// * @'show' (Leaf 1 :^: Leaf 2 :^: Leaf 3)@ produces the string
//   @\"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\"@.

class Show a where
    // | Convert a value to a readable 'String'.
    //
    // 'showsPrec' should satisfy the law
    //
    // > showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
    //
    // Derived instances of 'Text.Read.Read' and 'Show' satisfy the following:
    //
    // * @(x,\"\")@ is an element of
    //   @('Text.Read.readsPrec' d ('showsPrec' d x \"\"))@.
    //
    // That is, 'Text.Read.readsPrec' parses the string produced by
    // 'showsPrec', and delivers the value that 'showsPrec' started with.

    showsPrec :: Int    // ^ the operator precedence of the enclosing
                        // context (a number from @0@ to @11@).
                        // Function application has precedence @10@.
                 a      // ^ the value to be converted to a 'String'
              -> ShowS

    // | A specialised variant of 'showsPrec', using precedence context
    // zero, and returning an ordinary 'String'.
    show      :: a   -> String

    // | The method 'showList' is provided to allow the programmer to
    // give a specialised way of showing lists of values.
    // For example, this is used by the predefined 'Show' instance of
    // the 'Char' type, where values of type 'String' should be shown
    // in double quotes, rather than between square brackets.
    showList  :: [a] -> ShowS

showList` :: (a -> ShowS) [a] -> ShowS
showList` _     []     = \s -> "[]" +++ s
showList` showx [x:xs] = \s -> "[" +++ showx x (showl xs s)
  where
    showl []     s = "]" +++ s
    showl [y:ys] s = "," +++ showx y (showl ys s)

        // Use unboxed stuff because we don't have overloaded numerics yet
appPrec = 10        // Precedence of application:
                        //   one more than the maximum operator precedence of 9
appPrec1 = 11       // appPrec + 1

//------------------------------------------------------------
// Simple Instances
//------------------------------------------------------------

instance Show () where
  showsPrec _ _ = \x -> showString "()" x
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show [a] | Show a where
  showsPrec _ xs = \s -> showList xs s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show Bool where
  showsPrec _ b = \s -> showString (toString b) s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance  Show Char  where
  showsPrec _ '\'' = \s -> showString "'\\''" s
  showsPrec _ c    = showChar '\'' o showLitChar c o showChar '\''
  show x = shows x ""

  showList cs = showChar '"' o showLitString (toString cs) o showChar '"'

instance Show Int where
  showsPrec x xs = \s -> showSignedInt x xs s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (Maybe a) | Show a where
  showsPrec _ (Just x) = showString "Just" o shows x
  showsPrec _ Nothing  = showString "Nothing"
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

//------------------------------------------------------------
// Show instances for the first few tuple
//------------------------------------------------------------

// The explicit 's' parameters are important
// Otherwise GHC thinks that "shows x" might take a lot of work to compute
// and generates defns like
//      showsPrec _ (x,y) = let sx = shows x; sy = shows y in
//                          \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

instance Show (a,b) | Show a & Show b where
  showsPrec _ (a,b) = \s -> show_tuple [shows a, shows b] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c) | Show a & Show b & Show c where
  showsPrec _ (a,b,c) = \s -> show_tuple [shows a, shows b, shows c] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d) | Show a & Show b & Show c & Show d where
  showsPrec _ (a,b,c,d) = \s -> show_tuple [shows a, shows b, shows c, shows d] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e) | Show a & Show b & Show c & Show d & Show e where
  showsPrec _ (a,b,c,d,e) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f) | Show a & Show b & Show c & Show d & Show e & Show f where
  showsPrec _ (a,b,c,d,e,f) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g) | Show a & Show b & Show c & Show d & Show e & Show f & Show g where
  showsPrec _ (a,b,c,d,e,f,g) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g,h) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h where
  showsPrec _ (a,b,c,d,e,f,g,h) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g,h,i) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i where
  showsPrec _ (a,b,c,d,e,f,g,h,i) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g,h,i,j) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g,h,i,j,k) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j & Show k where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

instance Show (a,b,c,d,e,f,g,h,i,j,k,l) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j & Show k & Show l where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l) = \s -> show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h,
                      shows i, shows j, shows k, shows l] s
  show x = shows x ""
  showList ls = \s -> showList` shows ls s

show_tuple :: [ShowS] -> ShowS
show_tuple ss = showChar '('
              o 'Data.Foldable'.foldr1 (\s r -> s o showChar ',' o r) ss
              o showChar ')'

//------------------------------------------------------------
// Support code for Show
//------------------------------------------------------------

// | equivalent to 'showsPrec' with a precedence of 0.
shows :: a -> ShowS | Show a
shows x =  showsPrec 0 x

// | utility function converting a 'Char' to a show function that
// simply prepends the character unchanged.
showChar :: Char -> ShowS
showChar c = \xs -> {c} +++ xs

// | utility function converting a 'String' to a show function that
// simply prepends the string unchanged.
showString :: String -> ShowS
showString xs = \ys -> xs +++ ys

// | utility function that surrounds the inner show function with
// parentheses when the 'Bool' parameter is 'True'.
showParen :: Bool ShowS -> ShowS
showParen b p = if b (showChar '(' o p o showChar ')') p

showSpace :: ShowS
showSpace = \ xs -> " " +++ xs

// Code specific for characters

// | Convert a character to a string using only printable characters,
// using Haskell source-language escape conventions.  For example:
//
// > showLitChar '\n' s  =  "\\n" ++ s
//
showLitChar :: Char -> ShowS
showLitChar c | toInt c > 127  = \s -> showChar '\\' (protectEsc isDec (shows (toInt c)) s)
showLitChar c | toInt c == 127 = \s -> showString "\\DEL" s
showLitChar '\\'           = \s -> showString "\\\\" s
showLitChar c | c >= ' '   = \s -> showChar c s
showLitChar '\a'           = \s -> showString "\\a" s
showLitChar '\b'           = \s -> showString "\\b" s
showLitChar '\f'           = \s -> showString "\\f" s
showLitChar '\n'           = \s -> showString "\\n" s
showLitChar '\r'           = \s -> showString "\\r" s
showLitChar '\t'           = \s -> showString "\\t" s
showLitChar '\v'           = \s -> showString "\\v" s
showLitChar c
| c == toChar 14    = \s -> protectEsc (\x -> x == 'H') (showString "\\SO") s
= \s -> showString ("\\" +++ asciiTab.[toInt c]) s

showLitString :: String -> ShowS
// | Same as 'showLitChar', but for strings
// It converts the string to a string using Haskell escape conventions
// for non-printable characters. Does not add double-quotes around the
// whole thing; the caller should do that.
// The main difference from showLitChar (apart from the fact that the
// argument is a string not a list) is that we must escape double-quotes
showLitString "" = \s -> s
showLitString cs
  | cs.[0] == '"' = \s -> showString "\\\"" (showLitString cs s)
  | otherwise     = \s -> showLitChar cs.[0] (showLitString cs s)
   // Making 's' an explicit parameter makes it clear to GHC that
   // showLitString has arity 2, which avoids it allocating an extra lambda
   // The sticking point is the recursive call to (showLitString cs), which
   // it can't figure out would be ok with arity 2.

showMultiLineString :: String -> [String]
// | Like 'showLitString' (expand escape characters using Haskell
// escape conventions), but
//   * break the string into multiple lines
//   * wrap the entire thing in double quotes
// Example:  @showMultiLineString "hello\ngoodbye\nblah"@
// returns   @["\"hello\\n\\", "\\goodbye\n\\", "\\blah\""]@
showMultiLineString str
  = case [pt \\ pt <- split "\n" str] of
    []  -> []
    pts -> map (quote o flip (+++) "\n") (init pts) ++ [quote (last pts)]
  where quote s = "\"" +++ s +++ "\""

isDec :: Char -> Bool
isDec c = c >= '0' && c <= '9'

protectEsc :: (Char -> Bool) ShowS -> ShowS
protectEsc p f = f o cont
                 where cont s | size s > 0 && p s.[0] = "\\&" +++ s
                       cont s                         = s


asciiTab :: {String}
asciiTab = // Using an array drags in the array module.  listArray ('\NUL', ' ')
           {"NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"}

showSignedInt :: Int Int -> ShowS
showSignedInt p n
    | n < 0 && p > 6 = \r -> "(" +++ toString n +++ ")" +++ r
    | otherwise      = \r -> toString n +++ r
