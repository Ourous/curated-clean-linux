definition module Text.Show

from Data.Maybe import :: Maybe

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

    showsPrec :: !Int   // ^ the operator precedence of the enclosing
                        // context (a number from @0@ to @11@).
                        // Function application has precedence @10@.
                 !a     // ^ the value to be converted to a 'String'
              -> ShowS

    // | A specialised variant of 'showsPrec', using precedence context
    // zero, and returning an ordinary 'String'.
    show      :: !a   -> String

    // | The method 'showList' is provided to allow the programmer to
    // give a specialised way of showing lists of values.
    // For example, this is used by the predefined 'Show' instance of
    // the 'Char' type, where values of type 'String' should be shown
    // in double quotes, rather than between square brackets.
    showList  :: ![a] -> ShowS

//------------------------------------------------------------
// Simple Instances
//------------------------------------------------------------

instance Show ()
instance Show [a] | Show a
instance Show Bool
instance Show Char
instance Show Int
instance Show (Maybe a) | Show a

//------------------------------------------------------------
// Show instances for the first few tuple
//------------------------------------------------------------

// The explicit 's' parameters are important
// Otherwise GHC thinks that "shows x" might take a lot of work to compute
// and generates defns like
//      showsPrec _ (x,y) = let sx = shows x; sy = shows y in
//                          \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

instance Show (a,b) | Show a & Show b
instance Show (a,b,c) | Show a & Show b & Show c
instance Show (a,b,c,d) | Show a & Show b & Show c & Show d
instance Show (a,b,c,d,e) | Show a & Show b & Show c & Show d & Show e
instance Show (a,b,c,d,e,f) | Show a & Show b & Show c & Show d & Show e & Show f
instance Show (a,b,c,d,e,f,g) | Show a & Show b & Show c & Show d & Show e & Show f & Show g
instance Show (a,b,c,d,e,f,g,h) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h
instance Show (a,b,c,d,e,f,g,h,i) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i
instance Show (a,b,c,d,e,f,g,h,i,j) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j
instance Show (a,b,c,d,e,f,g,h,i,j,k) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j & Show k
instance Show (a,b,c,d,e,f,g,h,i,j,k,l) | Show a & Show b & Show c & Show d & Show e & Show f & Show g & Show h & Show i & Show j & Show k & Show l

show_tuple :: ![ShowS] -> ShowS

//------------------------------------------------------------
// Support code for Show
//------------------------------------------------------------

// | equivalent to 'showsPrec' with a precedence of 0.
shows :: !a -> ShowS | Show a

// | utility function converting a 'Char' to a show function that
// simply prepends the character unchanged.
showChar :: !Char -> ShowS

// | utility function converting a 'String' to a show function that
// simply prepends the string unchanged.
showString :: !String -> ShowS

// | utility function that surrounds the inner show function with
// parentheses when the 'Bool' parameter is 'True'.
showParen :: !Bool !ShowS -> ShowS

showSpace :: ShowS

// Code specific for characters

// | Convert a character to a string using only printable characters,
// using Haskell source-language escape conventions.  For example:
//
// > showLitChar '\n' s  =  "\\n" ++ s
//
showLitChar :: !Char -> ShowS
        // I've done manual eta-expansion here, because otherwise it's
        // impossible to stop (asciiTab!!ord) getting floated out as an MFE

showLitString :: !String -> ShowS
// | Same as 'showLitChar', but for strings
// It converts the string to a string using Haskell escape conventions
// for non-printable characters. Does not add double-quotes around the
// whole thing; the caller should do that.
// The main difference from showLitChar (apart from the fact that the
// argument is a string not a list) is that we must escape double-quotes
   // Making 's' an explicit parameter makes it clear to GHC that
   // showLitString has arity 2, which avoids it allocating an extra lambda
   // The sticking point is the recursive call to (showLitString cs), which
   // it can't figure out would be ok with arity 2.

showMultiLineString :: !String -> [String]
// | Like 'showLitString' (expand escape characters using Haskell
// escape conventions), but
//   * break the string into multiple lines
//   * wrap the entire thing in double quotes
// Example:  @showMultiLineString "hello\ngoodbye\nblah"@
// returns   @["\"hello\\n\\", "\\goodbye\n\\", "\\blah\""]@

isDec :: !Char -> Bool

protectEsc :: !(Char -> Bool) !ShowS -> ShowS

asciiTab :: {String}

showSignedInt :: !Int !Int -> ShowS

