definition module Text.Parsers.ZParsers.ParsersAccessories

import Text.Parsers.ZParsers.ParsersKernel, Text.Parsers.ZParsers.ParsersDerived
from StdEnv import class toString, class toChar

number :: Parser  Char a Int //wants at least one digit and takes all it can get
number` :: Parser  Char a Int //takes any number of digits non-deterministically

digit    :: Parser Char a Char
hexDigit :: Parser Char a Char
letter   :: Parser Char a Char
alphaNum :: Parser Char a Char

oneOf :: [Char] -> Parser Char a Char

choice :: ![Parser s t r] -> Parser s t r

// drop initial spaces
ds :: !(Parser s t r) -> Parser s t r | space s

class space s :: !s -> Bool

instance space Char

// a version of symbol that automatically creates a hypothesis level
symbolH :: (s -> Parser s t s) | ==,toString s

// a version of token that automatically creates a hypothesis level
tokenH :: ([s] -> Parser s t [s]) | ==,toChar s

identifier :: Parser Char t String

/* Computes line and column number, taking into account tabs and line breaks. Mind that tabs and
   line breaks are themselves characters in the input string and have a position.*/
lineAndColumn :: ![Char] !Int       // position returned by error msg
                          Int ->    // standard tab width
                          (Int,Int) // line,column

errorToString :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> String

simpleErrorToString :: SymbolTypes !(Rose (String,[SugPosition])) ![SugPosition] -> String
// For testing only. Quick&Dirty really. Use import path ParserLanguage/For Testing/Language.dcl

flattenSep :: String ![x] -> String | toString x  // concatenate all with String in between

errorToStrings :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [String]

errorToFormat :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [(Int,String)]

instance toString SymbolType
