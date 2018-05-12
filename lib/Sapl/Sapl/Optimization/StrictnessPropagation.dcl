definition module Sapl.Optimization.StrictnessPropagation

import Sapl.SaplParser, Sapl.Target.Flavour

:: IsStrictArgFun :== ParserState String Int Int -> Bool

// strict argument checker for Flavour file
isStrictArgFlavour :: !Flavour !ParserState !String !Int !Int -> Bool

doStrictnessPropagation :: !ParserState !IsStrictArgFun ![FuncType] -> (![FuncType], !ParserState)
