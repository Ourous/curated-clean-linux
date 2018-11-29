implementation module Data.GenDefault

import StdEnv
import Data.Maybe, Data.Func, Control.GenBimap, Data.Functor, Data.List, Control.Applicative

generic gDefault a :: a
gDefault{|Int|}          = 0
gDefault{|Real|}         = 0.0
gDefault{|String|}       = ""
gDefault{|Bool|}         = False
gDefault{|Char|}         = '-'
gDefault{|(->)|} _ fb    = const fb
gDefault{|[]|}     _     = []
gDefault{|[!]|}    _     = [!]
gDefault{|[ !]|}   _     = [ !]
gDefault{|[!!]|}   _     = [!!]
gDefault{|{}|}     _     = {}
gDefault{|{!}|}    _     = {!}
gDefault{|UNIT|}         = UNIT
gDefault{|EITHER|} dl dr = LEFT   dl
gDefault{|PAIR|}   dl dr = PAIR   dl dr
gDefault{|CONS|}   dc    = CONS   dc
gDefault{|FIELD|}  df    = FIELD  df
gDefault{|OBJECT|} do    = OBJECT do
gDefault{|RECORD|} dr    = RECORD dr

derive gDefault (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

// this basically performs a breadth-first search for a finite value
generic gFiniteDefault a :: [Maybe a]
gFiniteDefault{|Int|}            = [Just 0]
gFiniteDefault{|Real|}           = [Just 0.0]
gFiniteDefault{|String|}         = [Just ""]
gFiniteDefault{|Bool|}           = [Just False]
gFiniteDefault{|Char|}           = [Just '-']
gFiniteDefault{|(->)|} _ fb      = fmap const <$> fb
gFiniteDefault{|[]|}     _       = [Just []]
gFiniteDefault{|[!]|}    _       = [Just [!]]
gFiniteDefault{|[ !]|}   _       = [Just [ !]]
gFiniteDefault{|[!!]|}   _       = [Just [!!]]
gFiniteDefault{|{}|}     _       = [Just {}]
gFiniteDefault{|{!}|}    _       = [Just {!}]
gFiniteDefault{|UNIT|}           = [Just UNIT]
gFiniteDefault{|EITHER|} dsl dsr = [(LEFT <$> dl) <|> (RIGHT <$> dr) \\ dl <- dsl & dr <- dsr]
gFiniteDefault{|PAIR|}   dsl dsr = [PAIR <$> dl <*> dr \\ dl <- dsl, dr <- dsr]
gFiniteDefault{|CONS|}   dc      = fmap CONS   <$> dc
gFiniteDefault{|FIELD|}  df      = fmap (\x -> FIELD x) <$> df
// add a Nothing for each level to prevent infinite recursion to produce list elements for infinite branches
gFiniteDefault{|OBJECT|} do      = [Nothing: fmap (\x -> OBJECT x) <$> do]
gFiniteDefault{|RECORD|} dr      = fmap RECORD <$> dr

derive gFiniteDefault (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
