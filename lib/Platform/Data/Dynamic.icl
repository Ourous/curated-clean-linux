implementation module Data.Dynamic

import Data.GenDefault

gDefault{|Dynamic|}       = dynamic ()
gFiniteDefault{|Dynamic|} = [Just (dynamic ())]
