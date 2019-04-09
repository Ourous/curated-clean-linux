implementation module Data.Either.Ord

import StdEnv
import Data.Either

instance < (Either a b) | < a & < b where
	< (Left x)  (Left y)  = x < y
	< (Right x) (Right y) = x < y
	< (Left _)  (Right _) = True
	< (Right _) (Left _)  = False
