implementation module iTasks.WF.Combinators.Overloaded

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Combinators.Core
import iTasks.WF.Combinators.Common
import Data.Maybe, Data.Either, Data.List, Data.Functor

import iTasks.Internal.Serialization

instance Functor Task where
  fmap f x = transform (fmap f) x 
instance TApplicative Task where
  (<#>) tf ta = tf >>= \f -> f <$> ta
  return x    = treturn x

instance TMonad Task where
  (>>=) l r = tbind l r
  (>>|) l r = l >>*
	[OnAction ActionContinue (always r)
	,OnValue (ifStable (\_->r))]

instance TApplicative Maybe where
  (<#>) (Just f) (Just x) = Just (f x)
  (<#>) _ _ = Nothing
  return x = Just x
instance TMonad Maybe where
  (>>=) (Just x) f = f x
  (>>=) _ _ = Nothing
  (>>|) l r = l >>= \_ -> r

instance TApplicative [] where
  (<#>) fs xs = [f x \\ f <- fs, x <- xs]
  return x = [x]
instance TMonad [] where
  (>>=) xs f = [y \\ x <- xs, y <- f x]
  (>>|) l r = l >>= \_ -> r

instance TApplicative (Either e) where
  (<#>) (Right f) (Right x) = Right (f x)
  (<#>) (Left e) _ = Left e
  (<#>) _ (Left e) = Left e
  return x = Right x
instance TMonad (Either e) where
  (>>=) (Left x) _ = Left x
  (>>=) (Right x) f = f x
  (>>|) l r = l >>= \_ -> r


