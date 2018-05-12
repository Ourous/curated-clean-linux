implementation module Data.Monoid

from StdOverloaded import class + (..), class * (..), class zero (..), class one (..)
from StdBool import &&, ||
from StdFunc import o, id
from Data.Maybe import :: Maybe(..)
from StdList import ++

instance Semigroup () where
  mappend _ _  = ()

instance Monoid () where
  mempty = ()

instance Semigroup (Dual a) | Semigroup a where
  mappend (Dual x) (Dual y) = Dual (mappend y x)

instance Monoid (Dual a) | Monoid a where
  mempty = Dual mempty

instance Semigroup (Endo a) where
  mappend (Endo f) (Endo g) = Endo (f o g)

instance Monoid (Endo a) where
  mempty = Endo id

instance Semigroup All where
  mappend (All x) (All y) = All (x && y)

instance Monoid All where
  mempty = All True

instance Semigroup Any where
  mappend (Any x) (Any y) = Any (x || y)

instance Monoid Any where
  mempty = Any False

instance Semigroup (Sum a) | + a & zero a where
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Monoid (Sum a) | + a & zero a where
  mempty = Sum zero

instance Semigroup (Product a) | * a & one a where
  mappend (Product x) (Product y) = Product (x * y)

instance Monoid (Product a) | * a & one a where
  mempty = Product one

instance Semigroup (First a) where
  mappend r=:(First (Just _)) _ = r
  mappend (First Nothing) r = r

instance Monoid (First a) where
  mempty = First Nothing

instance Semigroup (Last a) where
  mappend _ r=:(Last (Just _)) = r
  mappend r (Last Nothing) = r

instance Monoid (Last a) where
  mempty = Last Nothing

getDual :: !(Dual a) -> a
getDual (Dual x) = x

appEndo :: !(Endo a) -> (a -> a)
appEndo (Endo f) = f

getAll :: !All -> Bool
getAll (All b) = b

getAny :: !Any -> Bool
getAny (Any b) = b

getSum :: !(Sum a) -> a
getSum (Sum x) = x

getProduct :: !(Product a) -> a
getProduct (Product x) = x

getFirst :: !(First a) -> Maybe a
getFirst (First x) = x

getLast :: !(Last a) -> Maybe a
getLast (Last x) = x
