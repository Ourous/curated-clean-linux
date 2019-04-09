definition module Data.Either.Ord

from StdOverloaded import class <
from Data.Either   import :: Either

instance < (Either a b) | < a & < b
