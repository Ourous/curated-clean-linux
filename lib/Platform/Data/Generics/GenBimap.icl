implementation module Data.Generics.GenBimap

import StdGeneric
from Data.Maybe import :: Maybe

derive bimap Maybe, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
