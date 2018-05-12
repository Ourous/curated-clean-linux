definition module Control.GenBimap

import StdGeneric
from Data.Maybe import :: Maybe

derive bimap Maybe, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
