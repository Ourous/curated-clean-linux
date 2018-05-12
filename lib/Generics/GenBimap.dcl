definition module GenBimap

// from StdGeneric import generic bimap
import StdGeneric
from StdMaybe import :: Maybe

derive bimap Maybe, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
