definition module Data.Dynamic

from Data.Maybe      import :: Maybe
from Data.GenDefault import generic gDefault, generic gFiniteDefault

derive gDefault       Dynamic
derive gFiniteDefault Dynamic
