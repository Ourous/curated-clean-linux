definition module Sapl.Transform.TailRecursion

import Data.Maybe
from Sapl.SaplStruct import :: SaplTypedVar, :: SaplTerm

// Topological sort of the let definitions. Returns Nothing if a cycle is detected
sortSetters :: ![(SaplTypedVar, SaplTerm)] -> Maybe [(SaplTypedVar, SaplTerm)]
