definition module Sapl.Transform.Let

import Data.Maybe
from Sapl.SaplStruct import :: SaplVar, :: SaplLetDef

// Topological sort of the let definitions. Returns Nothing if a cycle is detected
sortBindings :: ![SaplLetDef] -> Maybe [SaplLetDef]
