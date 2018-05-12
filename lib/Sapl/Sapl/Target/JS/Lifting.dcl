definition module Sapl.Target.JS.Lifting

import Sapl.SaplStruct
from Data.Map import :: Map

// Returns True if a term can be inlined, i.e. no separate statement is needed
inline :: !SaplTerm -> Bool

// First function: decide on strictness. See doStrictnessPropagation
// Map: generated functions
prepareFun :: (String Int Int -> Bool) !FuncType (Map String FuncType) -> (FuncType, Map String FuncType)
prepareExpr :: (String Int Int -> Bool) !SaplTerm (Map String FuncType) -> (SaplTerm, Map String FuncType)


