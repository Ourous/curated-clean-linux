definition module ESMVizTool

import ESMVizTool.ESMSpec
import iTasks

class all a | iTask, render, gEq{|*|} a


/*
esmVizTool :: !(ESM s i o) *HSt -> ((Bool,String),Html,*HSt)
					| iData, gEq{|*|}, render s 
                    & iData, gEq{|*|}, render, ggen{|*|} i 
                    & iData, gEq{|*|}, render o
*/
//esmVizTool :: !(ESM s i o) *World -> *World | iTask, gEq{|*|}, render, Eq s & iTask, gEq{|*|}, render, ggen{|*|} i & iTask, gEq{|*|}, render o
//esmVizTool :: !(ESM s i o) *World -> *World | iTask, renderEq s & iTask, renderEq, Eq, ggen{|*|} i & iTask, gEq{|*|}, render o
esmVizTool :: !(ESM s i o) *World -> *World
//			| iTask, render, gEq{|*|}, Eq s & iTask, render, gEq{|*|}, ggen{|*|} i & iTask, gEq{|*|}, render o
			| all, Eq, genShow{|*|} s & all, genShow{|*|}, ggen{|*|} i & all o

toHtmlString :: a -> String | gText{|*|} a
