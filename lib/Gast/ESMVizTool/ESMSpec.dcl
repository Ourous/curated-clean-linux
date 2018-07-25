definition module ESMVizTool.ESMSpec

import StdClass, Data.Maybe
import Gast

::	Traces		   s i o :== [[SeenTrans s i o]]
::	ESM            s i o =   { s_0		:: s					// the initial state
							 , d_F		:: Spec s i o		// the state transition function (\delta_F)
							 , out		:: s i -> [[o]]		// outputs to be used if spec does not give them
							 , pred		:: (SeenTrans s i o)->[[String]] // consitency issues
							 , esm_name	:: String
							 }
::	KnownAutomaton s i o =   {trans :: [SeenTrans s i o]
							 ,issues:: [(SeenTrans s i o,[String])]
							 }
::	SeenTrans      s i o :== (s,i,[o],s)

tupToSpec :: (state input -> [([output],state)]) -> Spec state input output // conversion for old specificaions

class render a :: !a -> String		// show a concise text representation for rendering purposes

class renderEq a | render a & gEq{|*|} a

enumerate		:: [a] | ggen{|*|} a

possibleInputs	:: (ESM s i o) [s] -> [i] | gEq{|*|} s & ggen{|*|}, gEq{|*|} i
nextStates		:: (ESM s i o) i ![s] -> [s] | gEq{|*|} s
addStep			:: (ESM s i o) [s] i !(Traces s i o) -> Traces s i o | gEq{|*|} s
narrowTraces	:: (Traces s i o) [s] -> Traces s i o | gEq{|*|} s

nodesOf			:: !(KnownAutomaton s i o) -> [s] | gEq{|*|} s
sharedNodesOf	:: !(KnownAutomaton s i o) -> [s] | gEq{|*|}, Eq s
edgesFrom		:: s !(KnownAutomaton s i o) -> [SeenTrans s i o] | gEq{|*|} s
edgesTo			:: s !(KnownAutomaton s i o) -> [SeenTrans s i o] | gEq{|*|} s

startStates		:: ![SeenTrans s i o] -> [s] | gEq{|*|} s
targetStates	:: ![SeenTrans s i o] -> [s] | gEq{|*|} s

//addTransitions :: !Int (ESM s i o) [s] [i] !(KnownAutomaton s i o) -> KnownAutomaton s i o | gEq{|*|}, render s & render, ggen{|*|}, gEq{|*|} i & gEq{|*|} o
addTransitions :: !Int (ESM s i o) [s] [i] !(KnownAutomaton s i o) -> KnownAutomaton s i o | render, gEq{|*|} s & render, gEq{|*|}, ggen{|*|} i & gEq{|*|} o

nrOf			:: !(KnownAutomaton s i o) s -> Int | gEq{|*|}, render s

gisMember  :: a ![a] -> Bool | gEq{|*|} a
gremoveDup :: !.[a] -> .[a] | gEq{|*|} a
gelemIndex :: a ![a] -> Maybe Int | gEq{|*|} a
