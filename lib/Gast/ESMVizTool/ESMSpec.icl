implementation module ESMVizTool.ESMSpec

import StdBool, StdList, StdMisc, StdString, StdTuple, StdOrdList
import GenPrint, GenEq, Data.List, Data.Maybe
import Gast

tupToSpec :: (state input -> [([output],state)]) -> Spec state input output // conversion for old specificaions
tupToSpec fun = \s i = [Pt o t \\ (o,t) <- fun s i]

enumerate :: [a] | ggen{|*|} a
enumerate = take 100 (generateAll aStream)

possibleInputs :: (ESM s i o) [s] -> [i] | gEq{|*|} s & ggen{|*|}, gEq{|*|} i
possibleInputs esm states = gremoveDup (take 100 [i \\ s<-states, i<-enumerate | not (isEmpty (esm.d_F s i))])

nextStates :: (ESM s i o) i ![s] -> [s] | gEq{|*|} s
nextStates esm i states
	= gremoveDup	[ t
				\\	s <- states
				,	target <- esm.d_F s i
				,	t <- case target of
							Pt outs u = [u];
							Ft f = [u \\ o<-esm.out s i, u<-f o]
				]

narrowTraces :: (Traces s i o) [s] -> Traces s i o | gEq{|*|} s
narrowTraces trace states = fst (pruneTraces trace states)

pruneTraces :: (Traces s i o) [s] -> (Traces s i o,[s]) | gEq{|*|} s
pruneTraces [] states = ([],states)
pruneTraces [trans:rest] states
	# (rest ,states) = pruneTraces rest states
	# trans = [tr \\ tr=:(s,i,o,t) <- trans | gisMember t states]
	= ([trans:rest],startStates trans)

addStep :: (ESM s i o) [s] i !(Traces s i o) -> Traces s i o | gEq{|*|} s
addStep esm states i trace
	= 	narrowTraces trace states ++
		[[	(s,i,o,t)
		 \\ s <- states
		 ,	target <- esm.d_F s i
		 , (o,t) <- case target of
						Pt outs u = [(outs,u)];
						Ft f = [ (o,u) \\ o<-esm.out s i, u<-f o]
		]]

nodesOf :: !(KnownAutomaton s i o) -> [s] | gEq{|*|} s
//nodesOf automaton				= gremoveDup (flatten [[startnode,endnode] \\ (startnode,_,_,endnode) <- automaton.trans])
nodesOf automaton				= gremoveDup ([s \\ (s,_,_,t) <- automaton.trans]++[t \\ (s,_,_,t) <- automaton.trans])

sharedNodesOf :: !(KnownAutomaton s i o) -> [s] | gEq{|*|}, Eq s
sharedNodesOf automaton
 = [ n \\ n <- nodes, m <- nodes | n<>m && n === m ]
where nodes = removeDup ([s \\ (s,_,_,t) <- automaton.trans]++[t \\ (s,_,_,t) <- automaton.trans])

edgesFrom :: s !(KnownAutomaton s i o) -> [SeenTrans s i o] | gEq{|*|} s
edgesFrom startnode automaton	= [edge \\ edge=:(s,i,o,t) <- automaton.trans | s===startnode]

edgesTo :: s !(KnownAutomaton s i o) -> [SeenTrans s i o] | gEq{|*|} s
edgesTo endnode automaton		= [edge \\ edge=:(s,i,o,t) <- automaton.trans | t===endnode]

startStates :: ![SeenTrans s i o] -> [s] | gEq{|*|} s
startStates transitions			= gremoveDup [ s \\ (s,i,o,t) <- transitions ]

targetStates :: ![SeenTrans s i o] -> [s] | gEq{|*|} s
targetStates transitions		= gremoveDup [ t \\ (s,i,o,t) <- transitions ]

addTransitions :: !Int (ESM s i o) [s] [i] !(KnownAutomaton s i o) -> KnownAutomaton s i o | render, gEq{|*|} s & render, gEq{|*|}, ggen{|*|} i & gEq{|*|} o
addTransitions n esm startstates is automaton
	| n>0 && not (isEmpty startstates)
		# newSeenTrans
			=	[	(s,i,o,t)
				\\	s <- startstates
				,	i <- map snd (sortBy (\(a,_) (b,_).a<b) (map (\i.(render i,i)) is)) // is // sort inputs
				,	target <- esm.d_F s i
				,	(o,t) <- case target of
								Pt outs u = [(outs,u)];
								Ft f = [ (o,u) \\ o<-esm.out s i, u<-f o]
				]
		# newStates	= targetStates newSeenTrans
		# newTrans	= [t \\ t <- newSeenTrans | not (gisMember t automaton.trans)]
		# newIssues	= [(t,e) \\ t<-newTrans, e <- esm.pred t | not (isEmpty e)]
		= addTransitions (n-1) esm newStates (possibleInputs esm newStates) {trans=mix automaton.trans newTrans, issues=newIssues++automaton.issues}
	| otherwise		= automaton

mix :: [SeenTrans s i o] [SeenTrans s i o] -> [SeenTrans s i o] | render s & render i
mix known new = foldl (insertBy less) known new

insertBy :: (a a->Bool) [a] a -> [a]
insertBy le [] e = [e]
insertBy le l=:[a:x] e
	| le e a
		= [e:l]
		= [a:insertBy le x e]

less :: (SeenTrans s i o) (SeenTrans s i o) -> Bool | render s & render i
less (s1,i1,o1,t1) (s2,i2,o2,t2)
	# rs1 = render s1
	# rs2 = render s2
	# ro1 = render i1
	# ro2 = render i2
	# rt1 = render t1
	# rt2 = render t2
	= rs1<rs2 || (rs1==rs2 && (ro1<ro2 || (ro1==ro2 && rt1<=rt2)))

nrOf :: !(KnownAutomaton s i o) s -> Int | gEq{|*|}, render s
nrOf automaton s
	= case gelemIndex s (nodesOf automaton) of
		Just i					= i
		nothing					= abort ("nrOf applied to unknown state: "+++render s+++"\n")

gisMember :: a ![a] -> Bool | gEq{|*|} a
gisMember x [hd:tl] = hd===x || gisMember x tl
gisMember _ _       = False

gremoveDup :: !.[a] -> .[a] | gEq{|*|} a
gremoveDup [x:xs] = [x:gremoveDup (filter ((=!=) x) xs)]
gremoveDup _      = []

gelemIndex :: a ![a] -> Maybe Int | gEq{|*|} a
gelemIndex x l = scan 0 x l
where
	scan i x [a:r]
		| x===a	= Just i
				= scan (i+1) x r
	scan i x _ = Nothing

