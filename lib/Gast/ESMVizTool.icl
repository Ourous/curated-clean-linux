implementation module ESMVizTool

import iTasks
import ESMVizTool.ESMSpec
import GenPrint

import Data.Graphviz
import Gast.Graphviz.Visualization

derive bimap (,), Maybe
derive class iTask KnownAutomaton, State

finished_state_color :: (!Color,!Color)
finished_state_color	= (Color "blue", Color "white")

default_state_color :: (!Color,!Color)
default_state_color		= (Color "grey90",Color "black")
	
shared_state_color :: (!Color,!Color)
shared_state_color		= (Color "gray",Color "white")
	
shared_active_state_color :: (!Color,!Color)
shared_active_state_color = (Color "gray",Color "red")

active_state_color :: !Int -> (!Color,!Color)
active_state_color nr	= (RGB 255 dim dim,Color "white")
where
	dim	= min 250 (255 - 255 / (min nr 3))
	
fontsize = 12.0 // 18.0

:: State s i o
 =	{ ka	:: !KnownAutomaton s i o
	, ss	:: ![s]
	, trace	:: !Traces s i o
	, n		:: !Int
	, r		:: !Int
	}

esmVizTool :: !(ESM s i o) *World -> *World
			| all, Eq, genShow{|*|} s & all, genShow{|*|}, ggen{|*|} i & all o
esmVizTool esm world
	= startEngine (iterateTask (DiGraphFlow esm) newstate) world
where
	newstate = { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = 15102014}
	 
DiGraphFlow :: !(ESM s i o) (State s i o) -> Task (State s i o) 
				| all, Eq, genShow{|*|} s & all, genShow{|*|}, ggen{|*|} i & all o
DiGraphFlow	esm st=:{ka,ss,trace,n,r}
 = (anyTask
	[ state esm st
	, selectInput
	, editChoice "go to" [] nodes Nothing
      >>* [OnAction ActionOk (hasValue (updateDig st))]
    , updateInformation "Multiple steps" [] n
		>>* [OnAction ActionOk (ifValue (\n.n>0) (doStepN esm st))]
    ]	-|| viewIssues st
    	-||	viewInformation (Title "Trace & legend") [ViewWith traceHtml] trace
    )
    >>= DiGraphFlow esm
where
	selectInput
		| isEmpty inputs
			= viewInformation "no transition from current state" [] "Use another action" >>| return st
			=	(	enterInformation "input" []
				>>* [ OnAction ActionYes (ifValue (\i.not (isEmpty (nextStates esm i ss))) (stepState esm st))
					, OnAction (Action "I'm feeling lucky" []) (always systemInput)
			    	, OnAction ActionPrevious (always (back st))
			    	, OnAction (Action "Prune" []) (always (prune st))
			    	, OnAction ActionNew (always (return newState))
			    	, OnAction (Action "Clear trace" []) (always (return {st & trace  = []}))
					])
				-||-
				(	viewInformation "select input" [] ""
				>>* [	OnAction (Action (show1 inp) []) (always (stepState esm st inp))
					\\	inp <- take 7 inputs
					]
				)
	
	systemInput
		| isEmpty newInputs
			| isEmpty inputs2
				= return st
				= stepState esm {st & r = rn} (inputs2!!((abs r) rem (length inputs2)))
			= stepState esm {st & r = rn} (newInputs!!((abs r) rem (length newInputs)))

	inputs		= possibleInputs esm ss
	inputs2		= [ i \\ i <- inputs, (s,j,_,t) <- ka.trans | i === j && gisMember s ss && ~ (gisMember t ss) ]
	newInputs	= filter (\i.not (gisMember i usedInputs)) inputs
	usedInputs	= [ j \\ (s,j,_,_) <- ka.trans | gisMember s ss ]
	rn			= hd (genRandInt r)
	nodes		= toHead esm.s_0 (gremoveDup (nodesOf ka ++ ss))
	newState 	= { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = rn}

	//traceTweak	=  AfterLayout (tweakUI (\x -> appDeep [0] (fixedWidth 670 o fixedHeight 200) x)) 
	
stepStateN :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
stepStateN esm st=:{ka,ss,trace,n,r}
 =		updateInformation ("Steps","Add multiple stepStates...") [] n
	>>= doStepN esm st
	
doStepN :: !(ESM s i o) !(State s i o) Int -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
doStepN esm state=:{ka,ss,trace,r} n
	= if (n>0)
  		(return {state & ka = addTransitions n esm ss (possibleInputs esm ss) ka, r = rn })
  		(return {state & n = 1})
where rn = hd (genRandInt r)

chooseTask :: !d ![(String,Task o)] -> Task o | descr d & iTask o 
chooseTask msg tasks = enterChoice msg [] [(l, Hidden t) \\ (l,t) <- tasks] >>= \(l, Hidden t). t

chooseTaskComBo :: !d ![(String,Task o)] -> Task o | descr d & iTask o 
chooseTaskComBo msg tasks
 =	updateChoice msg [] trans (trans !! 0)
 	>>= \(l, Hidden t). t
	>>! return
where
	trans = [(l, Hidden t) \\ (l,t) <- tasks]

prune :: !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
prune state=:{ka,ss,trace,n,r}
	= return	{ state
				& ka = 	{ ka
						& trans =	[ t
									\\	t <- ka.trans
									|	gisMember t onTraces
									]
						, issues =	[ i
									\\	i=:(t,_) <- ka.issues
									|	gisMember t onTraces
									]
						}
//				, r = rn
				}
where
	onTraces = flatten trace
	rn		= hd (genRandInt r)

state :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
state esm st=:{ka,ss,trace,n,r} = digraph
where
	digraph = updateInformation Void [UpdateWith toView fromView] st <<@ AfterLayout (tweakUI (fixedWidth 800 o fixedHeight 350))
	
	//Make an editable digraph from the esm state
	toView st=:{ka,ss,trace}
		= includeChanges (mkDigraph esm.esm_name (ka, esm.s_0, ss, allEdgesFound esm ka, sharedNodesOf ka, map fst ka.issues, flatten trace)) 
	//Map changes in the diagraph back to the esm state
	fromView st dg = st
		
	 //(mkDigraph "ESM" (ka, esm.s_0, ss, allEdgesFound esm ka, sharedNodesOf ka, map fst ka.issues, flatten trace))
	 //	>>= updateDig st

mkDigraph :: String (KnownAutomaton s i o,s,[s],[s],[s],[SeenTrans s i o],[SeenTrans s i o]) -> Digraph | render, gEq{|*|}, genShow{|*|} s & render, gEq{|*|} i & render, gEq{|*|} o
mkDigraph name (automaton,s_0,init_states,finished,shared,issues,trace)
	= Digraph
		name
		graphAttributes
		((if	(  gisMember s_0 (init_states ++ all_nodes) // is s_0 part of the current machine state
			//	|| isEmpty automaton.trans
			//	&& isEmpty init_states
			 	)
			[ NodeDef -1 // initil black node and arrow to s_0
				[ ]
				[ NAttLabel		""
				, NAttStyle		NStyleFilled
				, NAttColor		(Color "black")
				, NAttShape		NShapeCircle
	   			, NAttFixedSize	True
	  			, NAttWidth		0.2
				, NAttHeight	0.2
				, NAttMargin	(SingleMargin 0.003)
				]
				[( //if (gisMember s_0 init_states)
					gIndex s_0 (all_nodes ++ init_states)
					//(nrOf automaton s_0)
				 ,	[ EAttColor		(Color "black")
					, EAttArrowSize	2.0
					, EAttStyle		EStyleBold
					]
				 )
				]
			]
			[]
		) ++
		(if (isEmpty automaton.trans)
			if (isEmpty init_states)
				[ NodeDef 0 [NStAllEdgesFound False] (nodeAttributes s_0 init_states False False) []]
				[ NodeDef i [NStAllEdgesFound False] (nodeAttributes n init_states False False) []
				\\	n <- toHead s_0 init_states
				&	i <- [0..]
				]
			[NodeDef (gIndex n all_nodes) //(nrOf automaton n)
					[ NStAllEdgesFound (gisMember n finished)]
					(nodeAttributes n init_states (gisMember n finished)
					(gisMember n shared))
			         [ (gIndex t all_nodes	, [ EAttLabel (render i+++"/"+++showList ("[","]",",") o)
			                                , EAttFontName "Ariel"
			                                , EAttFontSize fontsize
			                                , EAttLabelFontName "Ariel"
			                                , EAttLabelFontSize fontsize
			                                , EAttColor
			                                			 (if (gisMember trans issues)
			                                						(Color "red")
			                                			 (if (gisMember trans trace)
			                                						(Color "blue")
			                                						(Color "black")))
			                                , EAttArrowSize (if (gisMember trans trace) 2.0 1.2)
			                                , EAttStyle (if (gisMember trans trace) EStyleBold EStyleSolid)
			                                ])
			         \\ trans=:(s,i,o,t) <- edgesFrom n automaton
			         ]
			\\ n <- all_nodes
			]
		)) Nothing
where
	graphAttributes				= [ GAttRankDir  RDLR // horizontal
	//graphAttributes				= [ GAttRankDir  RDTB // RD_LR
							  	  , GAttSize     		(Sizef 7.2 3.0 False)
								 // , GAttSize			(Sizef 5.0 3.0 True)
								  , GAttFontSize		9.0 // 12.0
								  , GAttBGColor  		(Color "white")
								  , GAttOrdering 		"in"			// "out"
								  , GAttOutputOrder		OMEdgesFirst	// OMBreadthFirst	// OMEdgesFirst	//  PK
								  ]
	all_nodes = toHead s_0 (nodesOf automaton)
	nodeAttributes n init_states finished shared
								= (if (gisMember n init_states)
										(if shared	[ NAttFillColor shac_backgr, NAttFontColor shac_txt ]
													[ NAttFillColor act_backgr, NAttFontColor act_txt ])
								  (if finished [ NAttFillColor done_backgr,NAttFontColor done_txt]
								  		(if shared	[ NAttFillColor shar_backgr, NAttFontColor shar_txt ]
									              	[ NAttFillColor def_backgr, NAttFontColor def_txt ])
								  )) ++
						          [ NAttLabel		(render n)
						          , NAttTooltip		(show1 n)
						          , NAttStyle		NStyleFilled
						          , NAttShape		NShapeEllipse
						          , NAttFontName	"Ariel"
						          , NAttFontSize	fontsize
						          , NAttFixedSize	False
						          , NAttWidth 1.0
						          ,	NAttHeight 1.0
						          , NAttMargin		(SingleMargin 0.003)
						          ]
	where
		( act_backgr, act_txt)	= active_state_color (length init_states)
		(done_backgr,done_txt)	= finished_state_color
		( def_backgr, def_txt)	= default_state_color
		(shar_backgr,shar_txt)	= shared_state_color
		(shac_backgr,shac_txt)	= shared_active_state_color

	
	showList :: !(!String,!String,!String) ![a] -> String | render a
	showList (open,close,delimit) []  = open +++ close
	showList (open,close,delimit) [x] = open +++ render x +++ close
	showList (open,close,delimit) xs  = open +++ foldr (\x str->render x+++delimit+++str) "" (init xs) +++ render (last xs) +++ close

toHead :: a [a] -> [a] | gEq{|*|} a
toHead x l | gisMember x l && hd l =!= x
	= [x: filter ((=!=) x) l]
	= l

gIndex :: a [a] -> Int | gEq{|*|} a
gIndex a l
 = case [ i \\ x <- l & i <- [0..] | a === x] of
	[i:_] = i
	[]    = -1

includeChanges :: !Digraph -> Digraph
includeChanges dg=:(Digraph _ _ _ Nothing)		= dg
includeChanges (Digraph title atts nodes change)= Digraph title atts (map includeNodeChange nodes) Nothing
where
	(SelectedItem nr`)							= fromJust change
	
	includeNodeChange :: !NodeDef -> NodeDef
	includeNodeChange (NodeDef nr st atts edges)
		| nr == nr`								= NodeDef nr st (map replaceNodeAtt atts) edges
		| otherwise								= NodeDef nr st (map defaultNodeAtt atts) edges
	where
		all_edges_found							= not (isEmpty [s \\ s=:(NStAllEdgesFound True) <- st])
		
		replaceNodeAtt (NAttFillColor _)		= NAttFillColor (fst (active_state_color 1))
		replaceNodeAtt (NAttFontColor _)		= NAttFontColor (snd (active_state_color 1))
		replaceNodeAtt att						= att
		
		defaultNodeAtt (NAttFillColor c)		= NAttFillColor (if all_edges_found (fst finished_state_color) (fst default_state_color))
		defaultNodeAtt (NAttFontColor c)		= NAttFontColor (if all_edges_found (snd finished_state_color) (snd default_state_color))
		defaultNodeAtt att						= att

//TODO: Turn this into a (Diagraph State -> State function)
updateDig :: !(State s i o) !s  -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
updateDig state=:{ka,ss,trace,n,r} ns
	= return {state & ss = [ns], trace = findSelectedStates ns ka ss trace}
where
	findSelectedStates ns ka ss trace
		| gisMember ns ss
			= narrowTraces trace [ns]
		# oneStep = [tr \\ tr=:(s,i,o,t)<-ka.trans | t===ns && gisMember s ss]
		| not (isEmpty oneStep)
			= trace++[oneStep]
			= partTraces trace ns []

stepState :: !(ESM s i o) (State s i o) i -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
stepState esm state=:{ka,ss,trace,n,r} i
		= let next   = nextStates esm i ss
			  ka`    = addTransitions 1 esm ss [i] ka
			  trace` = addStep esm ss i trace
		//	  rn	 = hd (genRandInt r)
		  in return {state & ka = ka`, ss = next, trace = trace`}

back :: (State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
back state=:{ka,ss,trace,n,r}
	| isEmpty trace
		= return state
		= let next   = startStates (last trace)
			  trace` = init trace
			  rn	 = hd (genRandInt r)
		  in return {state & trace = trace`, ss = next, r = rn}

newKA = {trans = [], issues = []}

iterateTask :: (a->Task a) a -> Task a | iTask a
iterateTask task a = task a >>= iterateTask task

traceHtml :: (Traces a b c) -> HtmlTag | render a & render b & render c
traceHtml trace
   = DivTag []
		[ H3Tag [] [Text "Trace:"]
	    , TableTag []
	       [ TrTag [] [TdTag [] (map Text (flatten [transToStrings t [] \\ t <- stepState]))]
	       \\ stepState <- trace
	       ]
	    , BrTag []
	    , H3Tag [] [Text "Legend:"]
	    , TableTag []
	       [ TrTag [] [TdTag [] [Text string]]
	       \\ string <- [ "red node: current state"
	       				, "blue node: all transitions from this node shown"
	       				, "grey node: more transitions from this node exists"
	       				, "black arrow: transition not in current trace"
	       				, "blue arrow: transition in current trace"
	       				, "red arrow: transition with an issue"
	       				, "---"
	       				, "Back: go to the previous state"
	       				, "Prune: remove all correct transitions that are not on the trace"
	       				, "Reset: start all over"
	       				, "Clear trace: remove trace, but keep everything else"
	       				]
	       ]
	    ]

viewIssues :: (State s i o) -> Task [(SeenTrans s i o,[String])] | render, iTask s & render, iTask i & render, iTask o
viewIssues st=:{ka}
	= viewInformation "Issues" [ViewWith issuesToHtml] ka.issues
where	
	issuesToHtml :: [(SeenTrans s i o,[String])] -> HtmlTag | render s & render i & render o
	issuesToHtml l
		=	DivTag []
			[ H3Tag [] [Text "Issues found:"]
			: [ TableTag []
				[ TrTag [] [TdTag [] (map Text (transToStrings t [": ":ss])) ]
				\\ (t,ss) <- l
				]
			  ]
			]

transToStrings :: (SeenTrans s i o) [String] -> [String] | render s & render i & render o
transToStrings (s,i,o,t) c = ["(",render s,",",render i,",[":showList "," o ["],",render t,")":c]]

showList :: !String ![a] [String] -> [String] | render a
showList delimit []    c = c
showList delimit [x]   c = [render x:c]
showList delimit [x:r] c = [render x,delimit:showList delimit r c]

partTraces :: (Traces s i o) s (Traces s i o) -> (Traces s i o) | gEq{|*|} s
partTraces [] s seen = []
partTraces [trans:rest] s seen
	| gisMember s (targetStates trans)
		= narrowTraces (reverse [trans:seen]) [s]
		= partTraces rest s [trans:seen]

allEdgesFound :: (ESM s i o) (KnownAutomaton s i o) -> [s] | gEq{|*|} s & ggen{|*|} i
allEdgesFound esm automaton
	= [s \\ s <- nodesOf automaton
	  | length (edgesFrom s automaton) == 
	    length	[ t
	    		\\	i <- enumerate
	    		,	t <- nextStates esm i [s]
	    		] 
	  ]

remove_spaces :: !String -> String
remove_spaces str = toString [ c \\ c <- fromString str | not (isSpace c)]

toHtmlString :: a -> String | gText{|*|} a
toHtmlString x
	# string = toSingleLineText x
	= toString [checkChar c \\ c <-fromString string]
where
	checkChar '"' = '\''
	checkChar  c  = c

instance render Int    where render i = toString i
instance render String where render s = s
