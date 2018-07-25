implementation module Gast.ConfSM

/*
	GAST: A Generic Automatic Software Test-system
	
	ioco: Input Output COnformance of reactive systems

	Pieter Koopman, 2004 - 2016
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

trace_cycles yes no :== no

import StdEnv, Math.Random, Gast.Gen, Data.GenEq, Gast.GenLibTest, Gast.Testable, Data.Maybe
/*
import Debug // for tracing
//-- debug operator
(->>) infix 0 :: !b .a -> .a
(->>) debugValue value
	=	debugBefore debugValue showdb
		value

showdb
	=	debugShowWithOptions
			[DebugMaxChars 79, DebugMaxDepth 10, DebugMaxBreadth 60, DebugTerminator ""]
*/
//--
toSpec :: (state input -> [(state,[output])]) -> Spec state input output // conversion for old specificaions
toSpec fun = \s i = [Pt o t \\ (t,o) <- fun s i]

SpectoIUTstep :: (Spec t i o) (t i -> [[o]]) -> IUTstep (t,RandomStream) i o | genShow{|*|} t & genShow{|*|} i & genShow{|*|} o
SpectoIUTstep spec outputGen = selectTrans
where
	selectTrans (t,[r,r2,r3:rnd]) i
		# tuples = spec t i
		  len = lengthN 353 0 tuples
		| len == 0
			= abort ("\n\nIUT is not input enabled in state " +++ show1 t +++" for input " +++ show1 i +++ "\n")
			= case tuples !! ((abs r) rem len) of
				Pt o t = (o,(t,rnd))
				Ft f  #	outputs = outputGen t i
						leno = lengthN 31 0 outputs
					  | leno == 0
						= abort ("\n\nOutput function yields no output in state " +++ show1 t +++ " for input " +++ show1 i +++ "\n")
						# output = outputs !! ((abs r2) rem leno)
						  states = f output
						  lens = lengthN 37 0 states
						| lens == 0
							= abort ("\n\nIUT is not input enabled in state " +++ show1 t +++ " for input " +++ show1 i +++ " output " +++ show1 output +++"\n")
							= (output,(states !! ((abs r3) rem lens),rnd))
 
 // =abort "toUIT"

:: NewInput i = NewInput i | Reset | End
:: TestResult o s = TestBusy | EndFound | MaxPathDone | IssueFound [Trans o s]

// XXXX
:: *TestState s i o
 =	!{	spec	:: !Spec s i o
//	,	specW	:: !SpecWorld s i
	,	iniState:: !s
	,	curState:: ![s]
	,	nRej	:: !Int
	,	nTrun	:: !Int
	,	nPath	:: !Int
	,	nStep	:: !Int
	,	nTotal	:: !Int
	,	maxPath	:: !Int
	,	maxLen	:: !Int
	,	nOnPath	:: !Int
	,	inputs	:: (RandomStream s -> [i])
	,	input	:: !((TestState s i o) -> *(NewInput i, TestState s i o))
	,	n		:: !Int
	,	rnd		:: RandomStream
	,	h_in	:: [i]
	,	h_out	:: [[o]]
	,	h_state	:: [[s]]
	,	fileName:: String
	,	errFile	:: !*File
	,	mesFile	:: !*File
	,	trace	:: !Bool
	,	incons	:: [o] [s] -> Maybe [String]
	,	stop	:: [s] -> Bool
	,	result	:: TestResult o s
	,	wantShrinking	:: Bool
	}
 
switchSpec :: (Spec s i o) (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
switchSpec spec ts=:{nOnPath} = newSpec spec nOnPath ts

newSpec :: (Spec s i o) !Int (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
newSpec spec2 0 ts=:{spec} = oldSpec spec {ts & spec = spec2}
newSpec spec2 n ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = newSpec spec2 ts.nOnPath })
	NewInput _	= (i,{ ts & input = newSpec spec2 (n-1)})
	_		= (i,ts)

oldSpec :: (Spec s i o) (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
oldSpec spec2 ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = newSpec spec2 ts.nOnPath })
	_		= (i,ts)

onAndOff :: (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
onAndOff ts=:{nOnPath} = onPath nOnPath ts

onPath :: Int (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
onPath 0 ts=:{spec}
// # (rnd1,rnd2) = split ts.rnd
 # rnd1 = ts.rnd
   rnd2 = tl rnd1
// = offPath spec (ggen{|*|} {genState & random = rnd1}) {ts & spec = mkComplete spec, rnd = rnd2} 
 = offPath spec (ggen{|*|} genState) {ts & spec = mkComplete spec} 
onPath n ts
 # (i,ts) = onTheFly ts
 = case i of
	Reset	= (i,{ ts & input = onPath ts.nOnPath })
	NewInput _	= (i,{ ts & input = onPath (n-1)})
	_		= (i,ts)

mkComplete spec s i
	= case spec s i of
		[] = [Pt [] s]
		r  = r

offPath :: (Spec s i o) [i] (TestState s i o) -> *(NewInput i, TestState s i o) | ggen{|*|} i
offPath spec [] ts = (Reset,{ ts & input = onPath ts.nOnPath })
offPath spec [i:r] ts
  | ts.nStep >= ts.maxLen-1 || ts.TestState.nRej >= ts.maxLen-1
	= (Reset,{ ts & input = onPath ts.nOnPath })
	= (NewInput i,{ts & input = offPath spec r})

// XXXX
onTheFly :: (TestState s i o) -> *(NewInput i, TestState s i o)
onTheFly ts=:{curState,inputs,rnd,spec}
	# [r1,r2,r3:rnd] = rnd
	= case [ i	\\ s <- randomize curState (genRandInt r1) 2 (\_=[])
				,  i <- inputs (genRandInt r2) s
				| not (isEmpty (spec s i))
			] of
		[]	= (Reset, {ts & rnd=rnd})
		is	# max = 57				// the maximum number of outputs to consider
			  n = lengthN max 0 is	// the number of the selected input
//			  [r:rnd] = rnd
			  i = is !! ((abs r3) rem n)
			= (NewInput i,{ts & rnd = rnd})

lengthN :: !Int !Int ![a] -> Int
lengthN m n [] = n
lengthN m n [a:x]
	| n<m
		= lengthN m (n+1) x
		= m

fixedInputs :: ![[i]] *(TestState s i o) -> *(.(NewInput i),*(TestState s i o))
fixedInputs []        ts = (End, ts)
fixedInputs [[]   :r] ts = (Reset, { ts & input = fixedInputs r })
fixedInputs [[a:x]:r] ts=:{curState,spec} 
	| isEmpty [t \\ s<- curState, t<-spec s a]
		= (Reset, { ts & input = fixedInputs r , nTrun = ts.nTrun+1})
		= (NewInput a, {ts & input = fixedInputs [x:r]})

genLongInput :: s Int (Spec s i o) [i] [Int] -> [i]
genLongInput s 0 spec inputs [r:x] = randomize inputs x 7 (\_.[])
genLongInput s n spec inputs [r,r2:x]
	= case [ i \\ i <- inputs | not (isEmpty (spec s i)) ] of
		[]	= []
		l	# i = l !! ((abs r) rem (length l))
			= case spec s i of
				[]		= abort "\n\nError in genLongInput, please report.\n\n"
				list	# len		= length list
						  s	= case list !! ((abs r2) rem len) of
								Pt o s = s
								Ft f = abort "genLongInput Ft f"
						= [ i : genLongInput s (n-1) spec inputs x ]	

genLongInputs :: s (Spec s i o) [i] Int ![Int] -> [[i]]
genLongInputs s spec inputs n [r:x] = [genLongInput s n spec inputs (genRandInt r): genLongInputs s spec inputs n x]

testConfSM :: [TestOption s i o] (Spec s i o) s (IUTstep .t i o) .t (.t->.t) *d -> ((.t,[i]),*d)
			| FileSystem d & gEq{|*|}, gLess{|*|}, genShow{|*|} s & gEq{|*|}, genShow{|*|} o & ggen{|*|}, genShow{|*|} i //& genType{|*|} i
//			| FileSystem d & ggen{|*|} i & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
testConfSM opts spec s0 iut t reset world
	# (console ,world)  = stdio world
	  filename		    = findFileName opts outputFile
	# (ok, file, world) = fopen filename FWriteText world
	| not ok
		# console = console <<< "Cannot open output file "<<< filename
		= ((t, []), snd (fclose console world))
	# //console	= console <<< ".\n"
	  ts=:{fileName}		= initState file console
	# (t,ts)	= handleTestResult (doTest ts iut t reset) iut reset
	  {mesFile, errFile} = ts
	#! world = snd (fclose mesFile (snd (fclose errFile world)))
	= ((t, reverse ts.h_in), world)
where
	initState file console
	 =	handleOptions opts
	 	{	spec	= spec
		,	iniState= s0
		,	curState= [s0]
		,	nRej	= 0
		,	nTrun	= 0
		,	nPath	= 0
		,	nStep	= 0
		,	nTotal	= 0
		,	maxPath	= 100
		,	maxLen	= 1000
		,	nOnPath	= 50
		,	inputs	= (\rnd s -> ggen {|*|} genState)
		,	input	= onTheFly 
		,	n		= 0
		,	h_in	= []
		,	h_out	= []
		,	h_state	= []
		,	rnd		= aStream
		,	errFile	= file
		,	fileName= outputFile
		,	mesFile	= console <<< "Gast starts testing.\n"
		,	trace	= False
		,	incons	= \o ss -> Nothing
		,	stop	= (\states = False)
		,	result	= TestBusy
		,	wantShrinking	= True
		}

findFileName :: ![TestOption s  i o] String -> String
findFileName [] name = name
findFileName [ErrorFile s:r] name = findFileName r s
findFileName [_:r] name = findFileName r name

handleTestResult :: (.t,TestState s i o) (IUTstep .t i o) (.t->.t) -> (.t, TestState s i o)
				 | gEq{|*|}, gLess{|*|}, genShow{|*|} s & gEq{|*|}, genShow{|*|} o & genShow{|*|} i
handleTestResult (t, ts=:{result,wantShrinking}) iut reset
 = case result of
	MaxPathDone
		| ts.TestState.nRej == 0 && ts.nTrun == 0
			= (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
											<<< ts.nPath
											<<< " test paths executed successful, in total "
											<<< ts.nTotal <<< " transitions.\n"})
			= (t,{ts & mesFile = ts.mesFile	<<< "\nEnd of testing, maximum paths used. \n"
											<<< ts.nPath
											<<< " test paths executed successful, "
											<<< ts.nTrun <<< " paths truncated, "
											<<< " in total "
											<<< ts.nTotal <<< " transitions.\n"})
	EndFound
		| ts.TestState.nRej == 0 && ts.nTrun == 0
			= (t,{ts & mesFile = ts.mesFile	<<< "\nAll input paths tested successfully.\n"
											<<< "All " <<< ts.nPath
											<<< " executed test paths successful (Proof), in total "
											<<< ts.nTotal <<< " transitions.\n"})
			= (t,{ts & mesFile = ts.mesFile	<<< "\nAll inputs tested successfully.\n"
											<<< (ts.nPath-ts.TestState.nRej)
											<<< "test path executed successful (Proof), " <<< ts.TestState.nRej
											<<< " paths rejected " <<< ts.nTrun
											<<< " paths truncated, "
											<<< "in total " <<< ts.nTotal <<< " transitions.\n"})
	IssueFound tuples
	| wantShrinking
		= shrink t ts iut reset
		= reportError t ts
	result
		= (t, {ts & mesFile = ts.mesFile	<<< "\nUnexpected result state\n!"})

doTest :: (TestState s i o) (IUTstep .t i o) .t (.t->.t) -> (.t,TestState s i o)
		| gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
doTest ts=:{input,curState,spec,stop} iut t reset
  | isEmpty ts.curState
	= (t,{ ts & result = IssueFound [] })
  | ts.nStep >= ts.maxLen || ts.TestState.nRej >= ts.maxLen || stop curState
	= doTest (resetState ts) iut (reset t) reset
  | ts.nPath >= ts.maxPath
  	= (t, { ts & result = MaxPathDone})
  #	(inp,ts) = input ts
  =	case inp of
	  Reset	= doTest (resetState ts) iut (reset t) reset
	  End	= (t, { ts & result = EndFound})
	  NewInput i	// assumption: only inputs allowed by spec will be generated:
		#!	(iut_o,t) = iut t i
			tuples	= [tup \\ s <- curState, tup <- spec s i]
			ts		= {ts	& nStep		= ts.nStep + 1
							, h_state	= [curState: ts.h_state]
							, h_in		= [i: ts.h_in]
							, h_out		= [iut_o: ts.h_out]
					  }
		= case mkset (newStates tuples iut_o) of
			[]  # ts =	{ts	& result	= IssueFound tuples
						}
				= (t, ts)
			states #! mesFile = ts.mesFile <<< "paths: " <<< ts.nPath <<< ", rejected: " <<< ts.TestState.nRej <<< ", truncated: " <<< ts.nTrun <<< "...\r"
				   = doTest {ts	& curState	= states
								, nTotal	= ts.nTotal+1
								, mesFile	= if ts.trace (mesFile <<< ts.nPath <<< "," <<< ts.nStep <<< ": " <<< show1 ts.curState <<< " " <<< show1 i <<< " " <<< show1 iut_o <<< "\n") mesFile
								}
							iut t reset

outputFile :: String
outputFile = "testOut.txt"

/*
Shrinking:
- assuption: SUT behaves deterministicly in order to repeat tests.

0) test again with lower bound on trace length
	use binary search to determine trace showing the issue with minimum length

1) cycles in spec state
2) dropping inputs			// ok
3) canceling paired inputs	// too tricky
*/
newStates :: ![.(Trans o s)] [o] -> .[s] | gEq{|*|} o
newStates [] iut_o = []
newStates [Pt o s:r] iut_o
	| o === iut_o
		= [s:newStates r iut_o]
		= newStates r iut_o
newStates [Ft f:r] iut_o = f iut_o ++ newStates r iut_o

resetState :: !*(TestState a b c) -> *(TestState a b c)
resetState ts
  =	{ts	& curState = [ts.iniState]
		, nPath    = ts.nPath+1
		, nStep    = 0
		, h_in     = []
		, h_out    = []
		, h_state  = []
		, result   = TestBusy
		, mesFile  = if ts.trace (ts.mesFile <<< "End of path reached: reset.\n") ts.mesFile
	}

errorFound :: !*(TestState s i o) -> *(TestState s i o)
errorFound ts=:{errFile, mesFile}
 # errFile = errFile <<< "Issue Found!\n"
 # mesFile = mesFile <<< "Issue Found!\n"
 = {ts & errFile = errFile, mesFile = mesFile}

shrink :: .t !*(TestState s i o) (IUTstep .t i o) (.t->.t) -> *(.t, *(TestState s i o))
				| gEq{|*|}, gLess{|*|}, genShow{|*|} s & gEq{|*|}, genShow{|*|} o & genShow{|*|} i
shrink t ts=:{h_in, nPath, nStep, h_state, h_out,result = IssueFound tuples,nTotal} iut reset
	# ts = {ts & mesFile = ts.mesFile	<<< "Issue found in path "
										<<< (nPath + 1)
										<<< " after "
										<<< show nStep
										<<< " transitions. \nInput trace: "
										<<< show (reverse h_in)
										<<< " (length "
										<<< length h_in
										<<< ")\nStart shrinking.\n"
				, maxPath = ts.maxLen * 7 // factor is not critical
			}
	  nStep1 = nStep
	  nTotal1 = nTotal
	  inputs = reverse h_in
	  states = reverse h_state
///	= shrinkTrace (cycleElimination inputs states) t tuples ts2 iut reset nStep h_state h_in h_out
/*	  ts = {ts & mesFile = ts.mesFile <<< "Single element elimination.=======\n"}
	  (t,ts) = shrinkTrace (elemElimination inputs) t tuples {ts & nStep = nStep1, nTotal = nTotal1} iut reset nStep1 h_state h_in h_out
	  ts = {ts & mesFile = ts.mesFile <<< "Binary element elimination.=======\n"}
	  (t,ts) = shrinkTrace (binElemElimination inputs) t tuples {ts & nStep = nStep1, nTotal = nTotal1} iut reset nStep1 h_state h_in h_out
	  ts = {ts & mesFile = ts.mesFile <<< "Cycle elimination. NoShrinks =======\n"}
	  (t, ts) = shrinkTrace (cycleElimination (\is.NoShrinks) inputs states) t tuples {ts & nStep = nStep1, nTotal = nTotal1} iut reset nStep1 h_state h_in h_out
*/	  ts = {ts & mesFile = ts.mesFile <<< "Cycle elimination. elemElimination =======\n"}
	  (t, ts) = shrinkTrace (cycleElimination elemElimination inputs states) t tuples {ts & nStep = nStep1, nTotal = nTotal1} iut reset nStep1 h_state h_in h_out
/*	  ts = {ts & mesFile = ts.mesFile <<< "Cycle elimination. binElemElimination =======\n"}
	  (t, ts) = shrinkTrace (cycleElimination binElemElimination inputs states) t tuples {ts & nStep = nStep1, nTotal = nTotal1} iut reset nStep1 h_state h_in h_out
*/	= (t,ts)
//	= shrinkTrace (elemElimination2 inputs) t tuples ts2 iut reset nStep h_state h_in h_out
//	= shrinkTrace (elemElimination inputs) t tuples ts2 iut reset nStep h_state h_in h_out
shrink t ts=:{h_in, nPath, nStep, h_state, h_out} iut reset
	= (t,{ ts & mesFile = ts.mesFile <<< "shrink: no issue to shrink! ?? !"})

:: Shrinks i = Shrinks [i] (Shrinks i) (Shrinks i) | NoShrinks

shrinkTrace :: (Shrinks i) .t ![Trans o s] !*(TestState s i o) (IUTstep .t i o) (.t->.t) Int [[s]] [i] [[o]] -> *(.t, *(TestState s i o))
				| gEq{|*|}, gLess{|*|}, genShow{|*|} s & gEq{|*|}, genShow{|*|} o & genShow{|*|} i
shrinkTrace NoShrinks t tuples ts iut reset nStep h_state h_in h_out
	# ts =	{ts	& nStep		= nStep
				, h_state	= h_state
				, h_in		= h_in
				, h_out		= h_out
				, result	= IssueFound tuples
			}
	= reportError t ts
shrinkTrace (Shrinks inputs2 shrinksT shrinksF) t tuples ts iut reset nStep h_state h_in h_out
	//	# ts = { ts & mesFile = ts.mesFile <<< "input length " <<< length inputs2 <<< " " }
		= case doTest {resetState ts & input = fixedInputs [inputs2]} iut (reset t) reset of
			(t2, ts2=:{result=IssueFound tuples2, nStep, h_state, h_in, h_out})
				= shrinkTrace shrinksT t2 tuples2 ts2 iut reset nStep h_state h_in h_out
			(t2, ts2)
				= shrinkTrace shrinksF t2 tuples  ts2 iut reset nStep h_state h_in h_out		
//--
elemElimination :: [i] -> Shrinks i
elemElimination inputs
// #! len = length inputs
 = //"\b\bStart element elimination, current length " + toString (length inputs) + "\t\n" ->> 
	elim 0 (length inputs) inputs
where
	elim n len inputs
	| n < len
	//	# inputs2 = removeAt n inputs
		= Shrinks inputs2 (elim n (len-1) inputs2) (elim (n+1) len inputs)
		= NoShrinks
	where inputs2 = removeAt n inputs

binElemElimination :: [i] -> Shrinks i
binElemElimination inputs
 #! len = length inputs
 | len > 1
	= //"\b\bStart binary element elimination, current length " + toString len + "\t\n" ->> 
		elim [(0,len-1)] inputs
	= NoShrinks
where
	elim [(f,d):rest] inputs
		# inputs2 = slice f d inputs
		=  //"bin elem elim " + toString f + " " + toString d + "\n" ->>
			Shrinks inputs2 (elim (adjust d rest) inputs2) (elim (if (d>1) [(f,d/2),(f+d/2,d-d/2):rest] rest) inputs)
	elim [] inputs = NoShrinks
	
	adjust d [(f,e):rest]
	| e > 1
			= [(f-d,e/2),(f-d+e/2,e-e/2):rest1]
			= rest1
	where rest1 = [(x-d,y) \\ (x,y) <- rest]
	adjust d [] = []

slice :: !Int !Int [a] -> [a]
slice 0 d list = drop d list
slice f d [a:x] = [a:slice (f-1) d x]
slice f d l = abort "slice.." // l
/*
binElemElimination :: [i] -> Shrinks i
binElemElimination inputs
 #! len = length inputs
 = "\b\bStart binary element elimination, current length " + toString len + "\t\n" ->> 
	elim 0 (len/2) inputs
where
	elim n m inputs
	| // "bin elem elim " + toString n + " " + toString m + "\n" ->>
		m > 0
		# inputs2 = slice n m inputs
		= Shrinks inputs2 (elim n (m/2) inputs2) (elim (n+m) (m/2) inputs)
		= NoShrinks

slice :: !Int !Int [a] -> [a]
slice 0 m list = drop m list
where
	drop 0 l = l
	drop n [a:x]
	| isEmpty x
		= [a]
		= drop (n-1) x
slice n m [a:x] = [a:slice (n-1) m x]
slice n m l = l
*/

/*
repeated elimination gives better results!!
*/
elemElimination2 :: [i] -> Shrinks i
elemElimination2 inputs
 #! len = length inputs
 = //"\b\bStart repeated element elimination, current length " + toString len + "\t\n" ->> 
	elim False 4 0 len inputs
where
	elim b max n len inputs
	| n < len
		# inputs2 = removeAt n inputs
		= Shrinks inputs2 (elim True max n (len-1) inputs2) (elim b max (n+1) len inputs)
	| b && max > 0
		= elim False (max-1) 0 (length inputs) inputs
		= NoShrinks
//--

cycleElimination :: ([i]->Shrinks i) [i] [s] -> Shrinks i | gEq{|*|},gLess{|*|} s
cycleElimination cont []  _ = NoShrinks
cycleElimination cont [i] _ = NoShrinks
cycleElimination cont inputs states = elim (findCycles states) inputs
where
	elim [c=:(f,t):cycles] inputs
		# inputs2 = cut f t inputs
		= Shrinks inputs2
			(trace_cycles
				( //"\bcycle removed "+toString f+" "+toString t+" length "+toString (length inputs2)+"\t\t\n" ->>
				elim (updateCycles f t cycles) inputs2)
				(elim (updateCycles f t cycles) inputs2))
			(trace_cycles
				( //"\bcycle NOT removed "+toString f+" "+toString t+"\t\t\n" ->>
				 elim cycles inputs)
				(elim cycles inputs))
	elim [] inputs = cont inputs // needs less transitions
//	elim [] inputs = elemElimination2 inputs // sometimes a little smaller results
//	elim [] inputs = "\nStart element elimination, current length " + toString (length inputs) + "\n\n" ->> elemElimination2 inputs
//	elim [] inputs = NoShrinks

/*
cycleElimination :: [i] [s] -> Shrinks i | gEq{|*|}, gLess{|*|} s
cycleElimination []  _ = NoShrinks
cycleElimination [i] _ = NoShrinks
cycleElimination inputs states = elim (findCycles states) inputs
where
	elim [c=:(f,t):cycles] inputs
		# inputs2 = cut f t inputs
		= Shrinks inputs2
			(trace_cycles
				("\bcycle removed "+toString f+" "+toString t+" length "+toString (length inputs2)+"\t\t\n" ->> elim (updateCycles f t cycles) inputs2)
				(elim (updateCycles f t cycles) inputs2))
			(trace_cycles
				("\bcycle NOT removed "+toString f+" "+toString t+"\t\t\n" ->> elim cycles inputs)
				(elim cycles inputs))
	elim [] inputs = elemElimination inputs // needs less transitions
//	elim [] inputs = elemElimination2 inputs // sometimes a little smaller results
//	elim [] inputs = "\nStart element elimination, current length " + toString (length inputs) + "\n\n" ->> elemElimination2 inputs
//	elim [] inputs = NoShrinks
*/
findCycles :: [s] -> [(Int,Int)] | gEq{|*|}, gLess{|*|} s
findCycles [] = []
findCycles [s] = []
findCycles states
	# pairs		= sortBy (\(i,s) (j,t).s-<-t) [(i,s) \\ i <- [0..] & s <- init states]	// (indix,state)-pairs sorted by state
	  groups	= groupby (snd (hd pairs)) [] pairs										// indices of equivalen states
	= filterDoubleCycles (sortBy (\(a,b) (c,d).b-a > d-c) (mkCycles groups))			// largest cycles first

filterDoubleCycles :: [(Int,Int)] -> [(Int,Int)]
filterDoubleCycles [c1=:(a,b):r] = [c1: filterDoubleCycles (filter (\(c,d).not (c-a == d-b && (a<c && c<b || c<a && a<d))) r)]
filterDoubleCycles list = list

updateCycles :: Int Int [(Int,Int)] -> [(Int,Int)]
updateCycles f t [(x,y):r]
	| y<f	// new cycle before current cycle
		= [(x,y):updateCycles f t r]
	| x>t	// new cycle after cuurent cycle
		= [(x-t+f,y-t+f):updateCycles f t r]
	| otherwise // cycles overlap: remove new cycle
		= updateCycles f t r
updateCycles f t [] = []

cut :: !Int !Int [a] -> [a] // (from,to]
cut i j list=:[a:x]
	| i < 0
		= drop (j+1) list
		= [a: cut (i-1) (j-1) x]
cut i j []    = abort "\n\ncut: list too short!\n\n"

groupby :: !a [b] [(b,a)] -> [[b]] | gEq{|*|} a
groupby t g [] = [g]
groupby t g [(i,s):r]
	| t === s
		= groupby t [i:g] r
	= [g: groupby s [i] r]

mkCycles :: [[a]] -> [(a,a)]
mkCycles []				= []
mkCycles [[]:r]			= mkCycles r
mkCycles [[i:r]:next]	= reverse [(j,i) \\ j <- r] ++ mkCycles [r:next]
//--

reportError :: .t !*(TestState s i o) -> *(.t, *(TestState s i o)) | genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
reportError t ts=:{errFile, mesFile, result = IssueFound tuples}
 #	errFile = ts.errFile	<<< "Issue found! Reporting path "
							<<< ts.nPath
							<<< ". Trace:\n"
							<<< "SpecificationStates Input -> ObservedOutput\n"
	errFile = showError ts.nStep ts.h_state ts.h_in ts.h_out errFile
	errFile = errFile <<< "\nAllowed outputs and target states: " <<< show tuples <<< "\n"
	errFile = errFile <<< "Input trace: " <<< show (reverse ts.h_in) <<<  "\n"
	errFile = errorInfo ts.nPath ts.TestState.nRej ts.nTrun (ts.nTotal+1) (errFile <<< "\n")
	mesFile = errorInfo ts.nPath ts.TestState.nRej ts.nTrun (ts.nTotal+1) ts.mesFile
	mesFile = mesFile <<< "Input trace: " <<< show (reverse ts.h_in) <<< " (length " <<< length ts.h_in <<< ")\n"
	mesFile = mesFile <<< "See file \"" <<< ts.fileName <<< "\" for details about the issue.\n"
 = (t
   ,{ts	& mesFile = mesFile
		, errFile = errFile
		, curState = []
    }
   )
reportError t ts=:{errFile, mesFile}
 = (t, { ts & mesFile = ts.mesFile <<< "reportError: no issees found! ?? !" })

errorInfo :: !Int !Int !Int !Int *File -> *File
errorInfo nPath nRej nTrun nTotal file
 = file	<<< "Issue found in path " <<< (nPath+1) <<< ", "
		<<< (nPath-nRej) <<< " paths executed, "
		<<< nTrun <<< " paths truncated, in total "
		<<< nTotal <<< " transitions.\n"

restart :: !*(TestState s i o) -> *(TestState s i o)
restart testState = { testState & h_in = [], h_out = [], h_state = [] }

showError :: Int [a] [b] [c] !*File -> *File | genShow{|*|} a & genShow{|*|} b & genShow{|*|} c
showError n [a:x] [b:y] [c:z] file = showError (n-1) x y z file <<< "    " <<< n <<< ": " <<< show a <<< " " <<< show1 b <<< " -> " <<< show c <<< "\n"
showError _ []    []    []    file = file
showError _ _     _     _     file = file <<< "\n\n\tInternal error in \"showError\", please report to pieter@cs.ru.nl!\n\n"


mkset [a:xs] = [a: [x \\ x <- xs | x =!= a]]
mkset []     = []

instance <<< [a] | <<< a
where
	(<<<) file []    = file
	(<<<) file [a:x] = file <<< a <<< x

handleOptions :: ![TestOption s i o] !*(TestState s i o) -> *(TestState s i o) | ggen{|*|} i & gEq{|*|} s
handleOptions [] ts = ts
handleOptions [o:r] ts=:{mesFile}
	# ts = case o of
				Ntests n				= { ts & maxLen		= n }
				Nsequences n			= { ts & maxPath	= n }
				Seed n					= { ts & rnd		= genRandInt n }
				Randoms rnd				= { ts & rnd		= rnd }
				FixedInputs ll_input	= { ts & input		= fixedInputs ll_input }
				InputFun f				= { ts & inputs		= f }
			//	OutputFun f = {test & } //([s] i -> o)
				FSM inp identify		= { ts & input		= fixedInputs (generateFSMpaths ts.iniState ts.spec inp identify) }
				MkTrace b				= { ts & trace		= b }
				OnPath n				= { ts & nOnPath	= n }
				OnAndOffPath			= { ts & input		= onAndOff }
				SwitchSpec spec			= { ts & input		= switchSpec spec }
				OnTheFly				= { ts & input		= onTheFly }
				ErrorFile f				= { ts & fileName	= f }
				StopStates pred			= { ts & stop		= pred }
				Shrink b				= { ts & wantShrinking = b }
	= handleOptions r ts

derive genShow Trans

// ----------------------------------------------

:: Transition state input output :== (state, input, [output], state)

:: LTS state input output // Labelled Transition System
	=	{ trans 	:: [Transition state input output]
		, initial	:: state
		// set of states and labels are not needed, this is determined by the type state.
		}

generateLTS :: (Spec s i o) s (s->[i]) -> LTS s i o | gEq{|*|} s
generateLTS spec s i = generateLTSpred (\_=True) spec s i

generateLTSpred :: (s->Bool) (Spec s i o) s (s->[i]) -> LTS s i o | gEq{|*|} s
generateLTSpred p spec s inputs
 =	{ trans 	= generateTrans [] [s] spec inputs
	, initial	= s
	}
where
//	generateTrans :: [s] [s] (Spec s i o) (s->[i]) -> [(s,i,[o],s)] | gEq{|*|} s
	generateTrans seen [] spec inputs = []
	generateTrans seen [s:todo] spec inputs
		| not (isEmpty [f \\ i <- inputs s, Ft f <- spec s i ])
			= abort "Cannot handle (Ft f) transitions in FSM"
		# trans	= [ (s,i,o,t) \\ i <- inputs s, Pt o t <- spec s i ]
		  seen	= [ s: seen ]
		  new	= [ t \\ (_,_,_,t) <- trans | isNew seen t && p t ]
		= trans ++ generateTrans seen (removeDup (todo++new)) spec inputs
//	isNew :: [s] s -> Bool | gEq{|*|} s
	isNew [] t = True
	isNew [s:seen] t
		| s===t
			= False
			= isNew seen t
//	removeDup :: !.[a] -> .[a] | gEq{|*|} a
	removeDup [x:xs] = [x:removeDup (filter ((=!=) x) xs)]
	removeDup _      = []

A4 :: (LTS state input output) (state -> [input]) -> [[input]] | gEq{|*|} state
A4 lts stateIdent = gen [([],lts.initial)] lts.trans []
where
	gen _ [] _ = []
	gen [(input,state):tups] trans next
		= step input trans state [] tups next False False
	gen [] t [] = []
	gen [] t l  = gen (reverse l) t []
	
	step input [trans=:(s1, toki, toko,s2):r] state seentrans tups next ever now
		| s1 === state
			= step [toki:input] (revApp seentrans r) s2 [] tups [(input,state):next] True True
			= step input r state [trans:seentrans] tups next ever now
	step input [] state seentrans tups next ever now 
		| now	= step input seentrans state [] tups next True False // input is extended
		| ever	= [revApp input (stateIdent state): gen tups seentrans next]
				= gen tups seentrans next
	
	revApp [] acc = acc
	revApp [a:x] acc = revApp x [a:acc]

generateFSMpaths :: s (Spec s i o) ![i] (s->[i]) -> [[i]] | gEq{|*|} s
generateFSMpaths start spec inputs identify = A4 (generateLTS spec start (\_=inputs)) identify

//----------------- the after operator from ioco theory -----------------//
//----------------- yields the possible states after an input sequence --//

(after) infix 0 :: [s] (Spec s i o) -> ([i] -> [s])
(after) s spec // = apply s
  = \i = case i of
			[]    = s
			[i:r]
				| not (isEmpty [f \\ u<-s, Ft f <- spec u i ])
					= abort "Cannot handle (Ft f) transitions in (after)"
					= ([t \\ u<-s, Pt o t <- spec u i] after spec) r

//----------------- properties of specifications -----------------//

propDeterministic :: !(Spec s i o) s i -> Bool
propDeterministic m s i = length (m s i) <= 1

propTotal :: !(Spec s i o) s i -> Bool
propTotal m s i = not (isEmpty (m s i))
