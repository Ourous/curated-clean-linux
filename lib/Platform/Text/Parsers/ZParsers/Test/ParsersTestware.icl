module ParsersTestware
import Text.Parsers.Parsers, StdEnv
from Text.Parsers.ParsersAccessories import digit
import Control.Applicative

:: TestResult r = Error String | Success [r]
reportResult :: (Result r) -> TestResult r
reportResult (Succ rs)	= Success rs
reportResult (Err symbolTypes hypotheses position)
						= Error (simpleErrorToString symbolTypes hypotheses position)

testP :: (Parser s r r) [([s],String)] -> [(TestResult r,String)]
testP p pairs = [(reportResult (parse p input "line" "character"),"\nsoll: "+++soll+++"\n\n")\\ (input,soll) <- pairs]

Start = test1
myP1 = dgit <& letta <& wordstar
test1 = testP myP1	[(['5arc'],"Err [3]: 3 stars")
					,(['5rc'],"Err [2]: lett a")
					,(['*5rc'],"Err [1]: digit")
					,(['6a***'],"Succ 6")]

//Start = test1a
myP1a = anySymbol <&> \s -> (((yield 'a' <|> yield 'b') <!> yield 'c') checkIf (\x -> x==s))
test1a = testP myP1a	[(['a'],"1 Success")
						,(['b'],"1 Success")
						,(['x'],"Error")]

//Start = test1b
myP1b = (symbol 'a' <|> (symbol 'a' <@ const 'c') <!> symbol 'b') <&> symbol
test1b = testP myP1b	[(['aa'],"1 Success")
						,(['ac'],"1 Success")
						,(['bb'],"1 Success")
						,(['a'],"Error")
						,(['b'],"Error")
						,(['c'],"Error")]

//Start = test1e
myP1e	= ((yield  'a' <|> symbol 'b') <!> symbol 'c') <&> const (symbol '*')
test1e = testP myP1e	[(['*'],"1 Success")
						,(['b*'],"1 Success")		
						,(['c*'],"Error")]

//Start = test1f
myP1f	= anySymbol <&> \s -> (((symbol 'b' <@ const 'a' <|> symbol 'b') <!> symbol 'c') checkIf (\x -> x==s))
test1f = testP myP1f	[(['ab?'],"1 Success")
						,(['bb?'],"1 Success")
						,(['x?'],"Error")]

//Start = test1g
myP1g	= (yield 'a' <|> yield 'b') <!> yield 'c'
test1g = testP myP1g	[(['x'],"Success ['a','b']")]

//Start = test2
myP2 = 	"all" :> ("myP2-all" :> (letta  <&&> wordeqs) <|> "myP2-2"  :> (letta2 <&&> wordhaq))
test2	= testP myP2 [(['a??'],"Error [2]: all [1], and within that: myP2-all [1], and within that: ==/ myP2-2 [1], and within that: ##")
					 ,(['??'],"Error [1]: all, and within that: myP2-all, and within that: a / myP2-2, and within that: 2nd a")
					 ,(['a##'],"Succ (2a,\"##\")")]

//Start = test3
myP3 = dgit <& ("orpart" :> (letta <|> letta2)) <& wordeqs 	
test3	= testP myP3 [(['4a=='],"Succ [4,4]")
					 ,(['8a'],"Err [3]: ==")		
					 ,(['0=='],"Err [2]: orpart, and within that: a / 2nd a")
					 ,([],"Err [1]: digit")]	

//Start = test3a
myP3a = dgit &> ("orpart" :> (letta <|> letta2)) <&> ((\str -> str+++"==") :=> \_ -> wordeqs) 	
test3a	= testP myP3a [(['4a=='],"Succ [==,==]")
					 ,(['8a'],"Err [3]: a==, and within that:==/ 2a==, and within that:==")		
					 ,(['0=='],"Err [2]: orpart, and within that: a / 2nd a")
					 ,([],"Err [1]: digit")]	

//Start = test4
myP4 = (letta <!> ab) &> wordeqs 
test4	= testP myP4 [(['ab=='],"Err [2]: ==)")
					 ,(['a=='],"Succ ==")
					 ,(['xxx'],"Err: [1] a /ab")]	// misleading really, ab will never succeed, because a is its prefix

//Start = test4_1
myP4_1 = wordeqs &> (letta <!> ab)
test4_1	= testP myP4_1 [(['==a'],"Succ a")
					   ,(['zzz'],"Err [1]: ==")]

//Start = test4_2
myP4_2 = (letta <!> noThing) &> (lettb <!> four)
test4_2 = testP myP4_2 [(['rr'],"Err [1]: a/b/4")
					   ,([],"Err [1]: a/b/4")]

//Start = (test5,test5_1,test5_2)
myP5 = first (letta <|> letta2) &> wordeqs
test5	= testP myP5 [(['a='],"Err [2]: ==")	
					 ,(['a=='],"Succ ==")]		

myP5_1 = (first letta) &> wordeqs
test5_1 = testP myP5_1 [(['b2'],"Err [1]: a")]

myP5_2 = (<?> letta undef undef) &> wordeqs
test5_2	= testP myP5_2 [(['a3'],"Err [2]: ==")
					   ,(['3'],"Err [1]: a / ==")
					   ,(['a=='],"Succ ==")]

//Start = (test5_3,test5_4,test6,test7)
myP5_3 = (<!?> letta undef undef) &> wordeqs
test5_3	= testP myP5_3 [(['3'],"Err [1]: a / ==")]

myP5_4 = letta <&&> anonymous
test5_4 = testP myP5_4 [(['ax'],"Err [2]: no suggestions")]

myP6 = (letta <!> letta2) &> wordeqs
test6	= testP myP6 [(['a='],"Err [2]: ==")
					 ,(['a=='],"Succ ==")]

myP7 =  (wordstar <&&> wordeqs) <& ("Troep" :> empty)
test7	= testP myP7 [(['***=='],"Succ (***,==)")
					 ,(['***==4'],"Err [6]: Troep")]

//Start = test8
myP8 = (wordstar <& ("Troep" :> empty)) <&&> wordeqs
test8	= testP myP8 [(['***=='],"Err [4]: Troep")
					 ,(['***'],"undefined rest-input of 'empty' evaluated")]

//Start= (test9,test10,test11)			
myP9 = "grazer" :> (grazeTo letta)
test9	= testP myP9 [(['68ria-jha8'],"Succ [68ri,68ria-jh]")
					 ,(['ktosp'],"Err [6]: grazer [1], and within that: a")
					 ,(['atosp'],"Succ [[]]")]
			
myP10 = grazeTo letta <&&> ab
test10	= testP myP10 [(['dsfairtabureab5'],"Succ [(dsfairt,ab),(dsfairtabure,ab)]")]

myP11 = grazeOver (wordeqs <|> wordhaq) <&&> (wordeqs <|> wordhaq)
test11	= testP myP11 [(['ir====ad===##tr'],"Succ [(ir,==),(ir====ad=,##)]")]

//Start = (test12,test13,test14,test15)
myP12 = grazeTo (letta <|> letta2) <&&> ab
test12	= testP myP12 [(['^%$a*'],"Err [6]: a /2a")]

myP13 = wordeqs checkIf \r -> r == "??"
test13	= testP myP13 [(['==7'],"Err [..2]:")]
			
myP14 = "equal4" :> (dgit checkIf \r -> r == "4") &> letta
test14	= testP myP14 [(['***'],"Err [1]: equal4, and within that: digit")
					  ,(['5a'],"Err [..1]: equal4 [1]")
					  ,(['4b'],"Err [2]: a")
					  ,(['4a'],"Succ a")]

myP15 = symbolH 'x'
test15 = testP myP15 [(['y'],"Err [1]: x")]

//Start = (test16,test17)
myP16 = (drill (symbol 'r' <&&> "U!" :> symbol 'u') "puntje") <&&> symbol ['erik']
test16	= testP myP16 [([['ru'],['erik']],"Succ (r,u),['erik']")
					  ,([['rt'],['erik']],"Err [1,2]: U!")
					  ,([['ru'],['eriz']],"Err [2]:")				
					  ,([['r'],['erik']],"Err [1,2]: U!")] 

myP17 = letta <& ("end of input" :> empty)
test17	= testP myP17 [([],"Err [1]: a")
					  ,(['a'],"Succ a")
					  ,(['ab'],"Err [2]: end of input")]

//Start = (test18,test19)
myP18 = (atMost 4 p) &> wordstar
where	p = wordstar &> wordhaq <|> wordeqs
test18	= testP myP18 [(['***##***'],"Err [4]: ##")		
					  ,(['***#'],"Err [4]: ##")
					  ,(['***'],"Err [4]: ##")
					  ,(['==***'],"Succ ***")]
			
myP19 = (rewind dgit) &> (four &> wordstar)
test19	= testP myP19 [(['4***'],"Succ ***")
					  ,(['4**'],"Err [2]: ***")
					  ,(['5***'],"Err [1]: 4")
					  ,(['?***'],"Err [1]: digit")]

//Start = (test21,test24,test25)
myP21 = (skipTo letta) &> (ab <&&> four)
test21	= testP myP21 [(['ab4..ab4..'],"Succ (ab,4),(ab,4)")		
					  ,(['ab5..ab5..'],"Err [11]: a")
					  ,(['ac5..ac5..'],"Err [11]: a")] 

myP24 = letta <& lettb <|> ab <|> letta2 <& lettb
test24	= testP myP24 [(['ab'],"Succ a,ab,2a")]

myP25 = (<!*> word) <& symbolH '.'
where	word	=  <!*> (symbolH 'a') <& <!+> (symbolH 'b')
test25	= testP myP25 [(['aaaabbbbaaaaaaaaaabbb.'],"two lists of a's")]

//Start = test25a
myP25a = (symbol 'a' <& symbol 'b' ) <!> symbol 'a' 
test25a	= testP myP25a	[(['a'],"Succ a")
						,(['ab'],"Succ a")]

from StdEnv import repeatn, class length
//Start = test26 500000	// needs quite some heap and stack nodig, even much stack after printing out the result!
myP26 = (<!*> lettb) <@ length
test26 n	= testP myP26 [(repeatn n 'b',"Succ "+++toString n)]

//Start = (test27,test28)
myP27 = ds four <&&> ds wordstar
test27	= testP myP27 [(['   4    ***'],"Succ (4,***)"),(['  5'],"Err [3]: 4")]

myP28 =	"eqZ" :>
			("isUpper" :>
				("isAlpha" :>("char" :> anySymbol checkIf isAlpha) checkIf isUpper)
			 checkIf ((==) 'Z')
			)
test28	= testP myP28	[([],"Error [1]: eqZ, and within that: isUpper, and within that: isAlpha, and within that: char")
						,(['3'],"Error [..1]: eqZ [1], and within that: isUpper [1], and within that: isAlpha [1]")
						,(['a'],"Error [..1]: eqZ [1], and within that: isUpper []")
						,(['A'],"Error [..1]: eqZ [1]")	
						,(['Z'],"Succ Z")]
						
					
//Start = (test29,test30,test30a)
myP29 =  "eqZ" :> ((letta <|> lettb) checkIf ((==) "Z"))
test29 = testP myP29	[(['a'],"Error [..1]: eqZ [1], and within that: b")
						,(['b'],"Error [..1]: eqZ [1], and within that: a") 
						,(['c'],"Error [1]: eqZ, and within that: a / b")] 

myP30	=	"eqP" :> (chara checkIf ((==) 'p')) <|>
			charb <|>
			"eqQ" :> (chara2 checkIf ((==) 'q'))
where	chara	= "chara" :> symbol 'a'
		charb	= symbolH 'b'
		chara2	= "chara1" :> symbol 'a'

test30 =testP myP30 [(['a'],"Error: [..1] eqP [1], eqQ [1]")]

myP30a = letta <|> letta
test30a = testP myP30a [(['a'],"Succ [a, a]")]

//Start = (test31,test32,test33,test34,test35)
myP31 = "global" :> (symbol 'G' <&&> "detail" :> symbol 'D')
test31 = testP myP31 [(['GX'],"Error [2]: global [1], and within that: detail")]

myP32 = "global" :> (symbol 'G' <&&> symbol 'D')
test32 = testP myP32 [(['GX'],"Error [2]: global [1]")]

myP33 = ("G" :> symbol 'G' checkIf ((==) 'G')) <&&> symbol 'X' <&&> symbol 'Y'
test33 = testP myP33 [(['GXZ'],"Error [3]:")]

myP34 = "global" :> ((symbol 'G' <&&> "detail" :> symbol 'X') checkIf ((==) ('G','Y')))
test34 = testP myP34 [(['GX'],"Error [..2]: global [1]")]

myP35 = (letta <&&> getPosition) <|> (ab <&&> getPosition)
test35 = testP myP35 [(['abc'],"Succ: (a,[2]),(ab,[3])")]

//Start = test36
myP36 = (letta <&-> \a -> lettb) <-!> four
test36 = testP myP36 [(['ab'],"Succ: b"),(['4'],"Succ: 4"),(['ac'],"Error [2]: b")]

//Start = testP myP [(repeatn 3000 'a',"Succ")]
where myP = (<!*> letta) &> yield "einde"
 // needs at most 243 KB heap (dead branches are cut off at once) (heap is 48% of classical solution with <!> inside <!*>)
 
//Start = (test37,test38)
test37 = testP myP37[([['a']],"Error [2]: three")			
					,([['a'],['4']],"Error [2,1]: three [2]")
					,([['a'],['3']],"Succ 3")
					,([['b'],['3']],"Error [1,1]: prefix [1]")] 

myP37 :: Parser [Char] t String
myP37 = prefix &> three
where	prefix = "prefix" :> (word (symbol 'a'))
		three = "three" :> (word (symbol '3' <@ toString))
word p	= drill p "position"

test38 = testP myP38 [([],"Error [1,1]: inside")
					 ,([[]],"Error [1,1]: inside")]
myP38 :: Parser [Char] t Char
myP38 = word ("inside" :> symbol 'x')

//Start = (test39,test40)
test39 = testP myP [(['   a'],"Succ a")
				   ,(['12ab'],"Error [4]: a")
				   ,(['23'],"Error [1]: advance")] 
where	//myP :: Parser Char String _
		myP = "advance" :> advancePosition 3 &> letta 

test40 = testP myP [(['a'],"Error [1] advance / b")
				   ,(['b'],"Succ b")] 
where	//myP :: Parser Char String _
		myP = ("advance" :> advancePosition (~3) &> letta) <!> lettb
		
//Start = (test41,test42,test43)
test41 = testP myP [(['ab'],"Succ (ab,a)")
				   ,(['ar'],"Error [1]: ab")]
where	myP :: Parser Char (String,String) (String,String) 
		myP = getParsable <&> \p -> ab <&&> setParsable p &> letta

test42 = testP myP [(['ar'],"Error [1]: ab")]
where	myP :: Parser Char (String,String) (String,String) 
		myP = getParsable <&> \p -> letta <&&> setParsable p &> ab

test43 = testP myP [(['a'],"Succ a")
				   ,(['ab'],"Succ [a,ab]")
				   ,(['v'],"Error [1]: a / 2nd a")]
where	myP :: Parser Char String String 
		myP = getParsable <&> \p -> letta <|> letta2 &> setParsable p &> ab

//Start = test44
test44 = testP myP [(['***.......a'],"Succ (***,a)")
				   ,(['***......a'],"Error [11]: a")
				   ,(['***........a'],"Error [11]: a")
				   ,(['***....'],"Error [1]: Could not advance 10 steps")]
where	myP :: Parser Char (String,String) (String,String) 
		myP = getParsable <&> \p -> wordstar <&&> setParsable p &> adv 10 &> letta
		adv 0 = yield undef
		adv n # msg	= if (n < 0)("Cannot advance a negative number of steps ("+++toString n+++")")
								("Could not advance "+++toString n+++" steps")
			  = msg :> advancePosition n

//Start = test45
test45 = testP myP [(['***.......a'],"Succ (***,a)")
				   ,(['***......a'],"Error [11]: a")
				   ,(['***........a'],"Error [11]: a")]
where	//myP :: Parser Char (String,String) _ 
		myP = scrape wordstar (const 10) \stars -> letta <@ \a -> (stars,a)
		
//Start = (test46,test46a,test46b,test47)
test46 = testP myP [(['5555555'],"Succ triples, consisting of 8 fives in total, with maximum product")]
where	myP = minResultBy comp (number` <&> \i -> number` <&> \j -> number <@ \k -> (i,j,k,i*j*k))
		comp (_,_,_,i)(_,_,_,j) = i>j

test46a = testP myP [(['5555555'],"Succ triples, consisting of 8 fives in total, with decreasing product")]
where	myP = sortResultBy comp (number` <&> \i -> number` <&> \j -> number <@ \k -> (i,j,k,i*j*k))
		comp (_,_,_,i)(_,_,_,j) = i>j

test46b = testP myP [(['5555555'],"Succ triples, consisting of 8 fives in total, with maximum product")]
where	myP = minResultBy comp (wrap @> number` <++> number` <++> number)
		comp (_,_,_,i)(_,_,_,j) = i>j
		wrap i j k = (i,j,k,i*j*k)

test47 = testP myP [(['aaaba'],"Succ [([a,a,a],b),([a,a],ab)]")]
where	myP = longest (<.*> letta <&&> (ab <|> lettb))

//Start = (test48,test48a,test49,test49a,test49b,test49c)
test48 = testP myP [(['ab'],"Error [2]: 4"),(['a4'],"Success 4")]
where	myP = letta <&-> const four <-!> ab

test48a = testP myP [(['c'],"Error [1]: a / b"),(['b'],"Success b")]
where	myP = letta <&-> const four <-!> lettb

test49 = testP myP [(['ab'],"Error [2]: 4"),(['a4'],"Success a4"),(['c'],"Error [1]: a/ab")]
where	myP = (+++) @> letta <++-> four <-!> ab

test49a = testP myP [(['c'],"Error [1]: a / b"),(['b'],"Success b")]
where	myP = (+++) @> letta <++-> four <-!> lettb

test49b = testP myP [(['a4'],"Success a4"),(['b4'],"Success b4"),(['4'],"Success 4"),(['b5'],"Error [2]: 4"),(['c'],"Error [1]: a/b/digit")]
where	myP = (+++) @> (letta <|> lettb) <++-> four <-!> dgit

test49c = testP myP [(['x'],"Error [1]: a/ab/digit"),(['a4'],"Success a4"),(['3'],"Success 3")]
where	myP = ((+++) @> letta <++-> four <-!> ab) <|> dgit

//elementary parsers for testing:

dgit 		= "digit" :> digit		<@ toString
	
letta		= symbolH 'a'			<@ const "a"
letta2		= "2nd a" :> symbol 'a'	<@ const "2a"

lettb		= symbolH 'b'			<@ toString
	
four		= symbolH '4'			<@ toString

wordstar	= tokenH ['***'] 		<@ toString

wordeqs		= tokenH ['==']			<@ toString

wordhaq		= tokenH ['##']			<@ toString

ab			= tokenH ['ab']			<@ toString

noThing		= "nothing" :> yield "made up"

anonymous	= token ['NN']
