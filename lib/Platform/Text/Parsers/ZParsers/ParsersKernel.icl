implementation module Text.Parsers.ZParsers.ParsersKernel

// ****************************************************************************************
//	Clean Library Module
//	Erik Zuurbier erik.zuurbier@planet.nl
//	Copyright 2005 HILT bv & Radboud University Nijmegen, The Netherlands
// ****************************************************************************************

import StdEnv

import Data.Maybe
from Data.Monoid import class Monoid
from Data.Func import $

from Text.Parsers.ZParsers.ParsersDerived import <&
from Text.Parsers.ZParsers.ParsersAccessories import class toString (..), instance toString SymbolType
from Text.Parsers.ZParsers.ParserLanguage import endOf
from Text.Parsers.ZParsers.ParsersDerived import <&, @>, yield, <++>

from Control.Monad import class Monad(bind), >>=
import Control.Applicative, Data.Functor, Data.Tuple

:: Parsable s       :== ([Int],Int,[s],SymbolTypes)
:: ParseResult s r  :== (Suggestions,[(Parsable s,r)])
:: BareParser s r   :== (Parsable s) -> ParseResult s r
:: Alt s t          :== (Xor s t) Suggestions -> ParseResult s t
:: Succ s r t       :== r (Alt s t) (Xor s t) Suggestions -> BareParser s t
:: Parser s t r     = Parser (PCont s r t)
:: PCont s r t      :== Hypothesis (Succ s r t) (Alt s t) (Xor s t) Suggestions -> BareParser s t
:: Xor s t          :== Suggestions -> ParseResult s t

runParser :: (Parser s t r) -> PCont s r t
runParser (Parser p) = p

empty_xc :: Xor s t
empty_xc = \sg -> (sg,[])

:: Suggestions  = (@) infix 9 (SymbolTypes,[Hypothesis]) HypPos
:: SymbolTypes  :== [SymbolType]    // in increasingly coarser units
:: SymbolType   = Whole String      // e.g. "letter", "word", "sentence","paragraph"
                | Slice String Int  // when atMost has cut off some

instance + Suggestions where
  (+) sg1=:((symTypes,hyp1)@i1) sg2=:((_,hyp2)@i2)
    | i1 > i2	= sg1
    | i1 < i2	= sg2
    = (symTypes,hyp1++hyp2) @ i1
    // if the positions are equal, the symTypes should also be equal, right?

:: Hypothesis :== [(String,HypPos)]
// String = description of where we are in the PARSER (not in the input!)

:: HypPos :== [Int]
	/*	Int above indicates HALF a position:
			EndAt	0th symbol -> 1
			At		1st symbol -> 2
			EndAt	1st symbol -> 3
			At		2nd symbol -> 4
		This odd construction (quite literally) is for reasonable error-reporting in checkIf */

posLength		=: 2
halfPosLength	=: 1
startPos		=: posLength
startErrorPos	=: [startPos]

//toPos :: Int -> Int
toPos i :== i<<1

//fromPos :: Int -> Int
fromPos i :== i>>1

toSugPos :: Int -> SugPosition
toSugPos i = let sp=fromPos i in if (isEven i) (At sp) (EndAt sp) 

:: SugPosition	= At Int | EndAt Int

toSugPos` :: (String,HypPos) -> (String,[SugPosition])
toSugPos` (str,hyPos) = (str,map toSugPos hyPos)

instance == SugPosition
where	(==) (At i)		sp	= case sp of
				(At j)		-> i==j
				else		-> False
		(==) (EndAt i)	sp	= case sp of
				(EndAt j)	-> i==j
				else		-> False

instance < SugPosition
where	(<) (At i)		sp	= case sp of
				(At j)		-> i<j
				(EndAt j)	-> i<j
		(<) (EndAt i)	sp	= case sp of
				(EndAt j)	-> i<j
				(At j)		-> i<=j

instance Functor (Parser s t) where
  fmap f a = f @> a

instance Applicative (Parser s t) where
  pure a        = yield a
  (<*>) fab fa  = fab <++> fa

instance Alternative (Parser s t) where
  empty         = fail
  (<|>) fa fa`  = alternative fa fa`

instance Monad (Parser s t) where
  bind ma a2mb  = ma <&> a2mb

:: Gram f a = Gram [PAlt f a] (Maybe a)

:: PAlt f a
  =  E.b: Seq (f (b -> a)) (Gram f b)
  |  E.b: Bind (f b) (b -> Gram f a)

instance Splittable (Parser s t) where
  getNonPure x = Just x
  getPure (Parser pc) = Nothing // TODO Just (Parser (\hy sc ac xc sg (is,i,[s:ss],symTypes) -> ))

//:: Parsable s       :== ([Int],Int,[s],SymbolTypes)
//:: ParseResult s r  :== (Suggestions,[(Parsable s,r)])
//:: BareParser s r   :== (Parsable s) -> ParseResult s r
//:: Alt s t          :== (Xor s t) Suggestions -> ParseResult s t
//:: Succ s r t       :== r (Alt s t) (Xor s t) Suggestions -> BareParser s t
//:: Parser s t r     = Parser (PCont s r t)
//:: PCont s r t      :== Hypothesis (Succ s r t) (Alt s t) (Xor s t) Suggestions (Parsable s) -> (Suggestions, [(Parsable s, r)])
//:: Xor s t          :== Suggestions -> ParseResult s t

//instance Monoid (Gram f (r -> r)) | Functor f where
  //mappend p q  = (\f g -> f o g) <$> p <||> q
  //mempty       = empty

mkG :: (f a) -> Gram f a | Splittable f & Functor f
mkG p = Gram  (maybe [] (\p -> [Seq (const <$> p) (pure ())]) (getNonPure p))
              (getPure p)

instance Functor (Gram f) | Functor f where
  fmap f (Gram alts e) = Gram (map (\x -> f <$> x) alts) (f <$> e)

instance Functor (PAlt f) | Functor f where
  fmap a2c (Seq fb2a gb)   = Seq ((\f -> a2c o f) <$> fb2a) gb
  fmap a2c (Bind fb b2ga)  = Bind fb (\b -> fmap a2c (b2ga b))

(<<||>) infixl 4 :: !(Gram f (b -> a)) (Gram f b) -> Gram f a | Functor f
(<<||>) gb2a=:(Gram lb2a eb2a) gb =
  let  (Gram _ eb) = gb
  in   Gram (map (\x -> fwdby1 x gb) lb2a) (eb2a <*> eb)

fwdby1 (Seq fc2b2a gc)   gb = Seq (uncurry <$> fc2b2a) ((\x y -> (x, y)) <$> gc <||> gb)
fwdby1 (Bind fc c2gb2a)  gb = Bind fc (l c2gb2a gb)
  where  // l :: (a -> Gram b (c -> d)) (Gram b c) a -> Gram b d | Functor b
         l :: .(.a -> .(Gram b (c -> d))) .(Gram b c) .a -> Gram b d | Functor b
         l c2gb2a gb c = c2gb2a c <||> gb

(<||>) infixl 4 :: !(Gram f (b -> a)) !(Gram f b) -> Gram f a | Functor f
(<||>) fb2a fb = fb2a <<||> fb <|> flip ($) <$> fb <<||> fb2a

instance Applicative (Gram f) | Functor f where
   pure a = Gram [] (Just a)
   (<*>) (Gram lb2a mb2a) gb =
     let (Gram lb mb) = gb
     in  Gram (map (\x -> fwdby2 x gb) lb2a ++ [b2a <$> fb \\ Just b2a <- [mb2a] & fb <- lb]) (mb2a <*> mb)

fwdby2 (Seq fc2b2a gc)   gb = Seq (uncurry <$> fc2b2a) ((\x y -> (x, y)) <$> gc <*> gb)
fwdby2 (Bind fc c2gb2a)  gb = Bind fc (l c2gb2a gb)
  where  // l :: (a -> Gram b (c -> d)) (Gram b c) a -> Gram b d | Functor b
         l :: .(.a -> .(Gram b (c -> d))) .(Gram b c) .a -> Gram b d | Functor b
         l c2gb2a gb c = c2gb2a c <||> gb

instance Alternative (Gram f) | Functor f where
  empty = Gram [] Nothing
  (<|>) (Gram ps pe) (Gram qs qe) = Gram (ps ++ qs) (pe <|> qe)

instance Monad (Gram f) | Functor f where
  bind (Gram lb mb) b2g_a =
    let bindto :: (PAlt f b) (b -> Gram f a) -> PAlt f a | Functor f
        bindto (Seq f_c2b g_c) b2g_a = Bind f_c2b (\c2b -> c2b <$> g_c >>= b2g_a)
        bindto (Bind f_c c2g_b) b2g_a = Bind f_c (\c -> c2g_b c >>= b2g_a)
        la = map (\x -> bindto x b2g_a) lb
    in  case mb of
          Nothing   -> Gram la Nothing
          (Just b)  -> let (Gram lra ma) = b2g_a b
                       in  Gram (la ++ lra) ma

mkP :: !(Gram f a) -> f a | Monad f & Applicative f & Alternative f
mkP (Gram l_a m_a) = foldr (<|>) (maybe empty pure m_a)
                                 (map mkP_Alt l_a)
  where  mkP_Alt (Seq f_b2a g_b)   = f_b2a <*> mkP g_b
         mkP_Alt (Bind f_b b2g_a)  = f_b >>= (mkP o b2g_a)

sepBy :: !(Gram f a) (f b) -> f a | Monad, Applicative, Alternative, *> f
sepBy g sep = mkP (insertSep sep g)

insertSep :: (f b) !(Gram f a) -> Gram f a | Monad, Applicative, Alternative, *> f
insertSep sep (Gram na ea) = Gram (map insertSepInAlt na) ea
   where insertSepInAlt (Seq fb2a gb)   = Seq fb2a (prefixSepInGram sep gb)
         insertSepInAlt (Bind fc c2ga)  = Bind fc (insertSep sep o c2ga)

prefixSepInGram :: (f b) (Gram f a) -> Gram f a | Monad, Applicative, Alternative, *> f
prefixSepInGram sep (Gram na ne)   = Gram (map (prefixSepInAlt sep) na) ne

prefixSepInAlt :: (f a) (PAlt f b) -> PAlt f b | Monad, Applicative, Alternative, *> f
prefixSepInAlt sep (Seq fb2a gb)   = Seq (sep *> fb2a) (prefixSepInGram sep gb)

gmList :: !(Gram f a) -> Gram f [a] | Functor f
gmList p = let pm = ((\x xs -> [x:xs]) <$> p <<||> pm) <|> pure [] in pm

fail :: Parser s t r
fail = Parser (\hy sc ac xc sg ss -> ac xc sg)

yield :: r -> Parser s t r
yield x = Parser (\hy sc ac xc -> sc x ac empty_xc)

opt :: (Parser s t r) r -> Parser s t r
opt p a = p <|> pure a

anySymbol :: Parser s t s
anySymbol = Parser p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes)	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,[]	 ,symTypes)	= ac xc (sg + (symTypes,[hy])@(is++[i]))

symbol :: s -> Parser s t s | == s
symbol x = Parser p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes) | x==s	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,_	 ,symTypes)			= ac xc (sg + (symTypes,[hy])@(is++[i]))


satisfy :: (s -> Bool) -> Parser s t s
satisfy ok = Parser p
where	p hy sc ac xc sg (is,i,[s:ss],symTypes) | ok s	= sc s ac empty_xc sg (is,i+posLength,ss,symTypes)
		p hy sc ac xc sg (is,i,_	 ,symTypes)			= ac xc (sg + (symTypes,[hy])@(is++[i]))

getPosition	:: Parser s t [Int]
getPosition = Parser (\hy sc ac xc sg ss=:(is,i,_,_) -> sc (map fromPos (is++[i])) ac empty_xc sg ss)

advancePosition :: !Int -> Parser s t r
advancePosition n = Parser p
where	p hy sc ac xc sg (is,i,ss,symTypes)
			# error					= ac xc (sg + (symTypes,[hy])@(is++[i]))
			| n<0					= error
			# (firstn,rest)			= splitAt n ss
	 		| length firstn == n	= sc u ac empty_xc sg (is,i+toPos n,rest,symTypes)
									= error
		u	= abort "undefined result of parser-constructor 'advancePosition' accessed"

getParsable :: Parser s t (Parsable s)
getParsable = Parser (\hy sc ac xc sg ss -> sc ss ac empty_xc sg ss)

setParsable :: (Parsable s) -> Parser s t r
setParsable ss = Parser (\hy sc ac xc sg ss` -> sc u ac empty_xc sg ss)
where	u	= abort "undefined result of parser-constructor 'setParsable' accessed"

token :: [s] -> Parser s t [s] | == s
token ss = Parser p
where	p hy sc ac xc sg (is,i,ssi,symTypes)
			# n						= length ss
			# (firstn,rest)			= splitAt n ssi
			| firstn == ss			= sc ss ac empty_xc sg (is,i+toPos n,rest,symTypes)
			= ac xc (sg + (symTypes,[hy])@(is++[i]))

epsilon :: Parser s t r
epsilon = Parser p
where	p hy sc ac xc sg (is,i,ss,symTypes)	= case ss of
			[]	-> sc u1 ac empty_xc sg u2
			_	-> ac xc (sg + (symTypes,[hy])@(is++[i]))
		u1	= abort "undefined result of parser-constructor 'empty' accessed" 
		u2	= abort "undefined rest-input of parser-constructor 'empty' accessed" 

// PARSER COMBINATORS:

//(<|>) infixr 4 :: (Parser s t r) (Parser s t r) -> Parser s t r
//(<|>) (Parser p1) (Parser p2) = Parser (\hy sc ac xc sg ss -> p1 hy sc (\xc3 sg` -> p2 hy sc ac xc3 sg` ss) xc sg ss)
alternative :: (Parser s t r) (Parser s t r) -> Parser s t r
alternative (Parser p1) (Parser p2) = Parser (\hy sc ac xc sg ss -> p1 hy sc (\xc3 sg` -> p2 hy sc ac xc3 sg` ss) xc sg ss)

(<&>) infixr 6 :: !(Parser s t u) (u -> Parser s t v) -> Parser s t v
(<&>) (Parser p) u2wp = Parser (\hy sc ac xc -> p hy (\t ac1 xc1 -> (runParser (u2wp t)) hy sc ac1 xc) ac xc)

(<++>) infixl 6 :: !(Parser s t (r->u)) !(Parser s t r) -> Parser s t u
(<++>) (Parser p1) (Parser p2) = Parser (\hy sc ac xc -> p1 hy (\f ac1 xc1 -> p2 hy (\rp -> sc (f rp)) ac1 xc) ac xc)

:: MonadicSeq s r u t = (<&->)  infixr 6 (Parser s t      u) (u -> Parser s t r)
:: ArrowSeq   s u r t = (<++->) infixl 6 (Parser s t (r->u)) (     Parser s t r)

(<!>) infixr 4 :: !(Parser s t r) !(Parser s t r) -> Parser s t r
(<!>) (Parser p1) (Parser p2) = Parser (\hy sc ac xc sg ss -> p1 hy sc (choose ac) (\sg` -> p2 hy sc ac xc sg` ss) sg ss)

instance Orr MonadicSeq where
  (<-!>) :: !(MonadicSeq s r u t) !(Parser s t r) -> Parser s t r
  (<-!>) ((Parser p1) <&-> u2wp) (Parser p2) = Parser (\hy sc ac xc sg ss -> p1 hy (\r ac1 xc1 -> (runParser (u2wp r)) hy sc ac1 xc)
    (choose ac) (\sg` -> p2 hy sc ac xc sg` ss) sg ss)

instance Orr ArrowSeq where
  (<-!>) :: !(ArrowSeq s r u t) !(Parser s t r) -> Parser s t r
  (<-!>) ((Parser p1) <++-> (Parser p2)) (Parser p3) = Parser (\hy sc ac xc sg ss -> p1 hy (\f ac1 xc1 -> p2 hy (\r2 -> sc (f r2)) ac1 xc)
    (choose ac) (\sg` -> p3 hy sc ac xc sg` ss) sg ss)

choose :: (Alt s r) (Xor s r) Suggestions -> ParseResult s r
choose ac xc sg
	# (res=:(sg,sol))	= xc sg
	| isEmpty sol		= ac empty_xc sg
						= res

// PARSER TRANSFORMERS:

first :: !(Parser s t r) -> Parser s t r
first (Parser p) = Parser (\hy sc ac xc sg -> p hy (\r ac` xc -> sc r ac empty_xc) ac xc sg)

/*	checkIf: Paying attention to the proper error message will make things work after all this
	way. Say you have a parser that subsequently takes two digits and finally checks whether
	they form a triple. Then the checkIf will get the level description "triple". If you then
	let it parse 3F, you get: "Error [2]: triple [1], and within that: digit"
	If you let it parse 32, you get "Error [..2]: triple [1]"
	So see triple the same way you would regard any other multi-level construct, such as
	an ifstatement, encompassing a condition and two other statements. Now triple is the more
	global level and two digits are the more detailed level. triple covers no ground of its own,
	so what?*/


	
(checkIf) infix 7 :: !(Parser s t r) (r -> Bool) -> Parser s t r
(checkIf) p test = p <&> checkIf` test

checkIf` :: (r -> Bool) r -> Parser s t r
checkIf` test r = Parser (wp r)
where	wp r hy sc ac xc sg ss=:(is,i,_,symTypes)
			| test r	= sc r ac empty_xc sg ss
			= ac xc (sg + (symTypes,[hy])@(is++[i-halfPosLength])) 

/*
The following is slightly compromising the idea in error reporting that the hypotheses should
just cumulate the actual alternatives on a certain position. The compromise is that
the test may now only name an alternative if (a part of) a test is violated.
*/
//(checkExplain) infix 7 :: (Parser s t r) (r -> Maybe String) -> Parser s t r
//(checkExplain) p test = p <&> checkExplain` test

//checkExplain` :: (r -> Maybe String) r -> Parser s t r
//checkExplain` test r = Parser (wp r)
//where	wp r hy sc ac xc sg ss=:(is,i,_,symTypes)
			//= case test r of
				//Nothing		= sc r ac empty_xc sg ss
//The following sneaks in the extra error message produced by the test
				//(Just hyl)	= ac xc (sg + (symTypes,[[(hyl,is++[i-halfPosLength])]:[hy]])@(is++[i-halfPosLength]))

rewind :: (Parser s t r) -> Parser s t r
rewind p = getParsable <&> \pp -> p <& setParsable pp

dropCheck :: (s -> Bool) !(Parser s t r) -> Parser s t r
dropCheck pred (Parser p) = Parser p2
where	p2 hy sc ac xc sg (is,i,ss,symTypes)
			# (ss,j)	= drop pred ss 0
			= p hy sc ac xc sg (is,i+j,ss,symTypes)

		drop :: (a->Bool) [a] Int -> ([a],!Int)
		drop pred all=:[a:as] i
			| pred a	= drop pred as (i+posLength)
			= (all,i)
		drop pred as i	= (as,i)

atMost :: !Int !(Parser s t r) -> Parser s t r
atMost n (Parser p) = (Parser p1)
where	p1 hy sc ac xc sg (is,i,ss,symTypes=:[word,sentence:rSymTypes])
			| n < 0			= ac xc (sg + (symTypes,[hy])@(is++[i]))
			# (firstn,rest)	= splitAt n ss
			# sc1			= \t ac xc` sg (js,j,ss,_) -> sc t ac empty_xc sg (js,j,ss++rest,symTypes)
			# symTypes		= if (length firstn == n) [word,slice sentence n:rSymTypes] symTypes
			= p hy sc1 ac xc sg (is,i,firstn,symTypes)
		p1 _ _ _ _ _ _		= abort "parser-transformer 'atMost' called with fewer than two symboltype-levels"
		
		slice :: SymbolType Int -> SymbolType
		slice (Whole str) n		= Slice str n
		slice (Slice str m) n	= Slice str (min n m)

// PARSER TRANSFORMERS THAT LEAVE AND RE-ENTER THE REALM OF CONTINUATIONS:

drill :: (Parser s r r) String -> Parser [s] t r
drill p subSymbol = Parser p1
where	p1 hy sc ac xc sg (is,i,ss`,symTypes=:[st:_])
			= yieldAll sc ac xc sugs (map f list)
			where	(s,ss)	= case ss` of
						[]		-> ([],[])
						[s:ss]	-> (s,ss)
					ss1	= (is++[i],startPos,s,[Whole subSymbol:symTypes])
                    end = endOf +++ " " +++ toString st
					(sugs,list) = (runParser (p <& end :> epsilon)) hy init_sc init_ac init_xc sg ss1
					init_sc		= \r ac xc sg ss -> let (sg`,rs) = ac xc sg in (sg`,[(ss,r):rs])
					init_ac		= \xc sg -> xc sg
					init_xc		= empty_xc
					f (parsable,res) = ((is,i+posLength,ss,symTypes),res)
		p1 hy sc ac xc sg _ = abort "parser-transformer 'drill' called with empty symbol types"

yieldAll :: (Succ s r t) (Alt s t) (Xor s t) Suggestions [(Parsable s,r)] -> ParseResult s t
yieldAll sc ac xc sg [(ss`,r):rs]	= sc r (\xc` sg` -> yieldAll sc ac xc` sg` rs) empty_xc sg ss`
yieldAll sc ac xc sg []				= ac xc sg

sortResultBy :: (r r -> Bool)  !(Parser s r r) -> Parser s t r
sortResultBy less (Parser p) = Parser p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (sortBy (\(_,r1) (_,r2) -> less r1 r2) rs)

minListByAll :: (a a -> Bool) !.[a] -> [a]
minListByAll less [a:as] = min1 a [] as
where	min1 a cum [b:bs]
			| less a b	= min1 a cum	 bs	// b too great: skip
			| less b a	= min1 b []		 bs	// b smaller: start a new cumulation 
						= min1 a [b:cum] bs	// equal: add to list of results
		min1 a cum []	= [a:cum]
minListByAll less [] = []

minResultBy :: (r r -> Bool) !(Parser s r r) -> Parser s t r
minResultBy less (Parser p) = Parser p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (minListByAll (\(_,r1) (_,r2) -> less r1 r2) rs)

fstLonger :: (Parsable s,r) (Parsable s,r) -> Bool
fstLonger ((is1,i1,_,_),_) ((is2,i2,_,_),_) = is1 == is2 && i1 > i2 || is1 > is2

longest :: !(Parser s r r) -> Parser s t r
longest (Parser p) = Parser p1
where	p1 hy sc ac xc sg ss
			# (sg`,rs)	= parse` p hy sg ss
			= yieldAll sc ac xc sg` (minListByAll fstLonger rs)

// FOR ERROR REPORTING:

// To add a hypothesis level to a parser
/*	It adds a describing string and the starting-position to the hypothesis,
	so that you can get a suggestion like:
	
	in the 'function definition' that starts at position 34, and within that
	in the 'assignment statement' that starts at position 45, and within that
	in the 'expression' that starts at position 56, and within that
	at position 61 the parser expects either a '(' or a number.    */

(:>) infixl 8 :: String !(Parser s t r) -> Parser s t r
(:>) hyl (Parser p) = Parser (\hy sc ac xc sg ss=:(is,i,_,_) -> p [(hyl,is++[i]):hy] sc ac xc sg ss)

/*	To add a hypothesis level to a wanting parser, with the possibility to use the wanted
	result in the hypothesis level description */
(:=>) infixl 8 :: (r -> String) (r -> Parser s t r) -> (r -> Parser s t r)
(:=>) whyl wp = \r -> whyl r :> wp r	// whyl = wanting hypothesis level

// FOR APPLYING A PARSER TO AN INPUT

:: Result r	= Err SymbolTypes (Rose (String,[SugPosition])) [SugPosition] | Succ [r]

:: RoseNode a = RoseLeaf | RoseTwig a (Rose a)
	// RoseLeaf indicates where a twig ends, so the following are indeed different
	// [RoseTwig 1 [RoseTwig 3 [RoseLeaf,RoseTwig 4 [RoseLeaf]]]]
	// this contains a path [1,3] and a path [1,3,4] (both are closed by a RoseLeaf)
	
:: Rose a :== [RoseNode a]

toRose :: [[a]] -> Rose a | == a
toRose as = foldl addPath [] as
where	addPath :: (Rose a) [a] -> Rose a | == a
		addPath []					 [a:as]			 = [RoseTwig a (addPath [] as)] 
		addPath rose				 []	   			 = [RoseLeaf:rose]
		addPath [RoseTwig b bs:rest] [a:as] | a == b = [RoseTwig b (addPath bs as):rest]
		addPath [e	 		  :rest] as				 = [e:addPath rest as]

//parse` for internal use only

parse` :: !(PCont s r r) Hypothesis Suggestions (Parsable s) -> ParseResult s r
parse` p hy sg ss = p hy sc ac xc sg ss
where	sc :: Succ s r r			// final success-continuation
		sc	= \r ac xc sg ss -> let (sg`,rs) = ac xc sg in (sg`,[(ss,r):rs])
		ac :: Alt s r				// final alternative continuation
		ac	= \xc sg -> xc sg
		xc :: Xor s r				// final xor continuation
		xc	= empty_xc

parse :: !(Parser s r r) [s] String String -> Result r
parse (Parser p) syms sentence word = prettify (parse` p hy sg ([],startPos,syms,symTypes))
where	hy :: Hypothesis
		hy	= []
		
		sg :: Suggestions
		sg	= (symTypes,[])@startErrorPos
		
		symTypes :: [SymbolType]
		symTypes = [Whole word,Whole sentence]

		prettify :: (ParseResult s r) -> Result r
		prettify (sug,[])
			# ((symTypes,hyps)@i) = sug	// do not pattern-match in sug - that would be too strict
			# hyps	= map reverse hyps
					/*	hyps :: [[(String,[Int])]]
						one Hypothesis :: [(String,[Int]] is a path; the complete hyps is a number
						of hypotheses, so hyps is a list of lists. Because individual paths grow
						from coarse to fine by prepending ever more detailed parse-items, paths
						must be reversed for error reporting. */
			# hyps	= map (map toSugPos`) hyps
					/*	Go inside each hypothesis and convert each Int to a
						SugPosition: 1 -> EndAt 0, 2 -> At 1, 3 -> EndAt 1 etc. */
			= Err symTypes (toRose hyps) (map toSugPos i)
					/*	Factor out prefixes (making a Rose) and convert error position to
						End / At notation. The error position list is already in the right order */
		prettify (_,suc)	= Succ (map snd suc)
