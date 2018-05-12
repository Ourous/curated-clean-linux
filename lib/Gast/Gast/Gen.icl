implementation module Gast.Gen

/*
	GAST: A Generic Automatic Software Test-system
	
	gen: generic generation of values of a type

	Pieter Koopman, 2004 - 2017
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, Math.Random, Data.Maybe, Data.Functor
from Data.Set import :: Set
import qualified Data.Set as Set
from Data.Map import :: Map, instance Functor (Map k)
import qualified Data.Map as Map
from Data.Func import $

// -------
aStream :: RandomStream
aStream = genRandInt 42

splitRandomStream :: !RandomStream -> (RandomStream,RandomStream)
splitRandomStream [r,s:rnd]
	# seed = r*s
	| seed == 0
		= splitRandomStream rnd
		= (genRandInt seed, rnd)
splitRandomStream _ = abort "Gast.Gen.icl: the increadable has been done, you have used all random values!"

// -------

randomize :: ![a] [Int] Int ([Int] -> [a]) -> [a]
randomize list rnd n c = rand list rnd n []
where
	rand [] rnd m [] = c rnd
	rand [] rnd m [x] = [x:c rnd]
	rand [] rnd m l = rand l rnd n []
	rand [a:x] [i:rnd] m l
		| m==0 || (i rem m) == 0
			= [a:rand x rnd (m-1) l]
			= rand x rnd m [a:l]
// -------

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

genState :: GenState
genState =
	{ depth                 = 0
	, maxDepth              = maxint
	, path                  = []
	, skewl                 = 1
	, skewr                 = 3
	, recInfo               = 'Map'.newMap
	, pairTree              = PTLeaf
	, recFieldValueNrLimits = 'Map'.newMap
	}

// ================= skew generation ================= //
directNames :: GenericTypeDefDescriptor -> Set TypeName
directNames gtd = 'Set'.fromList (foldr scan [] gtd.gtd_conses)
where
	scan gcd = allTypes (init (types gcd.gcd_type)) // init removes the type itself, as required.

	types :: GenType -> [GenType]
	types (GenTypeArrow s t) = types s ++ types t
	types t = [t]

	allTypes :: [GenType] [TypeName] -> [TypeName]
	allTypes [GenTypeArrow s t:r] c	= allTypes [s] (allTypes [t] (allTypes r c))
	allTypes [GenTypeApp   s t:r] c	= allTypes [s] (allTypes [t] (allTypes r c))
	allTypes [GenTypeCons name:r] c = [name:allTypes r c]
	allTypes [GenTypeVar     v:r] c = allTypes r c
	allTypes []                   c = c

addRecInfo :: TypeName (Set TypeName) (Map TypeName (Set TypeName)) -> Map TypeName (Set TypeName)
addRecInfo name names ri
	# ri =  (\types -> if ('Set'.member name types) ('Set'.union types names) types) <$> ri
    = 'Map'.alter (Just o maybe names ('Set'.union names)) name ri

recArgs :: TypeName (Map TypeName (Set TypeName)) GenType -> [Bool]
recArgs typeName ri genType = scanArgs genType
where
	scanArgs :: GenType -> [Bool]
	scanArgs (GenTypeArrow t1 t2) = [recCount t1:scanArgs t2]
	scanArgs genType = []
	
	recCount :: GenType -> Bool
	recCount (GenTypeApp t1 t2)   = recCount t1 || recCount t2
	recCount (GenTypeArrow t1 t2) = recCount t1 || recCount t2
	recCount (GenTypeCons name)   = maybe False ('Set'.member typeName) ('Map'.get name ri)
	recCount genType              = False

genPairTree :: [Bool] Int -> PairTree
genPairTree l   n
	| n<2
		= PTLeaf
		= PTNode (genPairTree l1 m) (or l1) (or l2) (genPairTree l2 n2)
where
	m = n/2
	n2 = n-m
	(l1,l2) = splitAt m l

// ================= generic generation ================= //

ggen{|Int|}  s = [0: interleave [i \\ n <- [1..(s.maxDepth - s.depth)], i <- [n,~n]] (if (s.maxDepth == maxint) [maxint,minint,maxint-1,minint+1] [])]
ggen{|Bool|} s = [False,True]
ggen{|Char|} s = take (s.maxDepth - s.depth) (interleave ['a'..'~'] (interleave ['A'..'`'] (interleave (map toChar [32..64]) ['\t\n\r'])))
ggen{|Real|} s
	| s.maxDepth < maxint
		= takeWhile (\r -> abs r <= toReal (s.maxDepth - s.depth)) l
		= l
where
	l = [0.0
		:interleave
			[r \\ x <- diag s.skewl s.skewr [1:prims] [1:prims] (\n d.toReal n/toReal d), r <- [x,~ x]]
		(interleave
			[r \\ x <- map sqrt [2.0..], r <- [x, ~x]]
			(if (s.maxDepth == maxint)
				[5.0E-324, 2.2250738585072009E-308, 2.2250738585072014E-308,maxDouble: largeReals (maxDouble/2.0)] // double precision
				[]
			)
		)
		]
	maxDouble = 1.7976931348623157E308
	largeReals r
		| r < 10.0
			= []
			= [r, 0.0 - r: largeReals (r/2.0)]
	prims = sieve [2..]
	sieve [p:xs] = [p: sieve [x \\ x <- xs | x rem p <> 0]]

ggen{|UNIT|} s = [UNIT]
ggen{|PAIR|} f g s
	= case s.pairTree of
		PTNode ptl sl sr ptr = diag (if sl s.skewr s.skewl) (if sr s.skewr s.skewl) (f {s & pairTree = ptl}) (g {s & pairTree = ptr}) PAIR
		_ = abort "ggen{|PAIR|}: invalid pairTree: PTNode"

ggen{|EITHER|} f g s
		# path	= s.path
		| isEmpty path
			= interleave  (map LEFT (f s)) (map RIGHT (g s))
			# s	= { s & path = drop 1 path }
			  gs = map RIGHT (g s)
			  fs = map LEFT (f s)
			= case path of
				[ConsRight:_]	= interleave gs fs
				_				= interleave fs gs

ggen{|CONS of gcd|} f s
	= map CONS (f {s & pairTree = pairTree})
where
	typeName	= gcd.gcd_type_def.gtd_name
	type		= gcd.gcd_type
	recCount	= recArgs typeName s.recInfo type
	pairTree	= genPairTree recCount gcd.gcd_arity
ggen{|OBJECT of gtd|} f s
	| s.depth >= s.maxDepth
		= []
		= [OBJECT o \\ o <- (f {s & depth = s.depth + 1, path = path, recInfo = ri2})]
where
	path = hd (	[	getConsPath gcd
				\\	gcd <- sortBy argCount
					(filter (\gcd -> not (or (recArgs gtd.gtd_name s.recInfo gcd.gcd_type))) gtd.gtd_conses)
				] ++ [[]])
	argCount gcd1 gcd2 = gcd1.gcd_arity < gcd2.gcd_arity
	ri2 = addRecInfo gtd.gtd_name (directNames gtd) s.recInfo
	
ggen{|RECORD of grd|} f s
	= map RECORD (f {s & pairTree = pairTree})
where
	typeName	= grd.grd_name
	type		= grd.grd_type
	recCount	= recArgs typeName s.recInfo type
	pairTree	= genPairTree recCount grd.grd_arity

ggen{|FIELD of d|}  f s = [FIELD fi \\ fi <- vals]
where
    vals = case 'Map'.get (d.gfd_cons.grd_name, d.gfd_name) s.recFieldValueNrLimits of
        Nothing    -> f s
        Just limit -> take limit $ f s

ggen{|String|} s = ["hello world!","Gast": rndStrings 0 aStream]
where
	rndStrings 0 rnd = ["": rndStrings 1 rnd]
	rndStrings len [r,s:rnd] 
		# (chars,rnd)	= seqList (repeatn ((abs r) rem len) genElem) rnd
		  string		= {c \\ c<-chars}
		= [string:rndStrings ((len rem StrLen)+1) rnd]
	where
		genElem :: RandomStream -> .(Char, RandomStream)
		genElem [r:rnd] = (toChar (32+((abs r) rem 94)), rnd)

derive ggen (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive ggen [], [!], [ !], [!!]
ggen{|{}|}  fx r = [{x \\ x <- xs} \\ xs <- ggen{|*->*|} fx r]
ggen{|{!}|} fx r = [{x \\ x <- xs} \\ xs <- ggen{|*->*|} fx r]

interleave :: [a] [a] -> [a]
interleave [x:xs] ys = [x: interleave ys xs]
interleave []     ys = ys

diag :: !Int !Int [a] [b] (a b-> c) -> [c]
diag skewl skewr as bs f = skew skewl [] [] [[f a b \\ a <- as] \\ b <- bs]
where
	skew :: Int [[a]] [[a]] [[a]] -> [a]
	skew n [[a:as]:ass] bs cs     = [a: if (n>1) (skew (n-1) [as:ass] bs cs) (skew skewl ass [as:bs] cs)]
	skew n [[]    :ass] bs cs     = skew skewl ass bs cs
	skew n []           [] []     = []
	skew n []           bs cs     = skew skewl (rev bs cs1) [] cs2 where (cs1,cs2) = splitAt (max skewr 1) cs
 
 	rev :: [a] [a] -> [a]
	rev [a:as] bs = rev as [a:bs]
	rev []     bs = bs

derive bimap []
