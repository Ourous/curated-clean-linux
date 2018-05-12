implementation module Text.Parsers.ZParsers.ParsersAccessories

import Text.Parsers.ZParsers.ParsersKernel, Text.Parsers.ZParsers.ParsersDerived, Text.Parsers.ZParsers.ParserLanguage, StdEnv
from StdChar import isAlpha, isAlphanum, isHexDigit

number :: Parser  Char a Int
number = (<!+> digit) <@ foldl (\n d -> 10*n + digitToInt d) 0

number` :: Parser  Char a Int
number` = (<.*> digit) <@ foldl (\n d -> 10*n + digitToInt d) 0

digit :: Parser Char a Char
digit = satisfy (\c -> isMember c ['0'..'9']) 

hexDigit :: Parser Char a Char
hexDigit = satisfy isHexDigit

letter :: Parser Char a Char
letter = satisfy isAlpha 

alphaNum :: Parser Char a Char
alphaNum = satisfy isAlphanum 

oneOf :: [Char] -> Parser Char a Char
oneOf cs = satisfy (\c -> isMember c cs)  

choice :: ![Parser s t r] -> Parser s t r
choice l = foldl (<!>) fail l

ds :: !(Parser s t r) -> Parser s t r | space s
ds p = dropCheck space p

class space s :: !s -> Bool

instance space Char
where	space :: !Char -> Bool
		space c = isSpace c

symbolH :: (s -> Parser s t s) | ==,toString s
symbolH = toString :=> symbol

tokenH :: ([s] -> Parser s t [s]) | ==,toChar s
tokenH = toString :=> token

identifier :: Parser Char t String
identifier = satisfy isAlpha <&> \c -> <.*> (satisfy isAlphanum) <@ \r -> toString [c:r]

// Example: See Dutch Demo

/*	Computes line and column number, taking into account tabs and line breaks. Mind that tabs and
	line breaks are themselves characters in the input string and have a position.*/
		
lineAndColumn :: ![Char] !Int       // position returned by error msg
						 Int ->		// standard tab width
						 (Int,Int)	// line,column
lineAndColumn cs pos tab
	| pos < 1	= abort "ParserKernel.icl: position less than one"
	= lnc cs pos 1 1
where	lnc :: [Char]	Int			// position returned by error msg
						Int			// current line number
						Int ->		// current column number
						(Int,Int)	// line,column
		lnc [c:_] 1 line col = (line,col)
		lnc [c:cs] n line col = case c of
			'\n' -> lnc cs (n-1) (line+1) 1
			'\t' -> lnc cs (n-1) line     (col + tab - ((col-1) rem tab)) // rem was mod
			_	 -> lnc cs (n-1) line     (col+1)
		lnc []     n line col = abort "ParserKernel.icl: position beyond input-list"				

// START EXPLAIN RESULT

flattenSep :: String ![x] -> String | toString x
flattenSep s [a:rest=:[b:_]]	= (toString a)+++s+++(flattenSep s rest)
flattenSep _ [a]				= (toString a)
flattenSep _ _					= ""

errorToString :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> String
errorToString symbolTypes hypotheses position
	= flattenSep "\n" (errorToStrings symbolTypes hypotheses position)

simpleErrorToString :: SymbolTypes !(Rose (String,[SugPosition])) ![SugPosition] -> String
simpleErrorToString symbolTypes hypotheses position
	# loc	= "["+++ flattenSep "," (map toString position)+++"]: "
	# sugs	= fromRose hypotheses undef position
	= loc +++ flattenSep "/" (map snd sugs)

errorToStrings :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [String]
errorToStrings symbolTypes hypotheses position
	# elements	= errorToFormat symbolTypes hypotheses position
	= map (\(i,s) -> toString (repeatn (i*4) ' ') +++ s) elements

errorToFormat :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [(Int,String)]
errorToFormat symbolTypes hypotheses position
	# intro	= cantAnalyseAsOf +++ ":"
	# loc	= myZip symbolTypes position +++ "."
	# help	= if (isEmptyRose hypotheses) noOptions (optionsToSolve +++ ":")
	# sugs	= fromRose hypotheses 0 position
	= [(0,intro),(0,loc),(0,help):sugs]

myZip :: SymbolTypes [SugPosition] -> String
myZip syms ps
	# [s:ss]	 = reverse syms
	= toString s +++ myZip` ss ps 
where	myZip` []	   _		= ""
		myZip` [s:ss] [p:pp]	= ", "+++ toString s +++" "+++ toString p +++ myZip` ss pp
		myZip` _	  _			= abort "'myZip' in ParserAccessories called with unexpected combination of list lengths"

instance toString SymbolType
where	toString (Whole str)	= str 
		toString (Slice str i)	= toString i +++ sliceOf +++ str

instance toString SugPosition
where	toString (At i)		= toString i
		toString (EndAt i)	= ".." +++ toString i
		// ..45 should succinctly express that the error could be before or on position 45 

fromRose :: (Rose (String,[SugPosition])) Int [SugPosition] -> [(Int,String)]
fromRose []	_ _ = []
fromRose [RoseLeaf:rest] tab globalPos = fromRose rest tab globalPos
fromRose [RoseTwig (str,pos) down:rest] tab globalPos
	# (deeperIntro,deeperRose)
		= if (isEmptyRose down)
			("",[])
			(", " +++ andWithinThat +++ ":",fromRose down (tab+1) globalPos)
	= [(tab,str +++ pos2str pos globalPos +++ deeperIntro):deeperRose]++fromRose rest tab globalPos

pos2str :: [SugPosition] [SugPosition] -> String
pos2str sp globalPos
	| sp == globalPos	= ""	// do not display position if it equals the global error position
	= " ["+++ flattenSep "," (map toString sp)+++"]"

isEmptyRose :: (Rose a) -> Bool
isEmptyRose []				= True
isEmptyRose [RoseLeaf:as]	= isEmptyRose as
isEmptyRose _				= False

// END EXPLAIN RESULT
