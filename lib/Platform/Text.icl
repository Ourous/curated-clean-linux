implementation module Text

import StdOverloaded, StdString, StdArray, StdChar, StdInt, StdBool, StdClass, StdList
import Data.List

instance Text String
	where
	textSize :: !String -> Int
	textSize s = size s	

	concat :: ![String] -> String
	concat xs = concat` xs (createArray (foldl (\s a -> s+size a) 0 xs) '\0') 0
		where
        concat` :: ![String] !*String !Int -> *String
		concat` []     dst _		= dst
		concat` [x:xs] dst offset	= concat` xs (copyChars offset 0 (size x) x dst) (offset + size x)

		copyChars :: !Int !Int !Int !String !*String -> *String
		copyChars offset i num src dst
		| i == num		= dst
		| otherwise		= copyChars offset (inc i) num src {dst & [offset + i] = src.[i]}

	split :: !String !String -> [String]
	split sep s = splitAfter 0 (size s-1) sep s
	where
		splitAfter :: !Int !Int !String !String -> [String]
		splitAfter offs end sep s
			# index = indexOfAfter offs sep s
			| index == -1	= [s%(offs,end)]
							= [s%(offs,index-1) : splitAfter (index+size sep) end sep s]

	join :: !String ![String] -> String
	join sep xs = concat (join` sep xs)
	where
		join` :: !String ![String] -> [String]
		join` sep [] = []
		join` sep [x] = [x]
		join` sep [x:xs] = [x, sep : join` sep xs]

    indexOf :: !String !String -> Int
	indexOf needle haystack = indexOfAfter 0 needle haystack

    lastIndexOf :: !String !String -> Int
	lastIndexOf needle haystack
		| size needle==0
			= -1
			= lastIndexOf` (size haystack - size needle) needle.[0] needle haystack
		where
		lastIndexOf` :: !Int !Char !{#Char} !{#Char} -> Int
		lastIndexOf` offs needleChar0 needle haystack
			| offs>=0
				| haystack.[offs]<>needleChar0
					= lastIndexOf` (offs - 1) needleChar0 needle haystack
					= equalStringOrIndexOfPrevious 1 offs needle haystack
				= -1		

		equalStringOrIndexOfPrevious :: !Int !Int !{#Char} !{#Char} -> Int
		equalStringOrIndexOfPrevious i offs needle haystack
			| i<size needle
				| needle.[i]==haystack.[i+offs]
					= equalStringOrIndexOfPrevious (i+1) offs needle haystack
					= lastIndexOf` (offs - 1) needle.[0] needle haystack
				= offs
																					
	indexOfAfter :: !Int !String !String -> Int
	indexOfAfter offs needle haystack
		| size needle==0
			= -1
			= indexOf` offs needle.[0] (size haystack - size needle) needle haystack
		where
		indexOf` :: !Int !Char !Int !{#Char} !{#Char} -> Int
		indexOf` offs needleChar0 max_offs needle haystack
			| offs <= max_offs
				| haystack.[offs]<>needleChar0
					= indexOf` (offs + 1) needleChar0 max_offs needle haystack
					= equalStringOrIndexOfNext 1 offs max_offs needle haystack
				= -1

		equalStringOrIndexOfNext :: !Int !Int !Int !{#Char} !{#Char} -> Int
		equalStringOrIndexOfNext i offs max_offs needle haystack
			| i<size needle
				| needle.[i]==haystack.[i+offs]
					= equalStringOrIndexOfNext (i+1) offs max_offs needle haystack
					= indexOf` (offs + 1) needle.[0] max_offs needle haystack
				= offs

    startsWith :: !String !String -> Bool
	startsWith needle haystack
		#! s_needle = size needle
		= s_needle <= size haystack && needle == haystack%(0,s_needle-1)

    endsWith :: !String !String -> Bool
	endsWith needle haystack
		#! s_needle   = size needle
		#! s_haystack = size haystack
		= s_needle <= s_haystack && needle == haystack%(s_haystack-s_needle,s_haystack-1)

    subString :: !Int !Int !String -> String
	subString start len haystack = haystack % (start, start + len - 1)

	replaceSubString :: !String !String !String -> String
	replaceSubString needle replacement haystack
		#! index = indexOf needle haystack
		| index == -1 = haystack
		| otherwise
			#! start = subString 0 index haystack
			#! end   = subString (index + size needle) (size haystack) haystack
			= start +++ replacement +++ (replaceSubString needle replacement end)

    trim :: !String -> String
	trim s = ltrim (rtrim s)

	ltrim :: !String -> String
	ltrim s
		#! non_space_index     = non_space_left 0
		| non_space_index == 0 = s
		| otherwise            = s%(non_space_index,size_s-1)
	where
		size_s = size s
		
		non_space_left :: !Int -> Int
		non_space_left i
			| i < size_s && isSpace s.[i]	= non_space_left (i+1)
											= i

	rtrim :: !String -> String
	rtrim s
		#! non_space_index	= non_space_right (size_s-1)
		| non_space_index == size_s-1
						= s
						= s%(0,non_space_index)
	where
		size_s			= size s
		
		non_space_right :: !Int -> Int
		non_space_right i
			| i >= 0 && isSpace s.[i]	= non_space_right (i-1)
										= i

	lpad :: !String !Int !Char -> String
	lpad s w  c
		#! boundary = w - size s
        = {if (i < boundary) c s.[i - boundary] \\ i <- [0.. w - 1]}
	
	rpad :: !String !Int !Char -> String
    rpad s w c
    	#! boundary = size s
        = {if (i < boundary) s.[i] c \\ i <- [0.. w - 1]}

    toLowerCase :: !String -> String
	toLowerCase s = {toLower c \\ c <-: s}

    toUpperCase :: !String -> String
	toUpperCase s = {toUpper c \\ c <-: s}
	
	upperCaseFirst :: !String -> String
	upperCaseFirst "" = ""
	upperCaseFirst s = s:=(0,toUpper s.[0])
	
	dropChars :: !Int !String -> String	
	dropChars n s = s % (n, n + size s - n - 1)

instance Text [Char]
where
	textSize :: ![Char] -> Int
	textSize cs = length cs

	concat :: ![[Char]] -> [Char]
	concat css = flatten css

	split :: ![Char] ![Char] -> [[Char]]
	split by cs = case indexOf by cs of
		-1 -> [cs]
		i  -> [take i cs:split by (drop (i + length by) cs)]

	join :: ![Char] ![[Char]] -> [Char]
	join g cs = concat (intersperse g cs)

	indexOf :: ![Char] ![Char] -> Int
	indexOf []  _  = -1
	indexOf sub cs = index 0 sub cs
	where
		index :: !Int ![Char] ![Char] -> Int
		index n sub cs
		| startsWith sub cs = n
		| otherwise         = case cs of
			[]     -> -1
			[_:cs] -> index (n+1) sub cs

	lastIndexOf :: ![Char] ![Char] -> Int
	lastIndexOf sub cs = case indexOf (reverse sub) (reverse cs) of
		-1 -> -1
		i  -> length cs - length sub - i

	indexOfAfter :: !Int ![Char] ![Char] -> Int
	indexOfAfter i sub cs = case indexOf sub (drop i cs) of
		-1 -> -1
		j  -> i + j

	startsWith :: ![Char] ![Char] -> Bool
	startsWith []     _      = True
	startsWith [x:xs] [c:cs] = x == c && startsWith xs cs
	startsWith _      _      = False

	endsWith :: ![Char] ![Char] -> Bool
	endsWith xs cs
	| lcs < lxs = False
	| otherwise = xs == drop (lcs - lxs) cs
	where
		lcs = length cs
		lxs = length xs

	subString :: !Int !Int ![Char] -> [Char]
	subString start length cs = take length (drop start cs)

	replaceSubString :: ![Char] ![Char] ![Char] -> [Char]
	replaceSubString repl by cs = join by (split repl cs)

	trim :: ![Char] -> [Char]
	trim cs = ltrim (rtrim cs)

	ltrim :: ![Char] -> [Char]
	ltrim cs = dropWhile isSpace cs

	rtrim :: ![Char] -> [Char]
	rtrim cs = reverse (ltrim (reverse cs))

	lpad :: ![Char] !Int !Char -> [Char]
	lpad cs n p = repeatn (max 0 (n - length cs)) p ++ cs

	rpad :: ![Char] !Int !Char -> [Char]
	rpad cs n p = cs ++ repeatn (max 0 (n - length cs)) p

	toLowerCase :: ![Char] -> [Char]
	toLowerCase cs = map toLower cs

	toUpperCase :: ![Char] -> [Char]
	toUpperCase cs = map toUpper cs

	upperCaseFirst :: ![Char] -> [Char]
	upperCaseFirst []     = []
	upperCaseFirst [c:cs] = [toUpper c:cs]

	dropChars :: !Int ![Char] -> [Char]
	dropChars n cs = drop n cs

instance + String
where
	(+) s1 s2 = s1 +++ s2

(<+) infixr 5 :: !a !b -> String | toString a & toString b
(<+) x y = toString x + toString y
