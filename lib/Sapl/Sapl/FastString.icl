implementation module Sapl.FastString

import StdEnv 

skipChars :: !String !Int (Char -> Bool) -> Int
skipChars line start f
	| start == size line
		= size line
	#! c = line.[start]
	| f c
		= skipChars line (start + 1) f 
		= start

startsWith :: !String !String -> Bool
startsWith start str
	= size str >= size start && startsWith_ ((size start)-1)
where
	startsWith_ -1 = True
	startsWith_ starti
		= start.[starti] == str.[starti] && startsWith_ (starti-1)
					 
endsWith :: !String !String -> Bool
endsWith end str
	= size str >= size end && endsWith_ (size end-1) (size str-1)
where
	endsWith_ -1 _ = True
	endsWith_ endi stri 
		= end.[endi] == str.[stri] && endsWith_ (endi-1) (stri-1)

charIndex :: !String !Int !Char -> (!Bool,!Int)
charIndex s i char
	| i < size s
		| s.[i] == char
			= (True,i)
			= charIndex s (i+1) char
	| i == size s
		= (False,size s)
		= abort "CharIndex: index out of range"
					
charIndexBackwards :: !String !Int !Char -> (!Bool,!Int)
charIndexBackwards s i char
	| i == -1
		= (False,size s)
		
		| s.[i] == char
			= (True,i)
			= charIndexBackwards s (i-1) char
			
matchAt :: !String !String !Int -> Bool
matchAt s1 s2 p 
	= size s1 + p <= size s2 && match_at 0 p
	where
		match_at :: !Int !Int -> Bool
		match_at i1 i2
			| i1<size s1
				= s1.[i1]==s2.[i2] && match_at (i1+1) (i2+1)
				= True

countCharBackwards :: !Char !String -> Int
countCharBackwards chr str 
	= count ((size str)-1) 0 str
where
	count :: !Int !Int !String -> Int
	count -1 n_chars str
		= n_chars
	count pos n_chars str
		| str.[pos] == chr
			= count (pos-1) (n_chars+1) str
			= n_chars
