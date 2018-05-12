implementation module Sapl.SaplTokenizer

import StdEnv, Sapl.FastString, Text.Unicode, Text.Unicode.Encodings.JS
from Text.Unicode.UChar import instance toInt UChar, instance fromChar UChar, instance fromInt UChar

matchCharAt c1 s1 p 
	:== s1.[p] == c1

is_stopchar :: !Char -> Bool
is_stopchar char 
	= (char == '=') || (char == ':') || (char == ')') || (char == '(') || 
	  (char == '|') || (char == '{') || (char == '}') || (char == ',') || 
	  (char == '[') || (char == ']') || isSpace char

not_stopchar = not o is_stopchar
is_space c = isSpace c && not_eol c
not_eol c = not (c == '\n')

find_first_string :: !String !Int (Char String Int -> Bool) -> Int
find_first_string line start f
	| start == size line
		= size line
	| f line.[start-1] line start
		= find_first_string line (start + 1) f 
		= start

read_string_lit :: !Char !Int !String -> (!Int, !UString)
read_string_lit qc start str 
		# (nextbase, l) = decode start []
		= (nextbase, reverse l)
where
	decode p cs
		| eof p			 	= (p, cs)	
		| str.[p] == qc 	= (p+1, cs)
		| str.[p] == '\\' 	= let (n,c) = decodeBSChar (p+1) in decode n [c:cs]
							= decode (p+1) [fromChar str.[p]:cs]
	
	decodeBSChar p
		| eof p 			= (p  ,fromChar '\\')
		| str.[p] == '0'	= (p+1,fromInt 0)
		| str.[p] == 'a'	= (p+1,fromInt 7)
		| str.[p] == 'b'	= (p+1,fromChar '\b')
		| str.[p] == 'f'	= (p+1,fromChar '\f')		
		| str.[p] == 'n'	= (p+1,fromChar '\n')
		| str.[p] == 'r'	= (p+1,fromChar '\r')		
		| str.[p] == 't'	= (p+1,fromChar '\t')
		| str.[p] == 'v'	= (p+1,fromChar '\v')
		| str.[p] == '\''	= (p+1,fromChar '\'')
		| str.[p] == '"'	= (p+1,fromChar '"')
		| str.[p] == 'x'	= decodeHex 2 2 0
		| str.[p] == 'u'	= decodeHex 4 4 0					
		| str.[p] == 'U'	= decodeHex 8 8 0
							= (p+1,fromChar str.[p])	// skip the backslash otherwise
	where
		decodeHex :: !Int !Int !Int -> (!Int, !UChar)
		decodeHex len s acc
			| eof (p+len)	= (p+1, fromChar str.[p])
							= decodeHex` len s acc
		
		decodeHex` :: !Int !Int !Int -> (!Int, !UChar)
		decodeHex` len s acc 
			| s == 0			   = (p+len+1, fromInt acc)			
			| isHexDigit str.[p+s] = decodeHex` len (s-1) (acc+decodeTable.[len-s]*dti str.[p+s])
								   = (p+1, fromChar str.[p])
	
	eof p = p >= size str
	
	decodeTable :: {#Int}
	decodeTable = {1,16,256,4096,65536,1048576,16777216,268435456}
	
	dti :: !Char -> Int
	dti c 
		| c >= '0' && c <= '9' = toInt (c - '0')
		| c >= 'a' && c <= 'f' = 10 + toInt (c - 'a')
		| c >= 'A' && c <= 'F' = 10 + toInt (c - 'A')

read_token :: !Int !String -> (!Int, !Int, !Token)
read_token base line
	| start > ((size line)-1) 
		= rnoarg TEndOfLine 0	
	| matchCharAt '\n' line start
		= rnoarg TEndOfLine 1
	// Skip <{ and }> from the identifier. It's to help parsing "strange" function names
	| matchAt "!<{" line start
		# stop = find_first_string line (start+3) (\prev str base = not (matchAt "}>" str base))
		= return (TStrictIdentifier (line % (start + 3, stop - 1)), stop + 2)		
	| matchAt "<{" line start
		# stop = find_first_string line (start+2) (\prev str base = not (matchAt "}>" str base))
		= return (TIdentifier (line % (start + 2, stop - 1)), stop + 2)		
	| matchAt "=:" line start
		= rnoarg TCAFAssignmentOp 2
	| matchAt "::" line start
		= rnoarg TTypeDef 2		
	| matchAt "||" line start // skip comment
		# stop = (skipChars line (start+2) not_eol)
		= read_token stop line // skip, but leave the EOL there
	| matchCharAt '|' line start
		= rnoarg TVerticalBar 1		
	| matchCharAt '=' line start 
		= rnoarg TAssignmentOp 1
	| matchAt ":==" line start
		= rnoarg TMacroAssignmentOp 3
	| matchAt "->" line start
		= rnoarg TCaseAssignmentOp 2		
	| matchCharAt ',' line start 
		= rnoarg TComma 1
	| matchCharAt ':' line start 
		= rnoarg TColon 1		
	| matchCharAt '(' line start 
		= rnoarg TOpenParenthesis 1
	| matchCharAt ')' line start 
		= rnoarg TCloseParenthesis 1
	| matchCharAt '{' line start 
		= rnoarg TOpenBracket 1
	| matchCharAt '}' line start 
		= rnoarg TCloseBracket 1
	| matchCharAt '[' line start 
		= rnoarg TOpenSquareBracket 1
	| matchCharAt ']' line start 
		= rnoarg TCloseSquareBracket 1
	| matchCharAt '"' line start
		# (nextbase,ustr) = read_string_lit '"' (start+1) line
		= return (TLit (LString ustr), nextbase)
	| matchCharAt '\'' line start 
		# (nextbase,ustr) = read_string_lit '\'' (start+1) line
		= return (TLit (LChar ustr), nextbase)
	| matchCharAt '+' line start 
		= numberToken 1
	| matchCharAt '-' line start 
		= numberToken 1
// This is not a number!
//	| startsWith "." lline
//		= numberToken 0
	| isDigit (line.[start])
		= numberToken 0
	| otherwise
		# stop = skipChars line start not_stopchar
		= case tstr stop of
			"False"  = return (TLit (LBool False), stop)
			"false"  = return (TLit (LBool False), stop)
			"True"   = return (TLit (LBool True), stop)
			"true"   = return (TLit (LBool True), stop)
			"case"   = return (TCaseKeyword, stop)
			"select" = return (TSelectKeyword, stop)
			"update" = return (TUpdateKeyword, stop)
			"let"	 = return (TLetKeyword, stop)
			"in"	 = return (TInKeyword, stop)
			str		 = if (str.[0] == '!') 
							(return (TStrictIdentifier (str % (1, size str)), stop))
							(return (TIdentifier str, stop))
where
	tstr stop = line % (start, stop - 1)
	start = (skipChars line base is_space)
	
	rnoarg t length = (start, start + length, t)	
	return (a, newbase) = (start, newbase, a)
	
	numberToken p1
		# fpart = skipChars line (start+p1) isDigit
		# (dot, stop) = if ((size line) > fpart && line.[fpart] == '.') 
			(True, skipChars line (fpart+1) isDigit) (False, fpart)
		# (exp, stopexp) = readExp stop
		= return (TLit (if (dot || exp <> 0) 
					(LReal (toReal (tstr stop) * (10.0 ^ toReal (exp))))
					(LInt (toInt (tstr stop)))), stopexp)	
	
	readExp start
		| size line < start + 2
		= (0, start)
		| line.[start] <> 'e' && line.[start] <> 'E'
		= (0, start)
		# stop = skipChars line (start + 1 + signskip) isDigit
		= (sign * (toInt (line % (start + 1 + signskip, stop - 1))), stop)
	where
		sign = case line.[start+1] of
				'+' = 1
				'-' = -1
					= 1
		signskip = if (isDigit line.[start+1]) 0 1
	
tokensWithPositions :: !String -> [PosToken]
tokensWithPositions inp = tokens_ 1 1 0 [] 
where 
	finalise _ _ ts=:[PosToken _ _ TEndOfLine:_] = reverse ts
	finalise lp cp ts = reverse [PosToken lp cp TEndOfLine:ts]
	
	tokens_ lp cp base ts 
			= case base < (size inp) of
				True = let (cp2, newbase, t) = read_token base inp in 
							case t of
								TEndOfLine  = tokens_ (lp+1) 1 newbase [PosToken lp (cp+cp2-base) t:ts]
											= tokens_ lp (cp+newbase-base) newbase [PosToken lp (cp+cp2-base) t:ts]
					 = finalise lp cp ts
				
tokens :: !String -> [Token]
tokens inp = tokens_ 0 [] 
where 
	tokens_ base ts 
			= case base < (size inp) of
				True = let (_, newbase, t) = read_token base inp in tokens_ newbase [t:ts]
					 = reverse ts	

toStringR :: Real -> String
toStringR r = if (r - toReal(entier r) == 0.0) (toString r +++ ".0") (toString r)
 
instance toString Literal	
where
	toString (LString ustr) = "\"" +++ toJSLiteral ustr +++ "\""
	toString (LChar uchr) = "'" +++  toJSLiteral uchr +++ "'"	
	toString (LInt int) = toString int
	toString (LReal real) = toStringR real
	toString (LBool True) = "True"
	toString (LBool False) = "False"
					 
instance toString Token	
where
	toString (TIdentifier name) = escape name
	toString (TStrictIdentifier name) = "!" +++ (escape name)
	toString (TComment comment) = "||" +++ comment
	toString TInlineAnnotation = ""	// skip
	toString TAssignmentOp = "="
	toString TMacroAssignmentOp = ":=="
	toString TCAFAssignmentOp = "=:"
	toString TComma = ","
	toString TColon = ":"
	toString TVerticalBar = "|"
	toString TOpenParenthesis = "("
	toString TCloseParenthesis = ")"
	toString TOpenBracket = "{"
	toString TCloseBracket = "}"
	toString TTypeDef = "::"
	toString (TLit lit) = toString lit
	toString TCaseKeyword = "case"
	toString TSelectKeyword = "select"
	toString TLetKeyword = "let"
	toString TInKeyword = "in"
	toString TEndOfLine = "\n"
	toString _ = "\n"	

escape f | ss f                            
	= "<{" +++ f +++ "}>"
	= f
where 
	ss f = or [is_ss c \\ c <-: f]
    is_ss c = not (isAlphanum c || c == '_' || c == '.')  

