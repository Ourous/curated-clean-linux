implementation module Text.GenJSON

import StdGeneric, Data.Maybe, StdList, StdOrdList, StdString, _SystemArray, StdTuple, StdBool, StdFunc, StdOverloadedList, StdFile
import Data.Func, Data.List, Text, Text.PPrint, Text.GenJSON, Data.GenEq

//Basic JSON serialization
instance toString JSONNode
where
	//make target string -> copy characters
	//The reason why first a big string is made into which the characters are copied is to
	//avoid many string concatenations with big strings
	toString node
		#! len = sizeOf node
		= snd (copyNode 0 node (createArray len '\0'))

//Determine serialized size of a JSON datastructure
sizeOf :: !JSONNode -> Int
sizeOf (JSONNull)       = 4
sizeOf (JSONBool True)  = 4
sizeOf (JSONBool False) = 5
sizeOf (JSONInt x)      = size (toString x)
sizeOf (JSONReal x)     = size (jsonRealtoString x)
//For strings we need to allocate extra size for the enclosing double quotes and the escaping of special characters
sizeOf (JSONString x)   = size x + 2 + sizeOfEscapeChars x
//For arrays we need to allocate extra size for the enclosing brackets and comma's
sizeOf (JSONArray x)
  #! len = length x
  = (if (len > 0) (foldl (\s x -> s + sizeOf x) (len - 1) x) 0) + 2
//For objects we need to allocate extra size for the enclosing braces, comma's and labels
sizeOf (JSONObject x)
  #! len = length x
  = (if (len > 0) (foldl (\s (l,o) -> s + sizeOf (JSONString l) + 1 + sizeOf o) (len - 1) x) 0) + 2
sizeOf (JSONRaw x)      = size x
sizeOf (JSONError)      = 0

sizeOfEscapeChars :: !String -> Int
sizeOfEscapeChars s = count 0 s 0
where
	count :: !Int !String !Int -> Int
	count i s n
		| i < size s
			#! c = s.[i]
			| c == '"' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t' || c == '\\'
				= count (i + 1) s (n + 1) //We'll add a '\' to escape
			| c < ' '
				= count (i + 1) s (n + 5) //We'll replace the character by '\uXXXX'
			= count(i + 1) s n
		= n

//Copy structure to a string
copyNode :: !Int !JSONNode !*{#Char} -> *(!Int, !*{#Char})
copyNode start (JSONNull) buffer		= (start + 4, copyChars start 4 "null" buffer)
copyNode start (JSONBool True) buffer	= (start + 4, copyChars start 4 "true" buffer)
copyNode start (JSONBool False) buffer	= (start + 5, copyChars start 5 "false" buffer)
copyNode start (JSONInt x) buffer
  #! s = toString x
  = (start + size s, copyChars start (size s) s buffer)
copyNode start (JSONReal x) buffer
  #! s = jsonRealtoString x
  = (start + size s, copyChars start (size s) s buffer)
copyNode start (JSONString s) buffer
  #! (start,buffer)	= (start + 1, {buffer & [start] = '"'})
  #! (start,buffer) = (start + size s + sizeOfEscapeChars s, copyAndEscapeChars 0 start (size s) s buffer)
  = (start + 1, {buffer & [start] = '"'})
copyNode start (JSONArray items) buffer
	#! (start,buffer)	= (start + 1, {buffer & [start] = '['})
	#! (start,buffer)	= copyArrayItems start items buffer
	= (start + 1, {buffer & [start] = ']'})
where
    copyArrayItems :: !Int ![JSONNode] !*String -> *(!Int, !*String)
	copyArrayItems start [] buffer = (start,buffer)
	copyArrayItems start [x] buffer = copyNode start x buffer
	copyArrayItems start [x:xs] buffer
		#! (start,buffer) = copyNode start x buffer
		= copyArrayItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONObject items) buffer
	#! (start, buffer) = (start + 1, {buffer & [start] = '{'})
	#! (start, buffer) = copyObjectItems start items buffer
	= (start + 1, {buffer &	[start] = '}'})
where
    copyObjectItems :: !Int ![(String, JSONNode)] !*String -> *(!Int, !*String)
	copyObjectItems start [] buffer = (start,buffer)
	copyObjectItems start [(l,x)] buffer
		# (start,buffer) = copyNode start (JSONString l) buffer
		# (start,buffer) = (start + 1, {buffer & [start] = ':'})
		= copyNode start x buffer
	copyObjectItems start [(l,x):xs] buffer
		# (start,buffer) = copyNode start (JSONString l) buffer
		# (start,buffer) = (start + 1, {buffer & [start] = ':'})
		# (start,buffer) = copyNode start x buffer
		= copyObjectItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONRaw x) buffer	= (start + size x, copyChars start (size x) x buffer) 	
copyNode start _ buffer				= (start,buffer)

//Straightforward copying of strings, with some optimization
copyChars :: !Int !Int !String !*String -> *String
copyChars offset num src dst
	| num > 3
		#! di = offset + num
		#! dst & [di-4] = src.[num-4]
		#! dst & [di-3] = src.[num-3]
		#! dst & [di-2] = src.[num-2]
		#! dst & [di-1] = src.[num-1]
		= copyChars offset (num - 4) src dst
	| num > 1
		#! dst & [offset] = src.[0]
		#! dst & [offset+1] = src.[1]
		| num == 3
			= {dst & [offset+2] = src.[2]}
		= dst
	| num == 1
		= {dst & [offset] = src.[0]}
	= dst

//Copying strings with escaping of special characters (not optimized)
copyAndEscapeChars :: !Int !Int !Int !String !*String -> *String
copyAndEscapeChars soffset doffset num src dst
	| num > 0
		#! c = src.[soffset]
		//Check for special characters
		| c == '"' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t' || c == '\\'
			#! dst & [doffset] = '\\'
			#! dst & [doffset + 1] = charOf c
			= copyAndEscapeChars (soffset + 1) (doffset + 2) (num - 1) src dst	
		| c < ' '
            #! cint = toInt c
			#! dst & [doffset] = '\\'
			#! dst & [doffset + 1] = 'u'
			//Put the hexadecimal representation of the character in the following 4 characters
			#! dst & [doffset + 2] = '0'
			#! dst & [doffset + 3] = '0'
			#! dst & [doffset + 4] = toHexDigit ((cint >> 4) bitand 15)
			#! dst & [doffset + 5] = toHexDigit (cint bitand 15)
			= copyAndEscapeChars (soffset + 1) (doffset + 6) (num - 1) src dst	
		| otherwise	
			#! dst & [doffset] = c
			= copyAndEscapeChars (soffset + 1) (doffset + 1) (num - 1) src dst	
	= dst
where
	charOf '"' = '"'
	charOf '\b' = 'b'
	charOf '\f' = 'f'
	charOf '\n' = 'n'
	charOf '\r' = 'r'
	charOf '\t' = 't'
	charOf '\\' = '\\'
	charOf _   = abort "error in copyAndEscapeChars\n"
	
	toHexDigit c
		| c < 10 = toChar (c + 48)
	 			 = toChar (c + 87)

//Escape a string
jsonEscape :: !String -> String
jsonEscape src
	= copyAndEscapeChars 0 0 (size src) src (createArray (size src + sizeOfEscapeChars src) '\0')

instance <<< JSONNode
where
	(<<<) f JSONNull            = f <<< "null"
	(<<<) f (JSONBool True)     = f <<< "true"
	(<<<) f (JSONBool False)    = f <<< "false"
	(<<<) f (JSONInt i)         = f <<< toString i
	(<<<) f (JSONReal r)        = f <<< jsonRealtoString r
	(<<<) f (JSONString s)      = f <<< '"' <<< jsonEscape s <<< '"'
	(<<<) f (JSONArray nodes)   = printNodes nodes (f <<< "[") <<< "]"
	where
		printNodes :: [JSONNode] *File -> *File
		printNodes []         f = f
		printNodes [n]        f = f <<< n
		printNodes [n:ns]     f = printNodes ns (f <<< n <<< ",")
	(<<<) f (JSONObject nodes)  = printNodes nodes (f <<< "{") <<< "}"
	where
		printNodes :: [(String,JSONNode)] *File -> *File
		printNodes []         f = f
		printNodes [(k,v)]    f = f <<< '"' <<< jsonEscape k <<< "\":" <<< v
		printNodes [(k,v):ns] f = printNodes ns (f <<< '"' <<< jsonEscape k <<< "\":" <<< v <<< ",")
	(<<<) f (JSONRaw s)         = f <<< s
	(<<<) f JSONError           = abort "<<< called on JSONError\n"

//Basic JSON deserialization (just structure)
instance fromString JSONNode
where
	fromString s = fst (parse 0 s)

IsDigit c :== c >= '0' && c <= '9'

parse :: !Int !String -> (!JSONNode,!Int)
parse offset input
	| offset<size input
		#! c = input.[offset]
		| c=='"'
			#! offset=offset+1
			= parse_string offset offset input
		| c=='n' && offset+3<size input && input.[offset+1]=='u' && input.[offset+2]=='l' && input.[offset+3]=='l'
			= (JSONNull,offset+4)
		| c=='t' && offset+3<size input && input.[offset+1]=='r' && input.[offset+2]=='u' && input.[offset+3]=='e'
			= (JSONBool True, offset+4)
		| c=='f' && offset+4<size input && input.[offset+1]=='a' && input.[offset+2]=='l' && input.[offset+3]=='s' && input.[offset+4]=='e'
			= (JSONBool False, offset+5)
		| IsDigit c
			= parse_number (offset+1) offset input
		| c=='-' && offset+1<size input && IsDigit input.[offset+1]
			= parse_number (offset+2) offset input
		| c=='['
			= parse_array (offset+1) input
		| c=='{'
		 	= parse_object (offset+1) input
		| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
			= parse (skip_spaces (offset+1) input) input
			= (JSONError, offset)
		= (JSONError, offset)
where
	parse_string :: !Int !Int !{#Char} -> (!JSONNode,!Int)
	parse_string offset stringCharsOffset input
		| offset<size input
			#! c=input.[offset]
			| c <> '"'
				| c <> '\\'
					= parse_string (offset + 1) stringCharsOffset input
					= parse_string_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
				#! string = input % (stringCharsOffset,offset-1)
				= (JSONString string, offset+1)
			= (JSONError,offset) // missing '"'
	where
		parse_string_with_escape :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_string_with_escape offset stringCharsOffset input
			| offset<size input
				#! c = input.[offset]
				| c <> '"'
					| c <> '\\'
						= parse_string_with_escape (offset + 1) stringCharsOffset input
						= parse_string_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
					#! string = input % (stringCharsOffset,offset-1)
					= (JSONString (jsonUnescape string), offset+1)
				= (JSONError,offset) // missing '"'

	skip_spaces :: !Int !String -> Int
	skip_spaces offset input
		| offset<size input
			#! c = input.[offset]
			| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
				= skip_spaces (offset+1) input
				= offset
			= offset

	parse_number :: !Int !Int !{#Char} -> (!JSONNode,!Int)
	parse_number offset numberOffset input
		| offset>=size input
			#! i = toInt (input % (numberOffset,offset-1))
			= (JSONInt i,offset)
		#! c = input.[offset]
		| IsDigit c
			= parse_number (offset+1) numberOffset input
		| c<>'.'
			#! i = toInt (input % (numberOffset,offset-1))
			= (JSONInt i, offset)
			= parse_real (offset+1) numberOffset input
	where
		parse_real :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_real offset numberOffset input
			| offset>=size input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r,offset)
			#! c = input.[offset]
			| IsDigit c
				= parse_real (offset+1) numberOffset input
			| c<>'e' && c<>'E'
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r, offset)
			| offset+1<size input && IsDigit input.[offset+1]
				= parse_real_with_exponent (offset+2) numberOffset input
			| offset+2<size input && (input.[offset+1]=='-' || input.[offset+1] == '+') && IsDigit input.[offset+2]
				= parse_real_with_exponent (offset+3) numberOffset input
			#! r = toReal (input % (numberOffset,offset-1))
			= (JSONReal r, offset)
		
		parse_real_with_exponent :: !Int !Int !{#Char} -> (!JSONNode,!Int)
		parse_real_with_exponent offset numberOffset input
			| offset>=size input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r,offset)
			| IsDigit input.[offset]
				= parse_real_with_exponent (offset+1) numberOffset input
				#! r = toReal (input % (numberOffset,offset-1))
				= (JSONReal r, offset)

	parse_array :: !Int !{#Char} -> (!JSONNode,!Int)
	parse_array offset input
		| offset<size input && input.[offset]==']'
			= (JSONArray [], offset+1)
		#! offset = skip_spaces offset input
		| offset<size input && input.[offset]==']'
			= (JSONArray [], offset+1)
			= parse_array_items offset [] offset input
	where
		parse_array_items :: !Int !*[JSONNode] !Int !{#Char} -> (!JSONNode,!Int)
		parse_array_items offset items offset_after_bracket_open input
			#! (item,offset) = parse offset input
			| offset<size input && input.[offset]==','
				= parse_array_items (offset+1) [item:items] offset_after_bracket_open input
			| offset<size input && input.[offset]==']'
				= (JSONArray (reverse_append items [item]), offset+1)
			#! offset = skip_spaces offset input
			| offset<size input && input.[offset]==','
				= parse_array_items (offset+1) [item:items] offset_after_bracket_open input
			| offset<size input && input.[offset]==']'
				= (JSONArray (reverse_append items [item]), offset+1)
				= (JSONError, offset_after_bracket_open)

	parse_object :: !Int !{#Char} -> (!JSONNode,!Int)
	parse_object offset input
		| offset<size input && input.[offset]=='}'
			= (JSONObject [], offset+1)
		#! offset = skip_spaces offset input
		| offset<size input && input.[offset]=='}'
			= (JSONObject [], offset+1)
			= parse_object_items offset [] offset input
	where
		parse_object_items :: !Int !*[({#Char}, JSONNode)] !Int !{#Char} -> (!JSONNode,!Int)
		parse_object_items offset items offset_after_bracket_open input
			| offset<size input
				| input.[offset]=='"'
					#! offset=offset+1
					#! (label,offset) = lex_label offset offset input
					| offset>=0
						| offset<size input && input.[offset]==':'
							= parse_object_items_after_label_and_colon label (offset+1) items offset_after_bracket_open input
							#! offset = skip_spaces offset input
							| offset<size input && input.[offset]==':'
								= parse_object_items_after_label_and_colon label (offset+1) items offset_after_bracket_open input
								= (JSONError, offset_after_bracket_open)
						= (JSONError, offset_after_bracket_open)
					#! c = input.[offset]
					| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
						= parse_object_items (skip_spaces (offset+1) input) items offset_after_bracket_open input
						= (JSONError, offset_after_bracket_open)
				= (JSONError, offset_after_bracket_open)
		where
			lex_label :: !Int !Int !{#Char} -> (!{#Char},!Int)
			lex_label offset stringCharsOffset input
				| offset<size input
					#! c=input.[offset]
					| c <> '"'
						| c <> '\\'
							= lex_label (offset + 1) stringCharsOffset input
							= lex_label_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
						#! string = input % (stringCharsOffset,offset-1)
						= (string, offset+1)
					= ("",-1) // missing '"'

			lex_label_with_escape :: !Int !Int !{#Char} -> (!{#Char},!Int)
			lex_label_with_escape offset stringCharsOffset input
				| offset<size input
					#! c=input.[offset]
					| c <> '"'
						| c <> '\\'
							= lex_label_with_escape (offset + 1) stringCharsOffset input
							= lex_label_with_escape (offset + 2) stringCharsOffset input // skip the escaped character
						#! string = input % (stringCharsOffset,offset-1)
						= (jsonUnescape string, offset+1)
					= ("",-1) // missing '"'

		parse_object_items_after_label_and_colon :: !{#Char} !Int !*[({#Char}, JSONNode)] !Int !{#Char} -> (!JSONNode,!Int)
		parse_object_items_after_label_and_colon label offset items offset_after_brace_open input
			#! (item,offset) = parse offset input
			| offset<size input && input.[offset]==','
				= parse_object_items (offset+1) [(label,item):items] offset_after_brace_open input
			| offset<size input && input.[offset]=='}'
				= (JSONObject (reverse_append items [(label,item)]), offset+1)
			#! offset = skip_spaces offset input
			| offset<size input && input.[offset]==','
				= parse_object_items (offset+1) [(label,item):items] offset_after_brace_open input
			| offset<size input && input.[offset]=='}'
				= (JSONObject (reverse_append items [(label,item)]), offset+1)
				= (JSONError, offset_after_brace_open)

	reverse_append :: !*[.a] !*[.a] -> *[.a]
	reverse_append [hd:tl] list	= reverse_append tl [hd:list]
	reverse_append [] list		= list

//For strings that contain escaped characters, the destination string will be smaller
//This function determines 
sizeOfExtraCharsOfEscapes :: !String -> Int
sizeOfExtraCharsOfEscapes s = count 0 s 0
where
	count :: !Int !String !Int -> Int
	count i s n
		| i < (size s - 1)
			#! cc = s.[i]
			#! cn = s.[i + 1]
			| cc == '\\'
				| cn == 'u' = count (i + 6) s (n + 5)
							= count (i + 2) s (n + 1)
			= count (i + 1) s n
		= n

//Copying strings with escaping of special characters (not optimized)
copyAndUnescapeChars :: !Int !Int !Int !String !*String -> *String
copyAndUnescapeChars soffset doffset num src dst
	| num > 0
		#! cc = src.[soffset]
		//Check for escapes
		| cc == '\\' && num > 1
			#! cn = src.[soffset + 1]
			| cn == '"' || cn == '/' || cn == 'b' || cn == 'f' || cn == 'n' || cn == 'r' || cn == 't' || cn == '\\'
				#! dst & [doffset] = charOf cn
				= copyAndUnescapeChars (soffset + 2) (doffset + 1) (num - 2) src dst	
			| cn == 'u' && num > 5
				// The escape is in the form \uXXXX
				// Use the last two hex numbers to reconstruct the character value
				#! dst & [doffset] = toChar (((fromHexDigit src.[soffset + 4]) << 4) + (fromHexDigit src.[soffset + 5]))
				= copyAndUnescapeChars (soffset + 6) (doffset + 1) (num - 6) src dst	
			| otherwise
				#! dst & [doffset] = cc
				= copyAndUnescapeChars (soffset + 1) (doffset + 1) (num - 1) src dst	
		| otherwise
			#! dst & [doffset] = cc
			= copyAndUnescapeChars (soffset + 1) (doffset + 1) (num - 1) src dst
	= dst
where
	charOf '"' = '"'
	charOf '/' = '/'
	charOf 'b' = '\b'
	charOf 'f' = '\f'
	charOf 'n' = '\n'
	charOf 'r' = '\r'
	charOf 't' = '\t'
	charOf '\\' = '\\'
	charOf _   = abort "error in copyAndUnescapeChars\n"
	
	fromHexDigit :: Char -> Int
	fromHexDigit c
		| isDigit c = digitToInt c
		| c <= 'f' && c >= 'a' = toInt c - 87
		| c <= 'F' && c >= 'A' = toInt c - 55
		= 0

//Unescape a string
jsonUnescape :: !String -> String
jsonUnescape src
	= copyAndUnescapeChars 0 0 (size src) src (createArray (size src - sizeOfExtraCharsOfEscapes src) '\0')

//-------------------------------------------------------------------------------------------

toJSON :: !a -> JSONNode | JSONEncode{|*|} a
toJSON x = toJSON` False x

toJSONInField :: !a -> JSONNode | JSONEncode{|*|} a
toJSONInField x = toJSON` True x

toJSON` :: !Bool !a -> JSONNode | JSONEncode{|*|} a
toJSON` flag x = case (JSONEncode{|*|} flag x) of
	[node]	= node
	_		= JSONError 

/*
* Generic JSON encoder
*/
generic JSONEncode t :: !Bool !t -> [JSONNode]

JSONEncode{|Int|} _ x = [JSONInt x]
JSONEncode{|Real|} _ x = [JSONReal x]
JSONEncode{|Char|} _ x = [JSONString {x}]
JSONEncode{|Bool|} _ x = [JSONBool x]
JSONEncode{|String|} _ x = [JSONString x]
JSONEncode{|UNIT|} _ (UNIT) = []
JSONEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list
JSONEncode{|EITHER|} fx fy _ (LEFT x) = fx False x
JSONEncode{|EITHER|} fx fy _ (RIGHT y) = fy False y
JSONEncode{|OBJECT|} fx _ (OBJECT x) = fx False x
JSONEncode{|CONS of {gcd_name}|} fx _ (CONS x)
  = [JSONArray [JSONString gcd_name : fx False x]]
JSONEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
	= [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
where
	isNotNull :: !JSONNode -> Bool
	isNotNull JSONNull = False
	isNotNull _ = True
JSONEncode{|FIELD|} fx _ (FIELD x) = fx True x
JSONEncode{|[]|} fx _ x = [JSONArray (flatten (map (fx False) x))]
JSONEncode{|()|} _ () = [JSONNull]
JSONEncode{|(,)|} fx fy _ (x,y) = [JSONArray (fx False x ++ fy False y)]
JSONEncode{|(,,)|} fx fy fz _ (x,y,z) = [JSONArray (fx False x ++ fy False y ++ fz False z)]
JSONEncode{|(,,,)|} fx fy fz fi _ (x,y,z,i) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i)]
JSONEncode{|(,,,,)|} fx fy fz fi fj _ (x,y,z,i,j) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j)]
JSONEncode{|(,,,,,)|} fx fy fz fi fj fk _ (x,y,z,i,j,k) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k)]
JSONEncode{|(,,,,,,)|} fx fy fz fi fj fk fl _ (x,y,z,i,j,k,l) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l)]
JSONEncode{|(,,,,,,,)|} fx fy fz fi fj fk fl fm _ (x,y,z,i,j,k,l,m) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l ++ fm False m)]
JSONEncode{|{}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSONEncode{|{!}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSONEncode{|Maybe|} fx inField (Just x) = if inField (fx False x) [JSONArray [JSONString "Just" : fx False x]]
JSONEncode{|Maybe|} fx inField Nothing = if inField [JSONNull] [JSONArray [JSONString "Nothing"]]
JSONEncode{|JSONNode|} _ node = [node]

//-------------------------------------------------------------------------------------------
fromJSON :: !JSONNode -> Maybe a | JSONDecode{|*|} a
fromJSON node = fst (JSONDecode{|*|} False [node])

/*
* Generic JSON parser, using a list of tokens
*/
generic JSONDecode t :: !Bool ![JSONNode] -> (!Maybe t, ![JSONNode])

JSONDecode{|Int|} _ [JSONInt i:xs]		= (Just i, xs)
JSONDecode{|Int|} _ l					= (Nothing, l)

JSONDecode{|Real|} _ [JSONNull:xs]		= (Just NaN, xs)
JSONDecode{|Real|} _ [JSONReal r:xs]	= (Just r, xs)
JSONDecode{|Real|} _ [JSONInt i:xs]		= (Just (toReal i), xs)
JSONDecode{|Real|} _ l					= (Nothing, l)

JSONDecode{|Char|} _ l=:[JSONString s:xs]
	| size s == 1						= (Just s.[0],xs)
										= (Nothing, l)
JSONDecode{|Char|} _ l					= (Nothing, l)

JSONDecode{|Bool|} _ [JSONBool b:xs]	= (Just b,xs)
JSONDecode{|Bool|} _ l					= (Nothing, l)

JSONDecode{|String|} _ [JSONString s:xs]= (Just s, xs)
JSONDecode{|String|} _ l				= (Nothing, l)

JSONDecode{|UNIT|} _ l					= (Just UNIT, l)

JSONDecode{|PAIR|} fx fy _ l = d1 fy (fx False l) l
  where
  d1 :: !(Bool [JSONNode] -> (Maybe b, [JSONNode])) !(!Maybe a, ![JSONNode]) ![JSONNode]
     -> (!Maybe (PAIR a b), ![JSONNode])
  d1 fy (Just x,xs)  l = d2 x (fy False xs) l
  d1 _  (Nothing, _) l = (Nothing, l)

  d2 :: !a !(!Maybe b, ![JSONNode]) ![JSONNode] -> (!Maybe (PAIR a b), ![JSONNode])
  d2 x (Just y, ys) l = (Just (PAIR x y), ys)
  d2 x (Nothing, _) l = (Nothing, l)

JSONDecode{|EITHER|} fx fy _ l = case fx False l of
	(Just x, xs)				= (Just (LEFT x),xs)
	(Nothing, xs)				= case fy False l of
		(Just y, ys)			= (Just (RIGHT y),ys)
		(Nothing, ys)			= (Nothing, l)

JSONDecode{|OBJECT|} fx _ l = case fx False l of
	(Just x, xs)	= (Just (OBJECT x),xs)
	_				= (Nothing, l)

JSONDecode{|CONS of {gcd_name}|} fx _ l=:[JSONArray [JSONString name:fields] :xs]
	| name == gcd_name				= case fx False fields of
		(Just x, _)					= (Just (CONS x), xs)
		_							= (Nothing, l)
	| otherwise						= (Nothing, l)		
JSONDecode{|CONS|} fx _ l = (Nothing, l)

JSONDecode{|RECORD|} fx _ l=:[obj=:JSONObject fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l=:[obj=:JSONArray fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l = (Nothing,l)

JSONDecode{|FIELD of {gfd_name}|} fx _ l =:[JSONObject fields]
  #! field = findField gfd_name fields
  = case fx True field of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
  where
  findField :: !String ![(String, JSONNode)] -> [JSONNode]
  findField match [(l,x):xs]
    | l == match = [x]
    | otherwise  = findField match xs
  findField match [] = []
JSONDecode{|FIELD of {gfd_index}|} fx _ l =:[JSONArray fields]
	= case fields !? gfd_index of
		Nothing    = (Nothing, l)
		Just field = case fx True [field] of
			(Just x, _) = (Just (FIELD x), l)
			(_, _)      = (Nothing, l)
JSONDecode{|FIELD|} fx _ l = (Nothing, l)

JSONDecode{|[]|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just x, xs)
		_				= (Nothing, l)
JSONDecode{|[]|} fx _ l = (Nothing, l)

JSONDecode{|()|} _ [JSONNull:c]     = (Just (), c)
JSONDecode{|()|} _ [JSONObject []:c]= (Just (), c)
JSONDecode{|()|} _ c                = (Nothing, c)

JSONDecode{|(,)|} fx fy _ l =:[JSONArray [xo,yo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)		= (Just (x,y), xs)
			_				= (Nothing, l)
		_					= (Nothing, l)
JSONDecode{|(,)|} fx fy _ l	= (Nothing, l)

JSONDecode{|(,,)|} fx fy fz _ l =:[JSONArray [xo,yo,zo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)			= case fz False [zo] of
				(Just z,_)		= (Just (x,y,z), xs)
				_				= (Nothing, l)
			_					= (Nothing, l)
		_						= (Nothing, l)
JSONDecode{|(,,)|} fx fy fz _ l	= (Nothing, l)

JSONDecode{|(,,,)|} fx fy fz fi _ l =:[JSONArray [xo,yo,zo,io]:xs]
	= case fx False [xo] of
		(Just x,_) = case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)		= (Just (x,y,z,i), xs)
					_				= (Nothing, l)
				_					= (Nothing, l)
			_						= (Nothing, l)
		_							= (Nothing, l)
JSONDecode{|(,,,)|} fx fy fz fi _ l	= (Nothing, l)

JSONDecode{|(,,,,)|} fx fy fz fi fj _ l =:[JSONArray [xo,yo,zo,io,jo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)	= case fj False [jo] of
						(Just j,_)		= (Just (x,y,z,i,j), xs)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,)|} fx fy fz fi fj _ l	= (Nothing, l)

JSONDecode{|(,,,,,)|} fx fy fz fi fj fk _ l =:[JSONArray [xo,yo,zo,io,jo,ko]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_)	= case fj False [jo] of
						(Just j,_)		= case fk False [ko] of
                            (Just k, _) = (Just (x,y,z,i,j,k), xs)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,)|} fx fy fz fi fj fk _ l	= (Nothing, l)

JSONDecode{|(,,,,,,)|} fx fy fz fi fj fk fm _ l =:[JSONArray [xo,yo,zo,io,jo,ko,mo]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_) = case fj False [jo] of
						(Just j,_) = case fk False [ko] of
                            (Just k, _) = case fm False [mo] of
                              (Just m, _) = (Just (x,y,z,i,j,k,m), xs)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,,)|} fx fy fz fi fj fk fm _ l	= (Nothing, l)

JSONDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn _ l =:[JSONArray [xo,yo,zo,io,jo,ko,mo,no]:xs]
	= case fx False [xo] of
		(Just x,_)	= case fy False [yo] of
			(Just y,_)	= case fz False [zo] of
				(Just z,_) = case fi False [io] of
					(Just i,_) = case fj False [jo] of
						(Just j,_) = case fk False [ko] of
                            (Just k, _) = case fm False [mo] of
                              (Just m, _) = case fn False [no] of
                                (Just n, _) = (Just (x,y,z,i,j,k,m,n), xs)
                                _           = (Nothing, l)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn _ l	= (Nothing, l)

JSONDecode{|{}|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{}|} fx _ l = (Nothing, l)

JSONDecode{|{!}|} fx _ l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{!}|} fx _ l = (Nothing, l)

decodeItems :: !(Bool [JSONNode] -> (Maybe a, [JSONNode])) ![JSONNode] -> Maybe [a]
decodeItems fx [] 		= Just []
decodeItems fx [ox:oxs]	= case fx False [ox] of
	(Just x, _)	= case decodeItems fx oxs of
		(Just xs)	= Just [x:xs]
		_ 			= Nothing
	_			= Nothing

// When not in a record, treat Maybe normally
JSONDecode{|Maybe|} fx False [JSONArray [JSONString "Nothing"]:xs] = (Just Nothing, xs)
JSONDecode{|Maybe|} fx False [JSONArray [JSONString "Just":l]:xs]
  = case fx False l of
      (Just x, _) = (Just (Just x), xs)
      _           = (Nothing, l)
// Maybe is treated a bit special in record fields for efficiency
JSONDecode{|Maybe|} fx True [JSONNull:xs] = (Just Nothing, xs) // Interpret null as Nothing
JSONDecode{|Maybe|} fx True []            = (Just Nothing, []) // Interpret absentness as Nothing
JSONDecode{|Maybe|} fx True l
  = case fx False l of                  // Interpret existense as Just
      (Just x,xs)                         = (Just (Just x), xs)
      _                                   = (Nothing, l)
JSONDecode{|Maybe|} _ _ l               = (Nothing, l) // If all else fails... Nothing

JSONDecode{|JSONNode|} _ [node:xs]      = (Just node, xs)
JSONDecode{|JSONNode|} True []			= (Just JSONNull, []) //In record fields, fields with value JSONNull are removed
JSONDecode{|JSONNode|} _ l				= (Nothing, l)

jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a
jsonQuery path node
	= case (findNode (split "/" path) node ) of
		Just child	= fromJSON child
		Nothing		= Nothing
where
	findNode :: ![String] !JSONNode -> Maybe JSONNode
	findNode [] node	= Just node
	findNode [s:ss] (JSONObject fields)
		= case findField s fields of
			Just f	= findNode ss f
			Nothing	= Nothing
	findNode [s:ss] (JSONArray items)
		#! index = toInt s
		| index >= 0 && index < length items	= findNode ss (items !! index)
		| otherwise								= Nothing
	findNode _ _		= Nothing
	
    findField :: !String ![(String, JSONNode)] -> Maybe JSONNode
	findField s []			= Nothing
	findField s [(l,x):xs]	= if (l == s) (Just x) (findField s xs)

instance == JSONNode
where
	// NB: put the most frequently encountered constructors at the top for performance
	== (JSONObject xs) y = case y of
		JSONObject ys
			-> sortBy cmpFst xs == sortBy cmpFst ys
			-> False
	where
		cmpFst = (<) `on` fst
	== (JSONArray x)   y = case y of JSONArray y  -> x==y; _ -> False
	== (JSONString x)  y = case y of JSONString y -> x==y; _ -> False
	== (JSONInt x)     y = case y of
		JSONInt y  -> x==y
		JSONReal y -> toString (toReal x) == toString y
		_          -> False
	== (JSONReal x)    y = case y of
		JSONReal y -> x==y
		JSONInt y  -> toString (toReal y) == toString x
		_          -> False
	== JSONNull        y = y=:JSONNull
	== (JSONBool x)    y = case y of JSONBool y -> x==y; _ -> False
	== (JSONRaw x)     y = case y of JSONRaw y    -> x==y; _ -> False
	== JSONError       y = y=:JSONError

gEq{|JSONNode|} x y = x == y

jsonPrettyPrint :: !JSONNode -> String
jsonPrettyPrint json = display (renderPretty 0.0 400 (pretty json))

instance Pretty JSONNode
where
	pretty JSONNull 			= string "null"
	pretty (JSONBool x)			= string (if x "true" "false")
	pretty (JSONInt x)			= string (toString x)
	pretty (JSONReal x)			= string (jsonRealtoString x)
	pretty (JSONString x)		= dquotes (string (jsonEscape x))
	pretty (JSONArray nodes)	= list (map pretty nodes)
	pretty (JSONObject attr)	= encloseSep lbrace rbrace comma [dquotes (string label) <-> colon <-> pretty val \\ (label,val) <- attr]
	pretty (JSONRaw x)			= string x
	pretty JSONError			= string "null"

jsonRealtoString :: !Real -> String
jsonRealtoString x
	| isInfinity x
		| x < 0.0 = "-1.e+9999"
		= "1.e+9999"
	| isNaN x = toString JSONNull
	= toString x
