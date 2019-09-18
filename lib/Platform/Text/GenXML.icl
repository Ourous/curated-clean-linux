implementation module Text.GenXML

import StdArray, StdBool, StdInt, StdList, StdMisc, StdTuple, StdGeneric, StdFunc, StdString
import Data.Error, Data.Either, Data.Maybe, Text, Data.GenEq
from Text.Parsers.CParsers.ParserCombinators import :: Parser, :: ParsResult, :: CParser, &>, +&+, +&-, -&+, <!>, <&, <&>, <*?>, <@, >?<, @>, begin1, satisfy, symbol, yield, <|>, <+?>

uname :: !String -> XMLQName
uname name = XMLQName Nothing name

qname :: !XMLNamespacePrefix !String -> XMLQName
qname namespace name = XMLQName (Just namespace) name

addNamespaces :: !(Maybe XMLURI) [(XMLNamespacePrefix,String)] !XMLNode -> XMLNode
addNamespaces mbDefaultNamespace namespaces (XMLElem qname attrs children)
	# ns = map (\(prefix,uri) -> XMLAttr (XMLQName (Just "xmlns") prefix) uri) namespaces
	# ns = case mbDefaultNamespace of
		Nothing					= ns
		Just defaultNamespace	= [XMLAttr (XMLQName Nothing "xmlns") defaultNamespace:ns]
	= (XMLElem qname (ns ++ attrs) children)
addNamespaces _ _ _ = abort "addNamespaces called on non-XMLElem\n"
	
docSize :: !XMLDoc -> Int
docSize (XMLDoc defaultNamespace namespaces documentElement)
	# documentElement = addNamespaces defaultNamespace namespaces documentElement
	= 37 + nodeSize documentElement

nodeSize :: !XMLNode -> Int
nodeSize (XMLText text) = escapedSize text
nodeSize (XMLElem qname attrs children)
	# attrsSize = sum (map attrSize attrs) + length attrs
 	= if (isEmpty children) 
 		(3 + qnameSize qname + attrsSize)
		(5 + 2 * qnameSize qname + attrsSize + sum (map nodeSize children))

attrSize :: !XMLAttr -> Int
attrSize (XMLAttr qname value) = 3 + qnameSize qname + escapedSize value

qnameSize :: !XMLQName -> Int
qnameSize (XMLQName Nothing   name) = size name
qnameSize (XMLQName (Just ns) name) = 1 + size ns + size name

//Calculates the number of chars in a string when xml special characters are escaped
escapedSize :: !{#Char} -> Int
escapedSize s = escapedSize` s (size s) 0
where
	escapedSize` s n i
		| i == n = 0
		| s.[i] == '<' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '>' = 4 + escapedSize` s n (i + 1)
		| s.[i] == '&' = 5 + escapedSize` s n (i + 1)
		| otherwise = 1 + escapedSize` s n (i + 1)

serializeDoc :: !XMLDoc !*{#Char} !Int -> (!*{#Char}, !Int)
serializeDoc (XMLDoc defaultNamespace namespaces documentElement) dest dest_i
	# documentElement = addNamespaces defaultNamespace namespaces documentElement
	# (dest,dest_i) = copyChars "<?xml version=\"1.0\" standalone=\"no\"?>" 0 False dest dest_i
	= serializeNode documentElement dest dest_i 

serializeNode :: !XMLNode !*{#Char} !Int -> (!*{#Char}, !Int)
serializeNode (XMLText text) dest dest_i = copyChars text 0 True dest dest_i
serializeNode (XMLElem qname attrs []) dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
	# dest = {dest & [dest_i] = '/'}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '>'}
	= (dest,dest_i + 1)
serializeNode (XMLElem qname attrs children) dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
	# dest = {dest & [dest_i] = '>'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeMap serializeNode children dest dest_i
	# dest = {dest & [dest_i] = '<'}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '/'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i
	# dest = {dest & [dest_i] = '>'}
	= (dest,dest_i + 1)

serializeMap f [] dest dest_i = (dest, dest_i)
serializeMap f [x:xs] dest dest_i
	# (dest, dest_i) = f x dest dest_i
	= serializeMap f xs dest dest_i

serializeAttr :: !XMLAttr !*{#Char} !Int -> (!*{#Char}, !Int)
serializeAttr (XMLAttr qname value) dest dest_i
	# dest = {dest & [dest_i] = ' '}
	# dest_i = dest_i + 1
	# (dest,dest_i) = serializeQName qname dest dest_i 
	# dest = {dest & [dest_i] = '='}
	# dest_i = dest_i + 1
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	# (dest,dest_i) = copyChars value 0 True dest dest_i
	# dest = {dest & [dest_i] = '"'}
	# dest_i = dest_i + 1
	= (dest,dest_i)
	
serializeQName :: !XMLQName !*{#Char} !Int -> (!*{#Char}, !Int)
serializeQName (XMLQName Nothing   name) dest dest_i = copyChars name 0 False dest dest_i
serializeQName (XMLQName (Just ns) name) dest dest_i
	# (dest, dest_i) = copyChars ns 0 False dest dest_i
	# dest = {dest & [dest_i] = ':'}
	# dest_i = dest_i + 1
	= copyChars name 0 False dest dest_i

copyChars :: !{#Char} !Int !Bool !*{#Char} !Int -> (!*{#Char},!Int)
copyChars src src_i escape dest dest_i
	| src_i == (size src) = (dest, dest_i)
	| otherwise	
		| escape && (src.[src_i] == '<')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'l', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '>')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'g', [dest_i + 2] = 't', [dest_i + 3] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 4)
		| escape && (src.[src_i] == '&')
			# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'm', [dest_i + 3] = 'p', [dest_i + 4] = ';'}
			= copyChars src (src_i + 1) escape dest (dest_i + 5)
		| otherwise
			# dest = {dest & [dest_i] = src.[src_i]}
			= copyChars src (src_i + 1) escape dest (dest_i + 1)

instance toString XMLDoc
where
	toString doc
		# docsize = docSize doc
		# docstring = createArray docsize '\0'
		# (docstring,_) = serializeDoc doc docstring 0
		= docstring

instance fromString (MaybeErrorString XMLDoc)
where
	fromString xmlStr
		# tokens = lex xmlStr 0 []
		| isError tokens = liftError tokens
		# xmlDoc = pXMLDoc (fromOk tokens)
		| isEmpty xmlDoc = Error "parse error"
		= Ok (snd (hd xmlDoc))

//Token type which is the intermediary representation during XML parsing
:: Token	= TokenAttrValue !String
			| TokenCharData !String
			| TokenName !String
			| TokenStartTagOpen
			| TokenTagClose
			| TokenEmptyTagClose
			| TokenEndTagOpen
			| TokenDeclarationStart
			| TokenDeclarationEnd
			| TokenEqual
			
derive gEq Token
instance == Token
where
	(==) a b = a === b
	
isName (TokenName _)			= True
isName _						= False

isCharData (TokenCharData _)	= True
isCharData _					= False

isAttrValue (TokenAttrValue _)	= True
isAttrValue _					= False
			
:: LexFunctionResult = Token !Int !Token | NoToken !Int | Fail !String
:: LexFunction :== String Int -> Maybe LexFunctionResult
			
lex :: !String !Int ![Token] -> MaybeErrorString [Token]
lex input offset tokens 
	| offset >= size input						= Ok (reverse tokens) //Done
	| dataMode tokens && isJust charDataResult 	= processResult (fromJust charDataResult)
	| otherwise									= processResult (lexAny input offset lexFunctions)
		
where
	lexFunctions	=	[ lexWhitespace
						, lexDeclarationStart
						, lexDeclarationEnd
						, lexEmptyTagClose
						, lexTagClose
						, lexEndTagOpen
						, lexStartTagOpen
						, lexEqual
						, lexAttrValue
						, lexName
						]
						
	dataMode [TokenTagClose:_]		= True
	dataMode [TokenEmptyTagClose:_]	= True
	dataMode _						= False
	
	charDataResult = lexCharData input offset
	
	processResult r = case r of
		Token offset token		= lex input offset [token:tokens] //Lex another token and do recursive call
		NoToken offset			= lex input offset tokens
		Fail err				= Error err
	
	//Try any of the lexers in the list until one succeeds
	lexAny :: !String !Int ![LexFunction] -> LexFunctionResult
	lexAny input offset [] = Fail ("invalid input character: '" +++ toString input.[offset] +++ "'")
	lexAny input offset [f:fs] = case f input offset of
		Just result	= result
		Nothing		= lexAny input offset fs
																
	lexEqual			= lexFixed "="	TokenEqual
	lexDeclarationEnd	= lexFixed "?>"	TokenDeclarationEnd
	lexEndTagOpen		= lexFixed "</"	TokenEndTagOpen
	lexStartTagOpen		= lexFixed "<"	TokenStartTagOpen
	lexEmptyTagClose	= lexFixed "/>"	TokenEmptyTagClose
	lexTagClose			= lexFixed ">"	TokenTagClose
	
	lexDeclarationStart	input offset = case lexFixed "<?xml" TokenDeclarationStart input offset of
		Nothing				= Nothing
		Just res
			| offset == 0	= Just res
			| otherwise		= Just (Fail ("XML declaration not at start of entity"))
		
	//Char data
	lexCharData input offset
		| isTextChar input.[offset]
			# data = trim (input % (offset, end - 1))
			| data <> ""	= Just (Token end (TokenCharData data))
			| otherwise		= Nothing
		| otherwise			= Nothing							
	where
		end = findEnd isTextChar input (offset + 1)
		
		isTextChar c = c <> '<' && c <> '&'
		
	//Names
	lexName input offset
		| isNameStartChar input.[offset]	= Just (Token end (TokenName (input % (offset, end - 1))))
		| otherwise							= Nothing							
	where
		end = findEnd isNameChar input (offset + 1)
		
		isNameStartChar c
			| c == ':' || c == '_'	= True
			| c >= 'a' && c <= 'z'	= True
			| c >= 'A' && c <= 'Z'	= True
			| otherwise				= False
			
		isNameChar c
			| isNameStartChar c		= True
			| c == '-' || c == '.'	= True
			| c >= '0' && c <= '9'	= True
			| otherwise				= False
			
	//AttrValue
	lexAttrValue input offset
		| input.[offset] <> '"'			= Nothing
										= Just (Token end (TokenAttrValue (input % (offset + 1, end - 2))))
	where
		end = findAttrValueEnd input (offset + 1)
		
		findAttrValueEnd input offset
			| offset >= size input	= offset
			| input.[offset] == '"'	= offset + 1
			| otherwise				= findAttrValueEnd input (offset + 1)
			
	lexWhitespace input offset
		| last == offset	= Nothing
							= Just (NoToken last)
	where
		last = findEnd isWhitespace input offset
		
		isWhitespace '\x20'	= True
		isWhitespace '\x9'	= True
		isWhitespace '\xD'	= True
		isWhitespace '\xA'	= True
		isWhitespace _		= False
	
	//Lex token of fixed size
	lexFixed chars token input offset
		| input % (offset,offset + (size chars) - 1) == chars	= Just (Token (offset + size chars) token)
																= Nothing
	
	//Find the first offset where the predicate no longer holds					
	findEnd pred input offset
			| offset >= size input		= offset
			| pred input.[offset]		= findEnd pred input (offset + 1)
										= offset
	
pXMLDoc :: Parser Token XMLDoc
pXMLDoc = begin1 pXMLDoc`
where
	pXMLDoc` = mkXMLDoc @> (pDocDeclaration <|> yield [] )-&+ pElem
	
	mkXMLDoc (XMLElem name attributes elements) = XMLDoc mbURI namespaces (XMLElem name attrs elements)
	where
		(mbURI,namespaces,attrs) = filterNamespaces attributes (Nothing,[],[])
		
		filterNamespaces [] acc = acc
		filterNamespaces [attr=:(XMLAttr name val):rest] (mbURI,namespaces,attrs)
			# acc = case name of
				XMLQName Nothing "xmlns"	= (Just val,namespaces,attrs)
				XMLQName (Just "xmlns") ns	= (mbURI,[(ns,val):namespaces],attrs)
				_							= (mbURI,namespaces,[attr:attrs])
			= filterNamespaces rest acc
	mkXMLDoc _ = abort "mkXMLDoc called on non-XMLElem\n"

pDocDeclaration	= symbol TokenDeclarationStart &> (<+?> pAttr) <& symbol TokenDeclarationEnd
pNode			= pCharData <@ (\d -> XMLText d) <!> pElem
pElem			= pElemCont <!> pElemEmpty
pElemCont		= pElemStart <&> (\(name,attributes) -> symbol TokenTagClose &> (<*?> pNode) <& pElemContEnd >?< ((==) name) <@ (\nodes -> XMLElem (toQName name) attributes nodes))
pElemEmpty		= pElemStart <& symbol TokenEmptyTagClose <@ (\(name,attributes) -> XMLElem (toQName name) attributes [])
pElemStart		= (\name attributes -> (name,attributes)) @> symbol TokenStartTagOpen -&+ pName +&+ (<*?> pAttr)
pElemContEnd	= symbol TokenEndTagOpen &> pName <& symbol TokenTagClose
pAttr			= (\name v -> XMLAttr (toQName name) v) @> pName +&- symbol TokenEqual +&+ pAttrValue
pName			= satisfy isName		<@ \n -> case n of TokenName n -> n; _ -> abort "error in pName\n"
pAttrValue		= satisfy isAttrValue	<@ \n -> case n of TokenAttrValue v -> v; _ -> abort "error in pAttrValue\n"
pCharData		= satisfy isCharData	<@ \n -> case n of TokenCharData d -> d; _ -> abort "error in pCharData\n"

toQName :: !String -> XMLQName
toQName name
	| colonIdx > 0	= qname (subString 0 colonIdx name) (subString (colonIdx + 1) (textSize name - colonIdx) name)
	| otherwise		= uname name
where
	colonIdx = indexOf ":" name 
	
// generic printer

toXML :: !a -> XMLDoc | XMLEncode{|*|} a
toXML a = XMLDoc Nothing [] (wrapToElem (XMLEncode{|*|} a))

toXMLString :: !a -> String | XMLEncode{|*|} a
toXMLString a = toString (toXML a)

:: XMLEncodeResult = XMLEncElem !(!XMLQName,![XMLAttr],![XMLNode]) | XMLEncText !(!String,!XMLQName) | XMLEncNodes ![XMLNode] !XMLQName | XMLEncNothing

generic XMLEncode a :: !a -> XMLEncodeResult

XMLEncode{|OBJECT|} fx (OBJECT o) = fx o
XMLEncode{|CONS of d|} fx (CONS c)
	# nodes	= getNodes (fx c)
	# name	= uname (formatConsName d.gcd_name)
	| d.gcd_type_def.gtd_num_conses > 1 = XMLEncElem (name,[],nodes)
	| otherwise							= XMLEncNodes nodes name
where
	nonEmpty (XMLElem _ _ [])	= False
	nonEmpty _					= True
	
	formatConsName name
		| startsWith "_" name	= subString 1 (textSize name - 1) name
		| otherwise				= name
XMLEncode{|RECORD of d|} fx (RECORD c)
	# nodes	= getNodes (fx c)
	# name	= uname (formatConsName d.grd_name)
	| not (isEmpty d.grd_fields)		= XMLEncNodes (filter nonEmpty nodes) name
	| otherwise							= XMLEncNodes nodes name
where
	nonEmpty (XMLElem _ _ [])	= False
	nonEmpty _					= True
	
	formatConsName name
		| startsWith "_" name	= subString 1 (textSize name - 1) name
		| otherwise				= name
XMLEncode{|FIELD of d|} fx (FIELD f) = XMLEncElem (uname d.gfd_name,[],getNodes (fx f))
XMLEncode{|EITHER|} fx fy either = case either of
	LEFT x	= fx x
	RIGHT y	= fy y
XMLEncode{|PAIR|} fx fy (PAIR x y) = XMLEncNodes (getNodes` (fx x) ++ getNodes` (fy y)) (uname "PAIR")
where
	getNodes` (XMLEncNodes nodes _)	= nodes
	getNodes` res					= [wrapToElem res]
XMLEncode{|UNIT|} _ = XMLEncNodes [] (uname "UNIT")

XMLEncode{|Int|} i		= basicXML "integer" i
XMLEncode{|Char|} c		= basicXML "character" c
XMLEncode{|Real|} r		= basicXML "float" r
XMLEncode{|String|} s	= basicXML "string" s
XMLEncode{|Bool|} b		= basicXML "boolean" b

basicXML name v = XMLEncText (toString v,uname name)

XMLEncode{|[]|} fx list = XMLEncNodes (map (wrapToElem o fx) list) (uname "list")
XMLEncode{|Maybe|} fx (Just x)	= fx x
XMLEncode{|Maybe|} _ Nothing	= XMLEncNothing

XMLEncode{|XMLIntAttribute|}	fx (XMLIntAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLCharAttribute|}	fx (XMLCharAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLRealAttribute|}	fx (XMLRealAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLStringAttribute|}	fx (XMLStringAttribute name v x)	= encodeAttr name v (fx x)
XMLEncode{|XMLBoolAttribute|}	fx (XMLBoolAttribute name v x)		= encodeAttr name v (fx x)

encodeAttr name a x = XMLEncElem (fromElem (wrapToElemAttr x [XMLAttr name (toString a)]))

derive XMLEncode Either, (,), (,,), (,,,)

// auxiliary functions
wrapToElem :: !XMLEncodeResult -> XMLNode
wrapToElem x = wrapToElemAttr x []

wrapToElemAttr :: !XMLEncodeResult ![XMLAttr] -> XMLNode
wrapToElemAttr (XMLEncElem (name,attr,nodes))	attr` = XMLElem name (attr ++ attr`) nodes
wrapToElemAttr (XMLEncText t=:(txt,name))		attr` = XMLElem name attr` [toText t]
wrapToElemAttr (XMLEncNodes nodes wname)		attr` = XMLElem wname attr` nodes
wrapToElemAttr XMLEncNothing					attr` = XMLElem (uname "nothing") attr` []

toElem :: !(!XMLQName,![XMLAttr],![XMLNode]) -> XMLNode
toElem (name,attr,nodes) = XMLElem name attr nodes

fromElem :: !XMLNode -> (!XMLQName,![XMLAttr],![XMLNode])
fromElem n = case n of
	XMLElem name attr nodes -> (name,attr,nodes)
	_                       -> abort "fromElem called on non-XMLElem\n"

toText :: !(!String,!XMLQName) -> XMLNode
toText (txt,_) = XMLText txt

getNodes :: !XMLEncodeResult -> [XMLNode]
getNodes (XMLEncElem elem)		= [toElem elem]
getNodes (XMLEncText txt)		= [toText txt]
getNodes (XMLEncNodes nodes _)	= nodes
getNodes XMLEncNothing			= []

