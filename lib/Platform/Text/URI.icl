implementation module Text.URI

import StdBool, StdChar, StdString, StdList, StdMisc
import Data.Maybe, Data.List, Data.Either, Data.Functor
import Text.Parsers.ZParsers.Parsers
import Text.Encodings.UrlEncoding

from StdFunc import id
from Text import qualified join, startsWith, split
from Text import class Text, instance Text String

import Control.Monad, Control.Applicative

nullURI :: URI
nullURI = {
	  uriScheme = Nothing
	, uriRegName = Nothing
	, uriUserInfo = Nothing
	, uriPort = Nothing
	, uriPath = ""
	, uriQuery = Nothing
	, uriFragment = Nothing
	}

instance toString URI
where
	toString u = 'Text'.join "" [
		  maybe "" (\s -> s +++ ":") u.uriScheme
		, if (isJust u.uriRegName) "//" ""
		, maybe "" (\s -> s +++ "@") u.uriUserInfo
		, fromMaybe "" u.uriRegName
		, maybe "" (\s -> ":" +++ toString s) u.uriPort
		, if (isJust u.uriRegName && not ('Text'.startsWith "/" u.uriPath || u.uriPath == "")) ("/" +++ u.uriPath) u.uriPath
		, maybe "" ((+++) "?") u.uriQuery
		, maybe "" ((+++) "#") u.uriFragment
		]

okInUserinfo :: Char -> Bool
okInUserinfo c = satisfiesAny [isUnreserved, isSubDelim, ((==) ':')] c

okInQuery :: Char -> Bool
okInQuery c = satisfiesAny [isPChar, (\c -> elem c ['/?'])] c

okInQueryItem :: Char -> Bool
okInQueryItem c = okInQuery c && (not (elem c ['&=']))

okInFragment :: Char -> Bool
okInFragment c = okInQuery c

okInPath :: Char -> Bool
okInPath c = satisfiesAny [isPChar, (\c -> elem c ['/@'])] c

okInPathSegment :: Char -> Bool
okInPathSegment c = satisfiesAny [isPChar, ((==) '@')] c

parseURI :: String -> Maybe URI
parseURI s = case parse uriP ss "user input" "" of
				Succ [r] = Just r
						 = Nothing
where
	ss :: [Char]
	ss = fromString s

// Escapes one char, see escapeString
escapeChar :: (Char -> Bool) Char -> [Char]
escapeChar f c = if (f c && c <> '%') [c] (let (a,b) = charToHex c in ['%',a,b])
where
	charToHex :: !Char -> (!Char, !Char)
	charToHex c	= (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
	where
	        i = toInt c
	        digitToHex :: !Int -> Int
	        digitToHex d
	                | d <= 9		= d + toInt '0'
	                | otherwise		= d + toInt 'A' - 10

// Escapes string, using predicate to determine whether character is allowed
escapeString :: (Char -> Bool) !String -> String
escapeString f s = toString (concatMap (escapeChar f) (fromString s))

// Convenience function for extracting www-urlencoded data
uriQueryItems :: !URI -> [(String, String)]
uriQueryItems u = maybe [] urlDecodePairs u.uriQuery

// Splits path to segments
pathToSegments :: !String -> [String]
pathToSegments p = 'Text'.split "/" p

// Convenience function for extracting path segments
uriPathSegments :: !URI -> [String]
uriPathSegments u = pathToSegments u.uriPath

// Joins path segments, with escaping
segmentsToPath :: ![String] -> String
segmentsToPath [""] = "/"
segmentsToPath ss = 'Text'.join "/" (map (escapeString (okInPathSegment)) ss)

// Checks if uri is a reference
isReference :: URI -> Bool
isReference u = all (isNothing) [u.uriRegName, u.uriScheme]

// Checks if uri is relative
isRelative :: URI -> Bool
isRelative u = isReference u && not ('Text'.startsWith "/" u.uriPath)

// Parser

// sepBy version thet returns full parsed string
sepByWSep p sep = sepByWSep1 p sep <!> pure []

// Character classes

isGenDelim c = elem c [':/?#[]@']
isSubDelim c = elem c ['!$&\'()*+,;=']
isReserved c = isGenDelim c || isSubDelim c
isUnreserved c = isAlphanum c || elem c ['-._~']
isPChar c = satisfiesAny [isUnreserved, isSubDelim, (\c -> elem c ['%:@'])] c

satisfiesAny :: [a -> Bool] a -> Bool
satisfiesAny fs a = or (map (\f -> f a) fs)

sepByWSep1 p sep =
	p >>= \first ->
	<!*> pP >>= \rest -> pure (flatten [first: rest])
where	
	pP = sep >>= \sepV -> p >>= \pV -> pure [sepV: pV]
	
percentEncodedP =
	symbol '%' >>|
	hexDigit >>= \d1 ->
	hexDigit >>= \d2 ->
	pure (toChar (hdi d1*16+hdi d2))
where
	hdi d | isDigit d = digitToInt d
	hdi d = (toInt (toLower d)) - (toInt 'a') + 10

reservedP = satisfy isReserved
unreservedP = satisfy isUnreserved
genDelimP = satisfy isGenDelim
subDelimP = satisfy isSubDelim
pCharP = satisfy isPChar

optionMaybe p = <!?> p Just Nothing
option d p = <!?> p id d

uriP = 
	optionMaybe schemeP >>= \schemeV ->
	hierPartP >>= \(authorityV, pathV) ->
	pure (fromMaybe (Nothing, Nothing, Nothing) authorityV) >>= \(userinfoV, hostV, portV) ->
	optionMaybe (symbol '?' >>| queryP) >>= \queryV ->
	optionMaybe (symbol '#' >>| fragmentP) >>= \fragmentV ->
	pure 
		{ uriScheme = fmap toString schemeV
		, uriRegName = fmap toString hostV
		, uriPort = portV
		, uriPath = toString pathV
		, uriUserInfo = fmap toString userinfoV
		, uriQuery = fmap toString queryV
		, uriFragment = fmap toString fragmentV
		}

schemeP = 
	letter >>= \l ->
	<!*> (alphaNum <!> oneOf ['+-.']) >>= \ls ->
	symbol ':' >>|
	pure [l:ls]

hierPartP =
	optionMaybe authP >>= \authorityV ->
	pathP >>= \pathV ->
	pure (authorityV, pathV)
where
	authP = token ['//'] >>| authorityP

// Path parser
pathP = choice [pathRootlessP, pathAbsoluteP, pathNoSchemeP, pathABEmptyP, pathEmptyP]

pathABEmptyP =
	<!*> partP >>= \segs ->
	pure (flatten segs)
where
	partP = symbol '/' >>| segmentP >>= \segmentV -> pure ['/': segmentV]

pathAbsoluteP =
	symbol '/' >>|
	option [] segP >>= \rest -> pure ['/': rest]
where
	segP = segmentNZP >>= \s1 -> <!*> partP >>= \segs -> pure (flatten [s1: segs])
	partP = symbol '/' >>| segmentP >>= \v -> pure ['/': v]
	
pathNoSchemeP =
	segmentNZNCP >>= \first ->
	sepByWSep segmentP (symbol '/') >>= \rest ->
	pure (first ++ rest)

pathRootlessP =
	segmentNZP >>= \first ->
	sepByWSep segmentP (symbol '/') >>= \rest ->
	pure (first ++ rest)

pathEmptyP = epsilon
segmentP = <!*> pCharP
segmentNZP = <!+> pCharP
segmentNZNCP = <!+> (subDelimP <!> unreservedP <!> oneOf ['@%'])

authorityP =
	optionMaybe (userinfoP >>= \result -> symbol '@' >>| pure result) >>= \userinfoV ->
	hostP >>= \hostV ->
	optionMaybe (symbol ':' >>| portP) >>= \portV ->
	pure (userinfoV, (Just hostV), portV)

hostP = ipLiteralP <|> ipv4AddressP <|> regNameP

// ip v6+ parser
ipLiteralP = 
	symbol '[' >>|
	ipv6AddressP <|> ipvFutureP >>= \result ->
	symbol ']' >>|
	pure result

// Future IP parser
ipvFutureP =
	token ['v'] >>= \v ->
	<!+> hexDigit >>= \versionV ->
	token ['.'] >>= \dot ->
	<!+> (satisfy (satisfiesAny [isUnreserved, isSubDelim, ((==)':')])) >>= \datV ->
	pure (flatten [v, versionV, dot, datV])

// Parse h16 followed by a colon, with no backtracking on failure.
h16Colon =
	h16 >>= \h ->
	symbol ':' >>| 
	pure (h ++ [':'])

// Process 0..n instances of the specified parser, backtracking on failure.
h16n 0 = pure []
h16n n = sequence (intersperse (token [':']) [h16 \\ i<-[1..n]])
upToh16 n = choice (reverse [h16n x \\ x <- [0..n]])

ipv6AddressP = choice [
		count 6 h16Colon >>= \hs -> 
		ls32 >>= \s -> 
		pure (flatten hs ++ s),
		
		token ['::'] >>= \co -> 
		count 5 h16Colon >>= \hs -> 
		ls32 >>= \s -> 
		pure (co ++ flatten hs ++ s),
		
		option [] h16 >>= \p -> 
		token ['::'] >>= \co -> 
		count 4 h16Colon >>= \hs -> 
		ls32 >>= \s -> 
		pure (p ++ co ++ flatten hs ++ s),

		upToh16 2 >>= \ps ->
		token ['::'] >>= \co -> 
		count 3 h16Colon >>= \hs -> 
		ls32 >>= \s -> 
		pure (flatten ps ++ co ++ flatten hs ++ s),
		
		upToh16 3 >>= \ps ->
		token ['::'] >>= \co -> 
		count 2 h16Colon >>= \hs -> 
		ls32 >>= \s -> 
		pure (flatten ps ++ co ++ flatten hs ++ s),

		upToh16 4 >>= \ps ->
		token ['::'] >>= \co -> 
		h16Colon >>= \h -> 
		ls32 >>= \s -> 
		pure (flatten ps ++ co ++ h ++ s),

		upToh16 5 >>= \ps ->
		token ['::'] >>= \co -> 
		ls32 >>= \s -> 
		pure (flatten ps ++ co ++ s),

		upToh16 6 >>= \ps ->
		token ['::'] >>= \co -> 
		h16 >>= \h -> 
		pure (flatten ps ++ co ++ h),
		
		upToh16 7 >>= \ps ->
		token ['::'] >>= \co -> 
		pure (flatten ps ++ co)]

h16 = choice [count x hexDigit \\ x <- [4,3,2,1]]
ls32 = 
	(h16n 2 >>= \r -> pure (flatten r))
	<!>
	ipv4AddressP

// ipv4Address parser
ipv4AddressP =
	decOctetP >>= \d1 ->
	symbol '.' >>|
	decOctetP >>= \d2 ->
	symbol '.' >>|
	decOctetP >>= \d3 ->
	symbol '.' >>|
	decOctetP >>= \d4 ->
	pure (flatten [d1, ['.'], d2, ['.'], d3, ['.'], d4])

// decimal octet
decOctetP =
	countMinMax 1 3 digit >>= \a1 -> pure a1
//	if (toInt (toString a1) > 255) (abort "Decimal octet value too large") (pure a1)

regNameP = <!*> (unreservedP <|> subDelimP <|> symbol '%')

// helper
countMinMax m n p | m > 0 =
	p >>= \a1 ->
	countMinMax (m-1) (n-1) p >>= \ar ->
	pure [a1:ar]
countMinMax _ n _ | n <= 0 = pure []
countMinMax _ n p = option [] (p >>= \a1 -> countMinMax 0 (n-1) p >>= \ar -> pure [a1:ar])

// port
portP =	<!*> digit >>= \digitV -> pure (toInt (toString digitV))

// userinfo
userinfoP = <!*> (satisfy (satisfiesAny [isUnreserved, isSubDelim, ((==)':')]))
queryP = <!*> (satisfy isPChar <!> oneOf ['/?'])
fragmentP = queryP

