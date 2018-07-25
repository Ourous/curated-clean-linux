definition module Text.URI

// Ported from Haskell Text.URI (uri-0.1.5) by László Domoszlai

import StdInt
import Data.Maybe

// Represents a general universal resource identifier using
// its component parts.
//
// For example, for the URI
//
//     foo://anonymous@www.haskell.org:42/ghc?query#frag
//
// the components are:

:: URI = 
		{ uriScheme		:: !Maybe String // foo
		, uriUserInfo	:: !Maybe String // anonymous
		, uriRegName	:: !Maybe String // www.haskell.org
		, uriPort		:: !Maybe Int    // 42
		, uriPath		:: !String       // /ghc
		, uriQuery 		:: !Maybe String // query
		, uriFragment 	:: !Maybe String // frag
		}

// Blank URI
nullURI :: URI

instance toString URI

// Checks if character is OK in userinfo
okInUserinfo :: Char -> Bool
// Checks if character is OK in query
okInQuery :: Char -> Bool
// Checks if character is OK in urlencoded query item
okInQueryItem :: Char -> Bool
// Checks if character is OK in fragment
okInFragment :: Char -> Bool
// Checks if character is OK in path
okInPath :: Char -> Bool
// Checks if character is ok in path segment
okInPathSegment :: Char -> Bool

// Parses URI
parseURI :: String -> Maybe URI

// Escapes string, using predicate to determine whether character is allowed
escapeString :: (Char -> Bool) !String -> String
// Convenience function for extracting www-urlencoded data
uriQueryItems :: !URI -> [(String, String)]
// Splits path to segments
pathToSegments :: !String -> [String]
// Convenience function for extracting path segments
uriPathSegments :: !URI -> [String]
// Joins path segments, with escaping
segmentsToPath :: ![String] -> String
// Checks if uri is a reference
isReference :: URI -> Bool
// Checks if uri is relative
isRelative :: URI -> Bool


