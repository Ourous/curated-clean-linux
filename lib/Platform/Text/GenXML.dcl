definition module Text.GenXML

/**
* This module provides data types for easy construction of XML documents and
* a generic printer/parser.
*/

import StdOverloaded, StdGeneric, Data.Either
from Data.Maybe	import :: Maybe
from Data.Error	import :: MaybeErrorString, :: MaybeError

:: XMLDoc = XMLDoc !(Maybe XMLURI) ![(!XMLNamespacePrefix,!XMLURI)] !XMLNode

:: XMLNode	= XMLElem !XMLQName ![XMLAttr] ![XMLNode]
			| XMLText !String

:: XMLAttr = XMLAttr !XMLQName !String

:: XMLQName = XMLQName !(Maybe XMLNamespacePrefix) !XMLName

:: XMLNamespacePrefix :== String
:: XMLURI :== String
:: XMLName :== String

/**
* Create an XMLQName containing an unqualified name from a String
* @param Unqualified name
* @return XMLQName containing the unqualified name
*/
uname ::         !String -> XMLQName

/**
* Create an XMLQName containing a qualified name from a String
* @param Qualified name
* @return XMLQName containing the qualified name
*/
qname :: !XMLNamespacePrefix !String -> XMLQName

instance toString XMLDoc
instance fromString (MaybeErrorString XMLDoc)

// generic printer

toXML			:: !a -> XMLDoc	| XMLEncode{|*|} a
toXMLString		:: !a -> String	| XMLEncode{|*|} a

:: XMLEncodeResult
generic XMLEncode a :: !a -> XMLEncodeResult

// special types for adding attributes to XML data
:: XMLIntAttribute		a = XMLIntAttribute		!XMLQName !Int		!a
:: XMLCharAttribute		a = XMLCharAttribute	!XMLQName !Char		!a
:: XMLRealAttribute		a = XMLRealAttribute	!XMLQName !Real		!a
:: XMLStringAttribute	a = XMLStringAttribute	!XMLQName !String	!a
:: XMLBoolAttribute		a = XMLBoolAttribute	!XMLQName !Bool		!a

derive XMLEncode OBJECT, CONS of d, FIELD of d, PAIR, EITHER, UNIT, Int, Char, Real, String, Bool
derive XMLEncode Maybe, Either, (,), (,,), (,,,), []
derive XMLEncode XMLIntAttribute, XMLCharAttribute, XMLRealAttribute, XMLStringAttribute, XMLBoolAttribute

