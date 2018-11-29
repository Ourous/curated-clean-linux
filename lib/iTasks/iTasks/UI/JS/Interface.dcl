definition module iTasks.UI.JS.Interface

import StdString, StdGeneric, Data.Maybe
import iTasks.WF.Definition

from Control.Applicative import class pure, class <*>
import Data.Functor
from Control.Monad       import class Monad

derive class iTask JSVal, JSObject, JSFunction, JSArray

:: DomElementId	:== String

/**
* This module provides access to the javascript world of webbrowsers
* where all the client side objects of which the iTask world live.
*/

:: JSIO a = JSIO (*JSWorld -> *(a, *JSWorld))

instance pure JSIO
instance <*> JSIO
instance Functor JSIO
instance Monad JSIO

:: *JSWorld
:: JSVal a		//Pointer to a javascript value
:: JSObj a :== JSVal (JSObject a)
:: JSFun a :== JSObj (JSFunction a)
:: JSArr a :== JSObj (JSArray a)
:: JSArg

:: JSFunction a	//A javascript function object
:: JSArray a	//A javascript array object
:: JSObject a
:: JSWindow		//Represents the global window object
:: JSDocument	//Represents the global window.document object
:: JSEvent		//Represents an event object


//CORE JAVASCRIPT ACCESS

//Constants
jsNull				:: JSVal a		  // Can be any type
jsWindow			:: JSObj JSWindow	  // Singleton 'window' object that serves a global scope
jsDocument			:: JSObj JSDocument // Singleton? 'document'

//Manipulating objects
jsEmptyObject       ::                               !*JSWorld -> *(!JSObj a, !*JSWorld) // {}
jsNewObject         :: !String ![JSArg]              !*JSWorld -> *(!JSObj b, !*JSWorld)
jsGetObjectAttr     :: !String            !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl       :: !Int               !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsSetObjectAttr     :: !String !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsSetObjectEl       :: !Int    !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsDeleteObjectAttr  :: !String            !(JSObj o) !*JSWorld -> *JSWorld

class JSObjAttr a where
  jsSetter :: !a !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
  jsGetter :: !a            !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)

instance JSObjAttr String
instance JSObjAttr Int

:: JSObjSelector

class (.#) infixl 3 s :: !s !t -> JSObjSelector | JSObjAttr t 
instance .# (JSVal o)
instance .# JSObjSelector

getElementById :: !String -> JSObjSelector

.? :: !JSObjSelector !*JSWorld -> *(!JSVal r, !*JSWorld)
(.=) infixl 2 :: !JSObjSelector !v -> *(*JSWorld -> *JSWorld)

class (.$) infixl 1 o :: !o !a -> *(*JSWorld -> *(!JSVal r, !*JSWorld)) | ToArgs a
instance .$ String
instance .$ JSObjSelector

(.$!) infixl 1 :: !o !a -> *(*JSWorld -> *JSWorld) | .$ o & ToArgs a

new                 :: !String !a -> (*JSWorld -> *(!JSObj o, !*JSWorld)) | ToArgs a

//Calling js functions
jsApply				:: !(JSFun f) !(JSObj scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)

//Wrapping clean functions
jsWrapFun           :: !([JSArg] *JSWorld -> *(!JSVal a, !*JSWorld)) !*JSWorld -> *(!JSFun f, !*JSWorld)

//Special keywords
jsThis				:: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsTypeof			:: !(JSVal a) -> String
jsAbort             :: a -> b

toJSVal 			:: !a -> JSVal b
toJSArg 			:: !a -> JSArg
toJSArgs 			:: ![a] -> [JSArg]
fromJSArgUnsafe     :: !JSArg -> Dynamic
fromJSArg 			:: !JSArg !*JSWorld -> *(!Dynamic, !*JSWorld)
fromJSValUnsafe		:: !(JSVal a) -> Dynamic
fromJSVal 			:: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)

newJSArray          :: !*JSWorld -> *(!JSArr a, !*JSWorld)

//Storing arbitrary Clean values in as properties on a Javascript object
jsPutCleanVal       :: !String !a !(JSVal o) !*JSWorld -> *JSWorld
jsGetCleanVal       :: !String    !(JSVal o) !*JSWorld -> *(!a,!*JSWorld)

//USEFUL DERIVED UTIL FUNCTIONS

jsArrayPush         :: !(JSVal a) !(JSArr a)      !*JSWorld -> *(!JSArr a, !*JSWorld)
jsArrayPop          :: !(JSArr a)                 !*JSWorld -> *(!JSVal a, !*JSWorld)
jsArrayReverse      :: !(JSArr a)                 !*JSWorld -> *(!JSArr a, !*JSWorld)
toJSArray           :: ![a]                       !*JSWorld -> *(!JSArr a, !*JSWorld)
fromJSArray         :: !(JSArr a) !((JSVal b) -> c) !*JSWorld -> *(![c], !*JSWorld)

jsIsUndefined 		:: !(JSVal a) -> Bool

//Call a method on a javascript object. Object can be (JSVal null)
callObjectMethod	:: !String ![JSArg] !(JSObj o) !*JSWorld -> *(!JSVal c, !*JSWorld)

//Get a value from the global scope.
//The argument may be in dotted notation (e.g. google.maps.MayTypeId.ROADMAP) for deep searching
findObject			:: !String !*JSWorld -> *(!JSVal a, !*JSWorld)

//Load external JS by its URL. A continuation can be given,
//which is called when script is actually loaded
addJSFromUrl		:: !String !(Maybe (JSFun f)) !*JSWorld -> *JSWorld
//Load external CSS stylesheet by its URL
addCSSFromUrl       :: !String !*JSWorld -> *JSWorld

jsTrace :: !a !*JSWorld -> *JSWorld

jsValToString :: !(JSVal a) -> String
jsValToReal   :: !(JSVal a) -> Real
jsValToInt    :: !(JSVal a) -> Int
jsValToBool   :: !(JSVal a) -> Bool

jsArgToString :: !JSArg -> String
jsArgToReal   :: !JSArg -> Real
jsArgToInt    :: !JSArg -> Int
jsArgToBool   :: !JSArg -> Bool

withDef     :: !((JSVal a) -> b) !b !(JSVal a) -> b

class ToArgs a where
  toArgs :: !a -> [JSArg]

instance ToArgs Int

instance ToArgs Bool

instance ToArgs Real

instance ToArgs Char

instance ToArgs String

instance ToArgs ()

instance ToArgs JSArg

instance ToArgs (JSVal a)

instance ToArgs [JSArg]

instance ToArgs (a, b)

instance ToArgs (a, b, c)

instance ToArgs (a, b, c, d)

instance ToArgs (a, b, c, d, e)

instance ToArgs (a, b, c, d, e, f)

instance ToArgs a

callFunction :: !String ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)

jsUnsafeCoerce :: !(JSVal a) -> JSVal b

jsUnsafeObjCoerce :: !(JSVal a) -> JSObj b

jsUnsafeArrCoerce :: !(JSVal a) -> JSArr b

jsUnsafeFunCoerce :: !(JSVal a) -> JSFun b

ifObj :: !(JSVal a) !((JSObj b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)

ifArr :: !(JSVal a) !((JSArr b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)

ifFun :: !(JSVal a) !((JSFun b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)

jsIsArray :: !(JSVal a) !*JSWorld -> *(!Bool, !*JSWorld)

jsIsNull :: !(JSVal a) -> Bool

