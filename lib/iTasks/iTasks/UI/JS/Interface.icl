implementation module iTasks.UI.JS.Interface

import iTasks

import StdGeneric, StdString, StdMisc, StdBool, StdEnum, StdTuple, StdList
import Data.Maybe, Text

import Control.Applicative
import qualified Control.Monad as M

derive class iTask JSVal, JSObject, JSFunction, JSArray

:: *JSWorld = JSWorld
:: JSVal a  = JSVal !a

// It describes what is the goal, but the actual wrapping doesn't happen,
// don't try to unwrap it!
:: JSArg = E.a: JSArg (JSVal a)

:: JSWindow     = JSWindow
:: JSDocument   = JSDocument
:: JSFunction a = JSFunction
:: JSArray    a = JSArray
:: JSObject   a = JSObject
:: JSEvent      = JSEvent

instance pure JSIO where
  pure x     = JSIO (\s -> (x, s))
instance <*> JSIO where
  (<*>) f g  = liftA2 id f g

instance Functor JSIO where
  fmap f x = 'M'.bind x (lift o f)

instance Monad JSIO where
  bind (JSIO f) a2mb = JSIO run
    where
      run world
        #! (x, world) = f world
        #! (JSIO g)     = a2mb x
        = g world

jsNull :: JSVal a
jsNull = undef

jsWindow :: JSObj JSWindow
jsWindow = undef

jsDocument :: JSObj JSDocument
jsDocument = undef

jsEmptyObject :: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsEmptyObject world = undef

jsNewObject	:: !String ![JSArg] !*JSWorld -> *(!JSObj b, !*JSWorld)
jsNewObject cons_name args world = undef

jsGetObjectAttr :: !String !(JSObj a) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectAttr attr obj world = undef

jsGetObjectEl :: !Int !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl index obj world = undef

jsSetObjectAttr	:: !String !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsSetObjectAttr attr value obj world = undef

jsSetObjectEl :: !Int !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsSetObjectEl index value obj world = undef

jsDeleteObjectAttr :: !String !(JSObj o) !*JSWorld -> *JSWorld
jsDeleteObjectAttr value obj world = undef

class JSObjAttr a where
  jsSetter :: !a !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
  jsGetter :: !a            !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)

instance JSObjAttr String where
  jsSetter idx val obj world = jsSetObjectAttr idx val obj world
  jsGetter idx     obj world = jsGetObjectAttr idx obj world

instance JSObjAttr Int where
  jsSetter idx val obj world = jsSetObjectEl idx val obj world
  jsGetter idx     obj world = jsGetObjectEl idx obj world

:: JSObjSelector = E.o.t: SObj !(JSObj o) !t & JSObjAttr t
                 | E.t: SDomId !String
                 | E.t: SRec !JSObjSelector !t & JSObjAttr t

getObject :: !JSObjSelector !*JSWorld -> *(!JSVal v, !*JSWorld)
getObject (SObj obj attr) world = jsGetter attr obj world
getObject (SDomId elemId) world = callObjectMethod "getElementById" [toJSArg elemId] jsDocument world
getObject (SRec sel attr) world
	#! (obj, world) = getObject sel world
	= jsGetter attr obj world

setObject :: !JSObjSelector !(JSVal v) !*JSWorld -> *JSWorld
setObject (SObj obj attr) val world = jsSetter attr val obj world
setObject (SRec sel attr) val world
	#! (obj, world) = getObject sel world
	= jsSetter attr val obj world

callObject :: !JSObjSelector ![JSArg] !*JSWorld -> *(!JSVal r, !*JSWorld)
callObject (SObj obj method) args world
	#! (fun, world) = jsGetter method obj world
	= jsApply fun obj args world
callObject (SRec sel method) args world
	#! (obj, world) = getObject sel world
	#! (fun, world) = jsGetter method obj world
	= jsApply fun obj args world

class (.#) infixl 3 s :: !s !t -> JSObjSelector | JSObjAttr t 

instance .# (JSVal o) where
	(.#) a b = SObj (jsUnsafeObjCoerce a) b

instance .# JSObjSelector where
	(.#) a b = SRec a b  

getElementById :: !String -> JSObjSelector
getElementById elemId = SDomId elemId

.? :: !JSObjSelector !*JSWorld -> *(!JSVal r, !*JSWorld)
.? sel world = getObject sel world

(.=) infixl 2 :: !JSObjSelector !v -> *(*JSWorld -> *JSWorld)
(.=) sel val = \world -> setObject sel (toJSVal val) world

class (.$) infixl 1 o :: !o !a -> *(*JSWorld -> *(!JSVal r, !*JSWorld)) | ToArgs a

instance .$ String where
  (.$) fun args = \world -> callFunction fun (toArgs args) world

instance .$ JSObjSelector where
  (.$) sel args = \world -> callObject sel (toArgs args) world

(.$!) infixl 1 :: !o !a -> *(*JSWorld -> *JSWorld) | .$ o & ToArgs a
(.$!) a b = snd o (a .$ b)

new :: !String !a -> (*JSWorld -> *(!JSObj o, !*JSWorld)) | ToArgs a
new objNm args = jsNewObject objNm (toArgs args)

jsApply	:: !(JSFun f) !(JSObj scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)
jsApply fun scope args world = undef

jsWrapFun :: !([JSArg] *JSWorld -> *(!JSVal a, !*JSWorld)) !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun fun world = undef

jsThis :: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsThis world = undef

jsTypeof :: !(JSVal a) -> String
jsTypeof obj = undef

jsAbort :: a -> b
jsAbort _ = undef

newJSArray :: !*JSWorld -> *(!JSArr a, !*JSWorld)
newJSArray world  = undef

toJSVal :: !a -> JSVal b
toJSVal val = undef

toJSArg :: !a -> JSArg
toJSArg val = undef

toJSArgs :: ![a] -> [JSArg]
toJSArgs xs = map toJSArg xs

fromJSArgUnsafe :: !JSArg -> Dynamic
fromJSArgUnsafe ptr = undef

fromJSArg :: !JSArg !*JSWorld -> *(!Dynamic, !*JSWorld)
fromJSArg ptr world = undef

fromJSValUnsafe :: !(JSVal a) -> Dynamic
fromJSValUnsafe ptr = undef

fromJSVal :: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)
fromJSVal ptr world = undef

jsPutCleanVal :: !String !a !(JSVal o) !*JSWorld -> *JSWorld
jsPutCleanVal key val obj world = undef

jsGetCleanVal :: !String !(JSVal o) !*JSWorld -> *(!a,!*JSWorld)
jsGetCleanVal key obj world = undef

//UTIL

jsArrayPush :: !(JSVal a) !(JSArr a) !*JSWorld -> *(!JSArr a, !*JSWorld)
jsArrayPush x arr world = callObjectMethod "push" [toJSArg x] arr world

jsArrayPop :: !(JSArr a) !*JSWorld -> *(!JSVal a, !*JSWorld)
jsArrayPop arr world = callObjectMethod "pop" [] arr world

jsArrayReverse :: !(JSArr a) !*JSWorld -> *(!JSArr a, !*JSWorld)
jsArrayReverse arr world = callObjectMethod "reverse" [] arr world

toJSArray :: ![a] !*JSWorld -> *(!JSArr a, !*JSWorld)
toJSArray xs world
  #! (arr, world) = newJSArray world
  #! world = foldl (op arr) world (zip2 [0..] xs)
  = (arr, world)
  where
  op :: !(JSArr a) !*JSWorld !(!Int, !a) -> *JSWorld
  op arr world (i, arg) = jsSetObjectEl i (toJSVal arg) arr world

fromJSArray :: !(JSArr a) !((JSVal b) -> c) !*JSWorld -> *(![c], !*JSWorld)
fromJSArray arr f world
  #! (l, world) = jsGetObjectAttr "length" arr world
  = fromJSArray` f 0 (jsValToInt l) arr world
  where
  fromJSArray` :: !((JSVal b) -> c) !Int !Int !(JSArr a) !*JSWorld -> *(![c], !*JSWorld)
  fromJSArray` f n l arr world
    | n == l         = ([], world)
    | otherwise
      #! (x, world)   = jsGetObjectEl n arr world
      #! (xs`, world) = fromJSArray` f (n + 1) l arr world
      = ([f x : xs`], world)

jsIsUndefined :: !(JSVal a) -> Bool
jsIsUndefined obj = jsTypeof obj == "undefined"

findObject :: !String !*JSWorld -> *(!JSVal a, !*JSWorld)
findObject query world
  #! (obj,world) = jsGetObjectAttr attr jsWindow world //deref first attr separate to make the typechecker happy
  = case attrs of
      []  = (obj,world)
          = foldl op (obj,world) attrs
  where
    [attr:attrs] = split "." query
    op :: !(!JSVal a, !*JSWorld) !String -> (!JSVal a, !JSWorld)
    op (obj, world) attr = ifObj obj
                             (\obj -> jsGetObjectAttr attr obj)
                             obj world

class ToArgs a where
  toArgs :: !a -> [JSArg]

instance ToArgs Int where
  toArgs n = [toJSArg n]

instance ToArgs Bool where
  toArgs b = [toJSArg b]

instance ToArgs Real where
  toArgs r = [toJSArg r]

instance ToArgs Char where
  toArgs c = [toJSArg c]

instance ToArgs String where
  toArgs s = [toJSArg s]

instance ToArgs () where
  toArgs _ = []

instance ToArgs (JSVal a) where
  toArgs x = [toJSArg x]

instance ToArgs JSArg where
  toArgs x = [x]

instance ToArgs [JSArg] where
  toArgs xs = xs

instance ToArgs (a, b) where
  toArgs (x, y) = [toJSArg x, toJSArg y]

instance ToArgs (a, b, c) where
  toArgs (x, y, z) = [toJSArg x, toJSArg y, toJSArg z]

instance ToArgs (a, b, c, d) where
  toArgs (x, y, z, p) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p]

instance ToArgs (a, b, c, d, e) where
  toArgs (x, y, z, p, q) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p, toJSArg q]

instance ToArgs (a, b, c, d, e, f) where
  toArgs (x, y, z, p, q, r) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p, toJSArg q, toJSArg r]

instance ToArgs a where
  toArgs x = [toJSArg x]

callObjectMethod	:: !String ![JSArg] !(JSObj o) !*JSWorld -> *(!JSVal c, !*JSWorld)
callObjectMethod method args obj world
	#! (fun, world) = jsGetObjectAttr method obj world
	= jsApply fun obj args world

// TODO: use instead one of these:
// 1. https://github.com/rgrove/lazyload/blob/master/lazyload.js
// 2. https://github.com/getify/LABjs

addJSFromUrl :: !String !(Maybe (JSFun a)) !*JSWorld -> *JSWorld
addJSFromUrl url mbCallback world
	//Create script tag
	#! (script,world)	= callObjectMethod "createElement" [toJSArg "script"] jsDocument world
	#! world				= jsSetObjectAttr "src" (toJSVal url) script world
	#! world				= jsSetObjectAttr "type" (toJSVal "text/javascript") script world
	#! world				= jsSetObjectAttr "async" (toJSVal False) script world
	#! world				= case mbCallback of
		Nothing			= world
		Just callback	= jsSetObjectAttr "onload" callback script world
	//Inject into the document head
	#! (head,world)		= callObjectMethod "getElementsByTagName" [toJSArg "head"] jsDocument world
	#! (head,world)		= jsGetObjectEl 0 head world
	#! (_,world)			= callObjectMethod "appendChild" [toJSArg script] head world
	= world

addCSSFromUrl :: !String !*JSWorld -> *JSWorld
addCSSFromUrl url world
    #! (link,world)      = callObjectMethod "createElement" [toJSArg "link"] jsDocument world
	#! world				= jsSetObjectAttr "rel" (toJSVal "stylesheet") link world
	#! world				= jsSetObjectAttr "type" (toJSVal "text/css") link world
	#! world				= jsSetObjectAttr "href" (toJSVal url) link world
	#! world				= jsSetObjectAttr "async" (toJSVal True) link world
	//Inject into the document head
	#! (head,world)		= callObjectMethod "getElementsByTagName" [toJSArg "head"] jsDocument world
	#! (head,world)		= jsGetObjectEl 0 head world
	#! (_,world)			= callObjectMethod "appendChild" [toJSArg link] head world
	= world

jsTrace :: !a !*JSWorld -> *JSWorld
jsTrace val world
	#! (console,world)	= findObject "console" world
	#! (_,world)			= callObjectMethod "log" [toJSArg val] console world
	= world

jsValToString :: !(JSVal a) -> String
jsValToString ptr = case fromJSValUnsafe ptr of
					(val :: String) = val
					(val :: Real)   = toString val
					(val :: Int)    = toString val
					val				= jsAbort val
									//= jsAbort "JSVal cannot be converted to String"

jsValToReal :: !(JSVal a) -> Real
jsValToReal ptr = case fromJSValUnsafe ptr of
					(val :: Real)   = val
					(val :: String)	= toReal val
					(val :: Int)    = toReal val
									= abort "Real was expected but something else came"

jsValToInt :: !(JSVal a) -> Int
jsValToInt ptr = case fromJSValUnsafe ptr of
					(val :: Int)	= val
					(val :: String)	= toInt val
					(val :: Real)	= toInt val
								   	= abort "Int was expected but something else came"

jsValToBool :: !(JSVal a) -> Bool
jsValToBool ptr = case fromJSValUnsafe ptr of
					(val :: Bool)	= val
								   	= abort "Bool was expected but something else came"

jsArgToString :: !JSArg -> String
jsArgToString ptr = case fromJSArgUnsafe ptr of
					(val :: String) = val
					(val :: Real)   = toString val
					(val :: Int)    = toString val
					val				= jsAbort val
jsArgToReal :: !JSArg -> Real
jsArgToReal ptr = case fromJSArgUnsafe ptr of
					(val :: Real)   = val
					(val :: String)	= toReal val
					(val :: Int)    = toReal val
									= abort "Real was expected but something else came"
jsArgToInt :: !JSArg -> Int
jsArgToInt ptr = case fromJSArgUnsafe ptr of
					(val :: Int)	= val
					(val :: String)	= toInt val
					(val :: Real)	= toInt val
								   	= abort "Int was expected but something else came"

jsArgToBool :: !JSArg -> Bool
jsArgToBool ptr = case fromJSArgUnsafe ptr of
					(val :: Bool)	= val
								   	= abort "Bool was expected but something else came"

withDef :: !((JSVal a) -> b) !b !(JSVal a) -> b
withDef f def ptr | jsIsUndefined ptr
	= def 
	= f ptr

callFunction :: !String ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)
callFunction fn args world = callObjectMethod fn args jsWindow world

jsUnsafeCoerce :: !(JSVal a) -> JSVal b
jsUnsafeCoerce x = undef

jsUnsafeObjCoerce :: !(JSVal a) -> JSObj b
jsUnsafeObjCoerce x = undef

jsUnsafeArrCoerce :: !(JSVal a) -> JSArr b
jsUnsafeArrCoerce x = undef

jsUnsafeFunCoerce :: !(JSVal a) -> JSFun b
jsUnsafeFunCoerce x = undef

ifObj :: !(JSVal a) !((JSObj b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifObj val f def world
  #! t = jsTypeof val
  | t == "object" || t == "string" = f (jsUnsafeObjCoerce val) world
  | otherwise                      = (def, world)

ifArr :: !(JSVal a) !((JSArr b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifArr val f def world
  #! (isArr, world) = jsIsArray val world
  | isArr     = f (jsUnsafeArrCoerce val) world
  | otherwise = (def, world)

ifFun :: !(JSVal a) !((JSFun b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifFun val f def world
  | jsTypeof val == "function" = f (jsUnsafeFunCoerce val) world
  | otherwise                  = (def, world)

jsIsArray :: !(JSVal a) !*JSWorld -> *(!Bool, !*JSWorld)
jsIsArray x world
  #! (arr, world) = findObject "Array" world
  #! (jb, world)  = callObjectMethod "isArray" [toJSArg x] arr world
  = (jsValToBool jb, world)

jsIsNull :: !(JSVal a) -> Bool
jsIsNull x = undef


