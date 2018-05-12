implementation module iTasks.UI.JS.Encoding

import iTasks.UI.JS.Interface
import Text.GenJSON
import Text.Encodings.Base64
import StdMisc, StdArray, StdTuple, StdList
import dynamic_string

/*
When we encode values on the server we directly encode to the representation used by the Sapl run-time such that
additional decoding on the client is not longer necessary.
*/
/*
* Format of sapl representation:
ADTs:
[<index of cons>,<name of cons>, <args ...>]
Records (same as ADT, record type with an underscore prepended is used as cons name):
[0, '_' + <name of type>, <args ...>]
Primitives:
[<boxed primitive>]
Thunks:
[<function ref>,[<args ...>]]
*/

encodeOnServer :: !a -> JSONNode | JSEncode{|*|} a
encodeOnServer x = case JSEncode{|*|} x of 
	[node] = node
	_      = JSONError

decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)
decodeOnClient val world = undef //Implemented in iTasks/Sapl FFI

generic JSEncode t :: !t -> [JSONNode]
JSEncode{|Int|} x = [JSONArray [JSONInt x]]
JSEncode{|Real|} x = [JSONArray [JSONReal x]]
JSEncode{|Char|} x = [JSONArray [JSONString {x}]]
JSEncode{|Bool|} x = [JSONArray [JSONBool x]]
JSEncode{|String|} x = [JSONArray [JSONString x]]
JSEncode{|UNIT|} (UNIT) = []
JSEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list

JSEncode{|EITHER|} fx fy (LEFT x) = fx x
JSEncode{|EITHER|} fx fy (RIGHT y) = fy y
JSEncode{|OBJECT|} fx (OBJECT x) = fx x
JSEncode{|CONS of {gcd_name,gcd_index}|} fx (CONS x) = [JSONArray [JSONInt gcd_index, JSONString gcd_name : fx x]]
JSEncode{|RECORD of {grd_name}|} fx (RECORD x) = [JSONArray [JSONInt 0, JSONString ("_" +++ grd_name) : fx x]]
JSEncode{|FIELD|} fx (FIELD x) = fx x
JSEncode{|{}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSEncode{|{!}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSEncode{|(->)|} fx fy x = [JSONString "error"]

derive JSEncode [],(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,)

encodeOnClient :: !a *JSWorld -> (!JSVal a, !*JSWorld)
encodeOnClient val world = undef //Implemented in iTasks/Sapl FFI

decodeOnServer :: !JSONNode -> (Maybe a) | JSDecode{|*|} a 
decodeOnServer node = fst (JSDecode{|*|} [node])

//Currently, this is just a copy of JSONDecode without the special treatment of maybe values
//but the encoding could be further optimized for its use in editlets
generic JSDecode t :: ![JSONNode] -> (!Maybe t, ![JSONNode])

JSDecode{|Int|} [JSONInt i:xs] = (Just i, xs)
JSDecode{|Int|} l              = (Nothing, l)

JSDecode{|Real|} [JSONReal r:xs] = (Just r, xs)
JSDecode{|Real|} [JSONInt i:xs]	 = (Just (toReal i), xs)
JSDecode{|Real|} l				 = (Nothing, l)

JSDecode{|Char|} l=:[JSONString s:xs]
	| size s == 1   = (Just s.[0],xs)
				    = (Nothing, l)
JSDecode{|Char|} l  = (Nothing, l)

JSDecode{|Bool|} [JSONBool b:xs] = (Just b,xs)
JSDecode{|Bool|} l               = (Nothing, l)

JSDecode{|String|} [JSONString s:xs] = (Just s, xs)
JSDecode{|String|} l                 = (Nothing, l)

JSDecode{|UNIT|} l                   = (Just UNIT, l)

JSDecode{|PAIR|} fx fy l = d1 fy (fx l) l
  where
  d1 :: !([JSONNode] -> (!Maybe b, ![JSONNode])) !(!Maybe a, ![JSONNode]) ![JSONNode]
     -> (!Maybe (PAIR a b), ![JSONNode])
  d1 fy (Just x,xs)  l = d2 x (fy xs) l
  d1 _  (Nothing, _) l = (Nothing, l)

  d2 :: !a !(!Maybe b, ![JSONNode]) ![JSONNode] -> (!Maybe (PAIR a b), ![JSONNode])
  d2 x (Just y, ys) l = (Just (PAIR x y), ys)
  d2 x (Nothing, _) l = (Nothing, l)

JSDecode{|EITHER|} fx fy l = case fx l of
	(Just x, xs)				= (Just (LEFT x),xs)
	(Nothing, xs)				= case fy l of
		(Just y, ys)			= (Just (RIGHT y),ys)
		(Nothing, ys)			= (Nothing, l)

JSDecode{|OBJECT|} fx l = case fx l of
	(Just x, xs)	= (Just (OBJECT x),xs)
	_				= (Nothing, l)

JSDecode{|CONS of {gcd_name}|} fx l=:[JSONArray [JSONString name:fields] :xs]
	| name == gcd_name				= case fx fields of
		(Just x, _)					= (Just (CONS x), xs)
		_							= (Nothing, l)
	| otherwise						= (Nothing, l)		
JSDecode{|CONS|} fx l = (Nothing, l)

JSDecode{|RECORD|} fx l=:[obj=:JSONObject fields : xs] = d (fx [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSDecode{|RECORD|} fx l=:[obj=:JSONArray fields : xs] = d (fx [obj]) xs l
  where
  d :: !(!Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSDecode{|RECORD|} fx l = (Nothing,l)

JSDecode{|FIELD of {gfd_name}|} fx l =:[JSONObject fields]
  #! field = findField gfd_name fields
  = case fx field of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
  where
  findField :: !String ![(!String, !JSONNode)] -> [JSONNode]
  findField match [(l,x):xs]
    | l == match = [x]
    | otherwise  = findField match xs
  findField match [] = []
JSDecode{|FIELD of {gfd_index}|} fx l =:[JSONArray fields]
  #! field = fields !! gfd_index
  = case fx [field] of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
JSDecode{|FIELD|} fx l = (Nothing, l)

JSDecode{|[]|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just x, xs)
		_				= (Nothing, l)
JSDecode{|[]|} fx l = (Nothing, l)

JSDecode{|(,)|} fx fy l =:[JSONArray [xo,yo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)		= (Just (x,y), xs)
			_				= (Nothing, l)
		_					= (Nothing, l)
JSDecode{|(,)|} fx fy l	= (Nothing, l)

JSDecode{|(,,)|} fx fy fz l =:[JSONArray [xo,yo,zo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)			= case fz [zo] of
				(Just z,_)		= (Just (x,y,z), xs)
				_				= (Nothing, l)
			_					= (Nothing, l)
		_						= (Nothing, l)
JSDecode{|(,,)|} fx fy fz l		= (Nothing, l)

JSDecode{|(,,,)|} fx fy fz fi l =:[JSONArray [xo,yo,zo,io]:xs]
	= case fx [xo] of
		(Just x,_) = case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)		= (Just (x,y,z,i), xs)
					_				= (Nothing, l)
				_					= (Nothing, l)
			_						= (Nothing, l)
		_							= (Nothing, l)
JSDecode{|(,,,)|} fx fy fz fi l		= (Nothing, l)

JSDecode{|(,,,,)|} fx fy fz fi fj l =:[JSONArray [xo,yo,zo,io,jo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)	= case fj [jo] of
						(Just j,_)		= (Just (x,y,z,i,j), xs)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSDecode{|(,,,,)|} fx fy fz fi fj l		= (Nothing, l)

JSDecode{|(,,,,,)|} fx fy fz fi fj fk l =:[JSONArray [xo,yo,zo,io,jo,ko]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)	= case fj [jo] of
						(Just j,_)		= case fk [ko] of
                            (Just k, _) = (Just (x,y,z,i,j,k), xs)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSDecode{|(,,,,,)|} fx fy fz fi fj fk l	= (Nothing, l)

JSDecode{|(,,,,,,)|} fx fy fz fi fj fk fm l =:[JSONArray [xo,yo,zo,io,jo,ko,mo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_) = case fj [jo] of
						(Just j,_) = case fk [ko] of
                            (Just k, _) = case fm [mo] of
                              (Just m, _) = (Just (x,y,z,i,j,k,m), xs)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSDecode{|(,,,,,,)|} fx fy fz fi fj fk fm l = (Nothing, l)

JSDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn l =:[JSONArray [xo,yo,zo,io,jo,ko,mo,no]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_) = case fj [jo] of
						(Just j,_) = case fk [ko] of
                            (Just k, _) = case fm [mo] of
                              (Just m, _) = case fn [no] of
                                (Just n, _) = (Just (x,y,z,i,j,k,m,n), xs)
                                _           = (Nothing, l)
                              _           = (Nothing, l)
                            _           = (Nothing, l)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSDecode{|(,,,,,,,)|} fx fy fz fi fj fk fm fn l	= (Nothing, l)

JSDecode{|{}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSDecode{|{}|} fx l = (Nothing, l)

JSDecode{|{!}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSDecode{|{!}|} fx l = (Nothing, l)

decodeItems :: !([JSONNode] -> (!Maybe a, ![JSONNode])) ![JSONNode] -> Maybe [a]
decodeItems fx [] 		= Just []
decodeItems fx [ox:oxs]	= case fx [ox] of
	(Just x, _)	= case decodeItems fx oxs of
		(Just xs)	= Just [x:xs]
		_ 			= Nothing
	_			= Nothing
