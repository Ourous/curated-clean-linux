definition module iTasks.UI.JS.Encoding
/**
* This module provides encoding/decoding functions for communicating values efficiently
* between an itasks server application and its client (webbrowser).
* It uses an encoding of Clean values as JSON that can be decoded natively in javascript
*/
import iTasks.UI.JS.Interface
import StdGeneric
from Text.GenJSON import :: JSONNode (..)
from StdList import !!

//Sending values server -> client
encodeOnServer :: !a -> JSONNode | JSEncode{|*|} a //Don't specialize JSEncode, it will break decoding
decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)

//Sending values client -> server
encodeOnClient :: !a *JSWorld -> (!JSVal a, !*JSWorld)
decodeOnServer :: !JSONNode -> (Maybe a) | JSDecode{|*|} a //Don't sepcialize JSDecode, it will break on the fixed encoding 

generic JSEncode t :: !t -> [JSONNode]
derive  JSEncode Int, Real, Char, Bool, String, UNIT, [],
	(,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!}, (->),
    EITHER, OBJECT

JSEncode{|CONS of {gcd_name,gcd_index}|} fx (CONS x) = [JSONArray [JSONInt gcd_index, JSONString gcd_name : fx x]]
JSEncode{|RECORD of {grd_name}|} fx (RECORD x) = [JSONArray [JSONInt 0, JSONString ("_" +++ grd_name) : fx x]]

JSEncode{|FIELD|} fx (FIELD x) = fx x

JSEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
where
    (++) infixr 5::![.a] !u:[.a] -> u:[.a]
    (++) [hd:tl]    list    = [hd:tl ++ list]
    (++) nil        list    = list

generic JSDecode t :: ![JSONNode] -> (!Maybe t,![JSONNode])
derive  JSDecode Int, Real, Char, Bool, String, UNIT, EITHER, CONS of {gcd_name}, OBJECT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!} 

JSDecode{|PAIR|} fx fy l = d1 fy (fx l) l
  where
  d1 :: !([JSONNode] -> (!Maybe b, ![JSONNode])) !(!Maybe a, ![JSONNode]) ![JSONNode]
     -> (!Maybe (PAIR a b), ![JSONNode])
  d1 fy (Just x,xs)  l = d2 x (fy xs) l
  d1 _  (Nothing, _) l = (Nothing, l)

  d2 :: !a !(!Maybe b, ![JSONNode]) ![JSONNode] -> (!Maybe (PAIR a b), ![JSONNode])
  d2 x (Just y, ys) l = (Just (PAIR x y), ys)
  d2 x (Nothing, _) l = (Nothing, l)

JSDecode{|RECORD|} fx l=:[obj=:JSONObject fields : xs] = d (fx [obj]) xs l
  where
  d :: !(Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSDecode{|RECORD|} fx l=:[obj=:JSONArray fields : xs] = d (fx [obj]) xs l
  where
  d :: !(Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
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
