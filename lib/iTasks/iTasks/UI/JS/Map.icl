implementation module iTasks.UI.JS.Map

import iTasks.UI.JS.Interface
import StdBool, StdTuple, StdList, StdMisc

jsNewMap :: !*JSWorld -> *(!JSMap k (JSVal v), !*JSWorld) | toString k
jsNewMap world = jsEmptyObject world

jsPut :: !k !(JSVal v) !(JSMap k (JSVal v)) !*JSWorld -> *JSWorld | toString k
jsPut key val m world
	= jsSetObjectAttr (toString key) val m world
	
jsDel :: !k !(JSMap k (JSVal v)) !*JSWorld -> *JSWorld | toString k
jsDel key m world
	= jsDeleteObjectAttr (toString key) m world
	
jsGet :: !k !(JSMap k (JSVal v)) !*JSWorld -> *(!Maybe (JSVal v), !*JSWorld) | toString k
jsGet key m world
	# (val, world) = jsGetObjectAttr (toString key) m world
	| jsIsUndefined val
	= (Nothing, world)
	= (Just val, world)
	
jsToList :: !(JSMap k (JSVal v)) !*JSWorld -> *(![(!String, !JSVal v)], !*JSWorld) | toString k
jsToList m world
	# (object, world) = findObject "Object" world
	# (keys, world) = callObjectMethod "keys" [toJSArg m] object world

	# keys = case fromJSValUnsafe keys of
					(keys :: [String]) 	= keys
										= abort "List of strings was expected but something else came"
									
	= foldl readprop ([], world) (reverse keys)
where
	readprop (ps, world) k = let (val, w) = jsGetObjectAttr k m world in ([(k,val):ps], w)
	
	
