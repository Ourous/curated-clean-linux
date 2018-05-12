definition module iTasks.UI.JS.Map

import iTasks.UI.JS.Interface

:: JSMap k v :== JSArr v


jsNewMap :: 							        !*JSWorld -> *(!JSMap k (JSVal v), !*JSWorld) | toString k
jsPut 	 :: !k !(JSVal v) !(JSMap k (JSVal v))  !*JSWorld -> *JSWorld | toString k
jsDel 	 :: !k !(JSMap k (JSVal v))         	!*JSWorld -> *JSWorld | toString k
jsGet 	 :: !k !(JSMap k (JSVal v))         	!*JSWorld -> *(!Maybe (JSVal v), !*JSWorld) | toString k

jsToList :: !(JSMap k (JSVal v))        		!*JSWorld -> *(![(!String, !JSVal v)], !*JSWorld) | toString k
