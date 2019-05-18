implementation module iTasks.UI.JavaScript

import StdEnv
import StdGeneric
import StdOverloadedList

import Data.Maybe
import Text.GenJSON

:: *JSWorld = JSWorld

:: JSVal
	= JSInt !Int
	| JSBool !Bool
	| JSString !String
	| JSReal !Real

	| JSVar !String
	| JSNull
	| JSUndefined
	| JSTypeOf !JSVal
	| JSDelete !JSVal // actually a statement, not an expression

	| JSObject !{#JSObjectElement}
	| JSArray !{!JSVal}

	| JSCall !JSVal !{!JSVal}
	| JSNew !String !{!JSVal}

	| JSSel !JSVal !JSVal // x[y]
	| JSSelPath !JSVal !String // x.path1.path2...pathn

	| JSRef !Int // a reference to js
	| JSCleanRef !Int // a reference to shared_clean_values

	| JSTempPair !JSVal !JSVal
	| JSTempField !String !JSVal

	| JSUnused // used as always-False pattern match; see comments on abort_with_node.

:: JSObjectElement =
	{ key :: !String
	, val :: !JSVal
	}

instance toString JSVal
where
	toString v = let s = toS v in if (size s<0) (abort_with_node v) s
	where
		toS :: !JSVal -> String
		toS v = fst (copy v (createArray (len v 0) '\0') 0)

		copy :: !JSVal !*{#Char} !Int -> (!.{#Char}, !Int)
		copy v dest i = case v of
			JSInt n
				-> copy_chars (toString n) dest i
			JSBool True
				# dest & [i]='t',[i+1]='r',[i+2]='u',[i+3]='e'
				-> (dest,i+4)
			JSBool False
				# dest & [i]='f',[i+1]='a',[i+2]='l',[i+3]='s',[i+4]='e'
				-> (dest,i+5)
			JSString s
				# dest & [i] = '\''
				# (dest,i) = copy_and_escape s 0 dest (i+1)
				# dest & [i] = '\''
				-> (dest,i+1)
			JSReal r
				-> copy_chars (toString r) dest i

			JSVar v
				-> copy_chars v dest i
			JSNull
				# dest & [i]='n',[i+1]='u',[i+2]='l',[i+3]='l'
				-> (dest,i+4)
			JSUndefined
				# dest & [i]='u',[i+1]='n',[i+2]='d',[i+3]='e',[i+4]='f',[i+5]='i',[i+6]='n',[i+7]='e',[i+8]='d'
				-> (dest,i+9)
			JSTypeOf v
				# dest & [i]='t',[i+1]='y',[i+2]='p',[i+3]='e',[i+4]='o',[i+5]='f',[i+6]=' '
				-> copy v dest (i+7)
			JSDelete v
				# dest & [i]='d',[i+1]='e',[i+2]='l',[i+3]='e',[i+4]='t',[i+5]='e',[i+6]=' '
				-> copy v dest (i+7)

			JSObject elems
				# dest & [i]='{'
				| size elems==0
					# dest & [i+1]='}'
					-> (dest,i+2)
				# (dest,i) = copy_elems elems 0 dest (i+1)
				# dest & [i]='}'
				-> (dest,i+1)
				with
					copy_elems :: !{#JSObjectElement} !Int !*{#Char} !Int -> (!.{#Char}, !Int)
					copy_elems elems k dest i
					# {key,val} = elems.[k]
					# (dest,i) = copy_chars key dest i
					# dest & [i]=':'
					# (dest,i) = copy val dest (i+1)
					| k+1>=size elems
						= (dest,i)
						= copy_elems elems (k+1) {dest & [i]=','} (i+1)
			JSArray elems
				# dest & [i]='['
				| size elems==0
					# dest & [i+1]=']'
					-> (dest,i+2)
				# (dest,i) = copy_with_commas elems 0 dest (i+1)
				# dest & [i]=']'
				-> (dest,i+1)
			JSCall fun args
				# (dest,i) = copy fun dest i
				# dest & [i]='('
				| size args==0
					# dest & [i+1]=')'
					-> (dest,i+2)
				# (dest,i) = copy_with_commas args 0 dest (i+1)
				# dest & [i]=')'
				-> (dest,i+1)
			JSNew cons args
				# dest & [i]='n',[i+1]='e',[i+2]='w',[i+3]=' '
				# (dest,i) = copy_chars cons dest (i+4)
				# dest & [i]='('
				| size args==0
					# dest & [i+1]=')'
					-> (dest,i+2)
				# (dest,i) = copy_with_commas args 0 dest (i+1)
				# dest & [i]=')'
				-> (dest,i+1)

			JSSel obj attr
				# (dest,i) = copy obj dest i
				# dest & [i]='['
				# (dest,i) = copy attr dest (i+1)
				# dest & [i]=']'
				-> (dest,i+1)
			JSSelPath obj path
				# (dest,i) = copy obj dest i
				# dest & [i]='.'
				-> copy_chars path dest (i+1)

			JSRef n
				# dest & [i]='A',[i+1]='B',[i+2]='C',[i+3]='.',[i+4]='j',[i+5]='s',[i+6]='['
				# (dest,i) = copy_chars (toString n) dest (i+7)
				# dest & [i]=']'
				-> (dest,i+1)
			JSCleanRef n
				# dest & [i]='A',[i+1]='B',[i+2]='C',[i+3]='.',[i+4]='a',[i+5]='p',[i+6]='('
				# (dest,i) = copy_chars (toString n) dest (i+7)
				# dest & [i]=')'
				-> (dest,i+1)
		where
			copy_chars :: !String !*{#Char} !Int -> (!.{#Char}, !Int)
			copy_chars src dest i = (copy` src (sz-1) dest (i+sz-1), i+sz)
			where
				sz = size src

				copy` :: !String !Int !*{#Char} !Int -> .{#Char}
				copy` _   -1 dest _  = dest
				copy` src si dest di = copy` src (si-1) {dest & [di]=src.[si]} (di-1)

			copy_and_escape :: !String !Int !*{#Char} !Int -> (!.{#Char}, !Int)
			copy_and_escape src si dest di
			| si >= size src = (dest,di)
			# c = src.[si]
			| c < '\x20'
				# c = toInt c
				# dest = {dest & [di]='\\', [di+1]='x', [di+2]=hex (c>>4), [di+3]=hex (c bitand 0x0f)}
				= copy_and_escape src (si+1) dest (di+4)
			| c == '\''
				# dest = {dest & [di]='\\', [di+1]='\''}
				= copy_and_escape src (si+1) dest (di+2)
			| otherwise
				# dest = {dest & [di]=c}
				= copy_and_escape src (si+1) dest (di+1)
			where
				hex i = "0123456789abcdef".[i]

			copy_with_commas :: !{!JSVal} !Int !*{#Char} !Int -> (!.{#Char}, !Int)
			copy_with_commas elems k dest i
			# (dest,i) = copy elems.[k] dest i
			| k+1>=size elems
				= (dest,i)
				= copy_with_commas elems (k+1) {dest & [i]=','} (i+1)

		len :: !JSVal !Int -> Int
		len v l = case v of
			JSInt i -> int_len i l
			JSBool b -> if b 4 5 + l
			JSString s -> escaped_size s (size s-1) (2+l)
			where
				escaped_size :: !String !Int !Int -> Int
				escaped_size s -1 n = n
				escaped_size s  i n
				| s.[i] < '\x20' = escaped_size s (i-1) (n+4)
				| s.[i] == '\''  = escaped_size s (i-1) (n+2)
				| otherwise      = escaped_size s (i-1) (n+1)
			JSReal r -> size (toString r) + l

			JSVar v -> size v + l
			JSNull -> 4+l
			JSUndefined -> 9+l
			JSTypeOf v -> len v (7+l)
			JSDelete v -> len v (7+l)

			JSObject elems
			| size elems==0
				-> 2+l
				-> count_elems (size elems-1) (l+(2*size elems)+1)
			where
				count_elems :: !Int !Int -> Int
				count_elems -1 l = l
				count_elems  i l
				# {key,val} = elems.[i]
				= count_elems (i-1) (len val (l+size key))
			JSArray elems
			| size elems==0
				-> 2+l
				-> count_array elems (size elems-1) (size elems+1+l)
			JSCall fun args
			| size args==0
				-> len fun (2+l)
				-> count_array args (size args-1) (len fun (size args+1+l))
			JSNew cons args
			| size args==0
				-> size cons+6+l
				-> count_array args (size args-1) (size cons+5+size args+l)

			JSSel obj attr -> len obj (len attr (l+2))
			JSSelPath obj path -> len obj (l+1+size path)

			JSRef i -> int_len i (8+l)
			JSCleanRef i -> int_len i (8+l)

			_ -> missing_case v
		where
			int_len :: !Int !Int -> Int
			int_len i l
			| i > 9 = int_len (i/10) (l+1)
			| i < 0 = int_len (0-i) (l+1)
			| otherwise = l+1

			count_array :: !{!JSVal} !Int !Int -> Int
			count_array elems -1 l = l
			count_array elems  i l = count_array elems (i-1) (len elems.[i] l)

		missing_case :: !JSVal -> .a
		missing_case _ = code {
			print "missing case in toString JSVal:\n"
			.d 1 0
			jsr _print_graph
			.o 0 0
			halt
		}

jsMakeCleanReference :: a !JSVal -> JSVal
jsMakeCleanReference x attach_to = share attach_to x

jsGetCleanReference :: !JSVal !*JSWorld -> *(!Maybe b, !*JSWorld)
jsGetCleanReference v w = case eval_js_with_return_value (toString v) of
	JSCleanRef i -> case fetch i of
		(val,True) -> (Just val, w)
	_            -> if (1==1) (Nothing, w) (abort_with_node v)
where
	fetch :: !Int -> (!a, !Bool)
	fetch _ = code {
		create
		instruction 5
		pop_b 1
		pushB TRUE
	}

jsTypeOf :: !JSVal -> JSVal
jsTypeOf v = JSTypeOf v

jsIsUndefined :: !JSVal -> Bool
jsIsUndefined v = v=:JSUndefined

jsIsNull :: !JSVal -> Bool
jsIsNull v = v=:JSNull

jsValToInt :: !JSVal -> Maybe Int
jsValToInt v = case v of
	JSInt i    -> Just i
	JSReal r   -> Just (toInt r)
	JSString s -> case toInt s of
		0 -> if (s=="0") (Just 0) Nothing
		i -> Just i
	_          -> Nothing

jsValToBool :: !JSVal -> Maybe Bool
jsValToBool v = case v of
	JSBool b   -> Just b
	JSInt i    -> Just (i<>0)
	JSReal r   -> Just (r<>0.0)
	JSString s -> case s of
		"true"  -> Just True
		"false" -> Just False
		_       -> Nothing
	_          -> Nothing

jsValToString :: !JSVal -> Maybe String
jsValToString v = case v of
	JSString s -> Just s
	JSInt i    -> Just (toString i)
	JSReal r   -> Just (toString r)
	JSBool b   -> Just (if b "true" "false")
	_          -> Nothing

jsValToReal :: !JSVal -> Maybe Real
jsValToReal v = case v of
	JSReal r   -> Just r
	JSInt i    -> Just (toReal i)
	JSString s -> Just (toReal s)
	_          -> Nothing

jsValToInt` :: !Int !JSVal -> Int
jsValToInt` i v = fromMaybe i (jsValToInt v)

jsValToBool` :: !Bool !JSVal -> Bool
jsValToBool` b v = fromMaybe b (jsValToBool v)

jsValToString` :: !String !JSVal -> String
jsValToString` s v = fromMaybe s (jsValToString v)

jsValToReal` :: !Real !JSVal -> Real
jsValToReal` r v = fromMaybe r (jsValToReal v)

jsValToList :: !JSVal !(JSVal -> Maybe a) !*JSWorld -> *(!Maybe [a], !*JSWorld)
jsValToList arr get w
# (len,w) = arr .# "length" .? w // get length before array is evaluated
# (arr,w) = arr .? w // copy array to Clean
= case jsValToInt len of
	Nothing  -> (Nothing,w)
	Just len -> get_elements arr [] (len-1) w
where
	get_elements arr xs -1 w = (Just xs,w)
	get_elements arr xs i w
	# (x,w) = arr .# i .? w
	= case get x of
		Nothing -> (Nothing,w)
		Just x  -> get_elements arr [x:xs] (i-1) w

jsValToList` :: !JSVal !(JSVal -> a) !*JSWorld -> *(![a], !*JSWorld)
jsValToList` arr get w
# (len,w) = arr .# "length" .? w // get length before array is evaluated
# (arr,w) = arr .? w // copy array to Clean
= get_elements arr [] (jsValToInt` 0 len - 1) w
where
	get_elements arr xs -1 w = (xs,w)
	get_elements arr xs i w
	# (x,w) = arr .# i .? w
	= get_elements arr [get x:xs] (i-1) w

gToJS{|Int|} i = JSInt i
gToJS{|Bool|} b = JSBool b
gToJS{|String|} s = JSString s
gToJS{|Real|} r = JSReal r
gToJS{|JSVal|} v = v
gToJS{|Maybe|} fx v = case v of
	Nothing -> JSNull
	Just x  -> fx x
gToJS{|[]|} fx xs = JSArray {fx x \\ x <- xs}
gToJS{|(,)|} fa fb (a,b) = JSArray {fa a,fb b}
gToJS{|(,,)|} fa fb fc (a,b,c) = JSArray {fa a,fb b,fc c}
gToJS{|(,,,)|} fa fb fc fd (a,b,c,d) = JSArray {fa a,fb b,fc c,fd d}
gToJS{|(,,,,)|} fa fb fc fd fe (a,b,c,d,e) = JSArray {fa a,fb b,fc c,fd d,fe e}
gToJS{|JSONNode|} n = case n of
	JSONNull -> JSNull
	JSONBool b -> JSBool b
	JSONInt i -> JSInt i
	JSONReal r -> JSReal r
	JSONString s -> JSString s
	JSONArray xs -> JSArray {toJS x \\ x <- xs}
	JSONObject xs -> JSObject {{key=k,val=toJS v} \\ (k,v) <- xs}
	_ -> abort "missing case in gToJS{|JSONNode|}"

gToJS{|PAIR|} fx fy (PAIR x y) = JSTempPair (fx x) (fy y)
gToJS{|FIELD of {gfd_name}|} fx (FIELD x) = JSTempField gfd_name (fx x)
gToJS{|RECORD|} fx (RECORD x) = JSObject {e \\ e <|- collect_elems (fx x)}
where
	collect_elems :: !JSVal -> [!JSObjectElement!]
	collect_elems (JSTempField k v) = [!{key=k,val=v}!]
	collect_elems (JSTempPair a b)  = collect_elems a ++| collect_elems b

instance .# Int
where
	.# arr i = case arr of
		JSArray xs
			| 0<=i && i<size xs -> xs.[i]
			| otherwise         -> JSUndefined
		arr -> JSSel arr (JSInt i)

instance .# String
where
	.# obj path
		| contains_dot (size path-1) path
			= JSSelPath obj path
			= JSSel obj (JSString path)
	where
		contains_dot -1 _ = False
		contains_dot i s = if (s.[i]=='.') True (contains_dot (i-1) s)

(.?) infixl 1 :: !JSVal !*JSWorld -> *(!JSVal, !*JSWorld)
(.?) js w
# (done,js) = try_local_computation js
| done
	= (js,w)
	= case eval_js_with_return_value (toString js) of
		JSUnused -> abort_with_node js
		result   -> (result, w)
where
	try_local_computation :: !JSVal -> (!Bool, !JSVal)
	try_local_computation v = case v of
		JSInt _      -> (True,v)
		JSBool _     -> (True,v)
		JSString _   -> (True,v)
		JSReal _     -> (True,v)

		JSNull       -> (True,v)
		JSUndefined  -> (True,v)

		JSSel (JSArray xs) (JSInt i)
			| 0<=i && i<size xs -> try_local_computation xs.[i]
			| otherwise         -> (True,JSUndefined)
		JSSel (JSArray xs) (JSString "length")
		             -> (True,JSInt (size xs))
		JSSelPath (JSArray xs) "length"
		             -> (True,JSInt (size xs))

		JSRef _      -> (True,v)
		JSCleanRef _ -> (True,v)

		_            -> (False,v)

(.=) infixl 1 :: !JSVal !b !*JSWorld -> *JSWorld | gToJS{|*|} b
(.=) sel v w
# v = toJS v
= case set_js (toString sel) (toString v) of
	True  -> w
	False -> abort_with_node (sel,v)

instance toJSArgs Int where toJSArgs i = {toJS i}
instance toJSArgs Bool where toJSArgs b = {toJS b}
instance toJSArgs String where toJSArgs s = {toJS s}
instance toJSArgs JSVal where toJSArgs v = {v}
instance toJSArgs (Maybe b) | gToJS{|*|} b
where
	toJSArgs v = case v of
		Just v  -> {toJS v}
		Nothing -> {JSNull}
instance toJSArgs () where toJSArgs _ = {}

instance toJSArgs (a,b) | gToJS{|*|} a & gToJS{|*|} b
where toJSArgs (a,b) = {toJS a, toJS b}

instance toJSArgs (a,b,c) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c
where toJSArgs (a,b,c) = {toJS a, toJS b, toJS c}

instance toJSArgs (a,b,c,d) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d
where toJSArgs (a,b,c,d) = {toJS a, toJS b, toJS c, toJS d}

instance toJSArgs (a,b,c,d,e) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e
where toJSArgs (a,b,c,d,e) = {toJS a, toJS b, toJS c, toJS d, toJS e}

instance toJSArgs (a,b,c,d,e,f) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e & gToJS{|*|} f
where toJSArgs (a,b,c,d,e,f) = {toJS a, toJS b, toJS c, toJS d, toJS e, toJS f}

(.$) infixl 2 :: !JSFun !b !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs b
(.$) f args w = case eval_js_with_return_value (toString call) of
	JSUnused -> abort_with_node call
	result   -> (result, w)
where
	call = JSCall f (toJSArgs args)

(.$!) infixl 2 :: !JSFun !b !*JSWorld -> *JSWorld | toJSArgs b
(.$!) f args w = case eval_js (toString call) of
	True  -> w
	False -> abort_with_node call
where
	call = JSCall f (toJSArgs args)

jsNew :: !String !a !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs a
jsNew cons args w = case eval_js_with_return_value (toString new) of
	JSUnused -> abort_with_node new
	result   -> (result, w)
where
	new = JSNew cons (toJSArgs args)

jsDelete :: !JSVal !*JSWorld -> *JSWorld
jsDelete v w = case eval_js (toString (JSDelete v)) of
	True  -> w
	False -> abort_with_node v

jsEmptyObject :: !*JSWorld -> *(!JSVal, !*JSWorld)
jsEmptyObject w = (eval_js_with_return_value "{}", w)

jsGlobal :: !String -> JSVal
jsGlobal s = JSVar s

jsWrapFun :: !({!JSVal} *JSWorld -> *JSWorld) !JSVal !*JSWorld -> *(!JSFun, !*JSWorld)
jsWrapFun f attach_to world = (share attach_to \(JSArray args) w -> f args w, world)

wrapInitUIFunction :: !(JSVal *JSWorld -> *JSWorld) -> {!JSVal} -> *JSWorld -> *JSWorld
wrapInitUIFunction f = init
where
	init :: !{!JSVal} !*JSWorld -> *JSWorld
	init args w
	# (needs_init,ok) = get_arg args.[1]
	| not ok = abort "internal error in wrapInitUIFunction\n"
	| needs_init==1 && init val = abort "internal error in wrapInitUIFunction\n"
	# (ref,ok) = get_arg args.[0]
	| not ok = abort "internal error in wrapInitUIFunction\n"
	= f (JSRef ref) w
	where
		// This function ensures that the client knows the addresses of some
		// of the constructors which it needs to know.
		init :: !{!JSVal} -> Bool
		init _ = code {
			instruction 7
			pop_a 1
			pushB FALSE
		}

		val :: {!JSVal}
		val =
			{ JSInt 0
			, JSBool True
			, JSString ""
			, JSReal 0.0
			, JSNull
			, JSUndefined
			, JSArray {}
			, JSRef 0
			, JSCleanRef 0
			}

		get_arg :: !a -> (!Int,!Bool)
		get_arg _ = code {
			pushB TRUE
			repl_r_args 0 1
		}

jsDeserializeGraph :: !String !JSVal !*JSWorld -> *(!.a, !*JSWorld)
jsDeserializeGraph s attach_to w = case attach_to of
	JSRef r -> (deserialize s r, w)
	_       -> abort "when deserializing a Clean value it must be linked to an iTasks component\n"
where
	deserialize :: !String !Int -> .a
	deserialize _ _ = code {
		instruction 6
		pop_b 1
	}

addCSSFromUrl :: !String !*JSWorld -> *JSWorld
addCSSFromUrl css w = case add_css css of
	True -> w
where
	add_css :: !String -> Bool
	add_css _ = code {
		instruction 10
		pop_a 1
		pushB TRUE
	}

addJSFromUrl :: !String !(Maybe JSFun) !*JSWorld -> *JSWorld
addJSFromUrl js mbCallback w = case add_js js callback of
	True  -> w
	False -> abort_with_node mbCallback
where
	callback = case mbCallback of
		Just cb -> toString cb
		Nothing -> ""

	add_js :: !String !String -> Bool
	add_js _ _ = code {
		instruction 11
		pop_a 2
		pushB TRUE
	}

jsTrace :: !a .b -> .b | toString a
jsTrace s x = case eval_js (toString (JSCall (JSVar "console.log") {JSString (toString s)})) of
	True  -> x
	False -> abort_with_node s // just in case it is a JSVal

set_js :: !String !String -> Bool
set_js var val = code {
	instruction 1
	pop_a 2
	pushB TRUE
}

eval_js :: !String -> Bool
eval_js s = code {
	instruction 2
	pop_a 1
	pushB TRUE
}

eval_js_with_return_value :: !String -> JSVal
eval_js_with_return_value s = code {
	instruction 3
	fill_a 0 1
	pop_a 1
}

share :: !JSVal !a -> JSVal
share attach_to x = case attach_to of
	JSRef r -> JSCleanRef (get_shared_value_index x r)
	_       -> abort "when sharing a value from Clean to JS it must be linked to an iTasks component\n"
where
	get_shared_value_index :: !a !Int -> Int
	get_shared_value_index _ _ = code {
		instruction 4
		pop_a 1
	}

// This function is meant to be called with a (node containing) JSVal(s) as its
// argument, and then ensures that a references to that value(s) remain
// reachable by the garbage collector. This is needed when a JSVal is converted
// to a string and then sent to JavaScript. If it contains a JSRef, the
// reference must not be garbage collected, but computing the string to send to
// JavaScript may trigger a garbage collection after the JSRef has been
// visited. This function, when used properly, makes sure that the JSRef stays
// in memory until after the call to JavaScript.
abort_with_node :: !a -> .b
abort_with_node _ = abort "abort_with_node\n"
