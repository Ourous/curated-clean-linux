definition module iTasks.UI.JavaScript

/**
 * This module provides ways to interact with JavaScript. All these functions
 * are supposed to be used from within a function that is wrapped by
 * `wrapInitUIFunction` (or hence, `withClientSideInit` in `iTasks.UI.Editor`),
 * which ensures that they are run within the WebAssembly runtime in the
 * browser. It does not make sense to use these functions on the server.
 *
 * The argument to `wrapInitUIFunction` receives a reference to the JavaScript
 * iTasks component it is related to, which must be used to share Clean values
 * with JavaScript (where indicated in documentation below).
 *
 * The JavaScript interfacing with this module can be found in itasks-core.js
 * and abc-interpreter.js.
 */

import StdGeneric
from StdMaybe import :: Maybe
from StdOverloaded import class toString
from Text.GenJSON import :: JSONNode

:: *JSWorld

:: JSVal
:: JSFun :== JSVal
:: JSObj :== JSVal

generic gToJS a :: !a -> JSVal
derive gToJS Int, Bool, String, Real
derive gToJS JSVal, Maybe, [], (,), (,,), (,,,), (,,,,), JSONNode
derive gToJS PAIR, FIELD of {gfd_name}, RECORD
toJS x :== gToJS{|*|} x

/**
 * Store a Clean value in the JavaScript heap. The value must be associated to
 * an iTasks component (typically given by `wrapInitUIFunction`). When the
 * iTasks component is destroyed, the value may eventually be garbage
 * collected. The value can be retrieved using `jsGetCleanReference`.
 * @param Any Clean value.
 * @param A reference to an iTasks component.
 * @result A JavaScript reference to the Clean value.
 */
jsMakeCleanReference :: a !JSVal -> JSVal

/**
 * Retrieve a Clean value from the JavaScript heap. The value must have been
 * shared using `jsMakeCleanReference`.
 */
jsGetCleanReference :: !JSVal !*JSWorld -> *(!Maybe b, !*JSWorld)

jsTypeOf :: !JSVal -> JSVal

jsIsUndefined :: !JSVal -> Bool
jsIsNull :: !JSVal -> Bool

jsValToInt :: !JSVal -> Maybe Int
jsValToBool :: !JSVal -> Maybe Bool
jsValToString :: !JSVal -> Maybe String
jsValToReal :: !JSVal -> Maybe Real

jsValToInt` :: !Int !JSVal -> Int
jsValToBool` :: !Bool !JSVal -> Bool
jsValToString` :: !String !JSVal -> String
jsValToReal` :: !Real !JSVal -> Real

/**
 * Retrieve a JavaScript iterable as a Clean list.
 * @param The JavaScript iterable.
 * @param The function to retrieve elements, called on every element of the
 *   iterable.
 * @result `Nothing` if the value is not iterable (has no `length` property) or
 *   if any of the elements could not be retrieved; otherwise `Just` with a
 *   list of the retrieved values.
 */
jsValToList :: !JSVal !(JSVal -> Maybe a) !*JSWorld -> *(!Maybe [a], !*JSWorld)

/**
 * Retrieve a JavaScript iterable as a Clean list.
 * @param The JavaScript iterable.
 * @param The function to retrieve elements, called on every element of the
 *   iterable.
 * @result A list of the retrieved values. If the JavaScript value is not
 *   iterable (has no `length` property), the empty list is returned.
 */
jsValToList` :: !JSVal !(JSVal -> a) !*JSWorld -> *(![a], !*JSWorld)

/**
 * Access properties of a JavaScript value.
 */
class (.#) infixl 3 attr :: !JSVal !attr -> JSVal

instance .# String // object access; may contain dots
instance .# Int // array access

/**
 * Retrieve a JavaScript value. This can be useful if the argument contains
 * computations which need to be shared among further usages of the result:
 *
 * ```
 * // object is a JavaScript object with a property 'prop' which requires computation
 * # (result,world) = object .# "prop" .? world
 * // continue to use result, so that object.prop is only evaluated once
 * ```
 *
 * However, if the argument does not contain computations, one can use a simple
 * Clean let: `# result = object .# "prop"`.
 *
 * @param The value to retrieve.
 * @result The retrieved value.
 */
(.?) infixl 1 :: !JSVal !*JSWorld -> *(!JSVal, !*JSWorld)

/**
 * Set a JavaScript value to another value.
 * @param The value to set.
 * @param The new value.
 */
(.=) infixl 1 :: !JSVal !b !*JSWorld -> *JSWorld | gToJS{|*|} b

/**
 * This is an internal class representing types which can be sent as function
 * arguments to JavaScript:
 *
 * - `Int`, `Bool`, and `String` map to their JavaScript equivalents
 * - `JSVal` requires no conversion
 * - `Maybe a` uses the conversion for the underlying type for `Just`, otherwise `null`
 * - `()` relates to no arguments; tuples relates to lists of arguments
 */
class toJSArgs a :: !a -> {!JSVal}
instance toJSArgs Int, Bool, String, JSVal, (Maybe b) | gToJS{|*|} b, ()
instance toJSArgs (a,b) | gToJS{|*|} a & gToJS{|*|} b
instance toJSArgs (a,b,c) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c
instance toJSArgs (a,b,c,d) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d
instance toJSArgs (a,b,c,d,e) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e
instance toJSArgs (a,b,c,d,e,f) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e & gToJS{|*|} f

/**
 * Call a JavaScript function and return the result.
 */
(.$) infixl 2 :: !JSFun !b !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs b

/**
 * Call a JavaScript function and discard the result.
 */
(.$!) infixl 2 :: !JSFun !b !*JSWorld -> *JSWorld | toJSArgs b

/**
 * Use the JavaScript `new` keyword to create a new object.
 * @param The constructor name.
 * @param The constructor arguments.
 * @result The new object.
 */
jsNew :: !String !a !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs a

/**
 * Create an empty JavaScript object (`{}`).
 */
jsEmptyObject :: !*JSWorld -> *(!JSVal, !*JSWorld)

/**
 * Delete a JavaScript value with the `delete` keyword.
 */
jsDelete :: !JSVal !*JSWorld -> *JSWorld

/**
 * Lift a String to a JavaScript value using the global address space.
 */
jsGlobal :: !String -> JSVal

jsNull :== jsGlobal "null"
jsThis :== jsGlobal "this"
jsWindow :== jsGlobal "window"
jsDocument :== jsGlobal "document"

/**
 * Wrap a function for use in JavaScript. This allows it to be used as the
 * callback for events and the like. The function must be associated to an
 * iTasks component (typically given by `wrapInitUIFunction`). When the iTasks
 * component is destroyed, the function may eventually be garbage collected.
 * @param The function to wrap. When called, it receives the JavaScript
 *   arguments as an array in its first parameter.
 * @param The iTasks component to link the function to.
 * @result A reference to the shared function.
 */
jsWrapFun :: !({!JSVal} *JSWorld -> *JSWorld) !JSVal !*JSWorld -> *(!JSFun, !*JSWorld)

/**
 * Wrap a function receiving a reference to a JavaScript iTasks component to
 * one matching the calling convention for the JavaScript interface (i.e.,
 * receiving an array of JavaScript values) so that it can be called using the
 * `initUI` step from `itasks-core.js`. Internally, this also sets up part of
 * the WebAssembly backend. The first argument to the wrapped function is a
 * reference to an iTasks component which can be used in `jsWrapFun`,
 * `jsMakeCleanReference`, and `jsDeserializeGraph`.
 */
wrapInitUIFunction :: !(JSVal *JSWorld -> *JSWorld) -> {!JSVal} -> *JSWorld -> *JSWorld

/**
 * Deserialize a graph that was serialized using the tools in
 * `iTasks.Internal.Client.Serialization`. The graph must be associated to an
 * iTasks component (typically given by `wrapInitUIFunction`). When the iTasks
 * component is destroyed, the graph may eventually be garbage collected.
 * @param The string to deserialize.
 * @param The iTasks component to link the graph to.
 * @result The deserialized value.
 */
jsDeserializeGraph :: !String !JSVal !*JSWorld -> *(!.a, !*JSWorld)

/**
 * Load external CSS stylesheet by its URL.
 * @param The URL.
 */
addCSSFromUrl :: !String !*JSWorld -> *JSWorld

/**
 * Load an external JavaScript file by its URL.
 * @param The URL.
 * @param An optional callback function for when the script has loaded.
 */
addJSFromUrl :: !String !(Maybe JSFun) !*JSWorld -> *JSWorld

/**
 * A simple wrapper around JavaScript's `console.log`.
 * @param The value to log.
 * @param The value to return.
 */
jsTrace :: !a .b -> .b | toString a
