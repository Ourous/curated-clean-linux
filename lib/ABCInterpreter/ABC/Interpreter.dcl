definition module ABC.Interpreter

/**
 * This module defines types and functions to use an interpreter for the ABC
 * language for cross-platform (de)serialization of lazy expressions.
 *
 * Currently, functionality is limited to 64-bit platforms, because ABC code
 * generated for 32-bit platforms is different.
 *
 * You need to set certain project options to use this library.
 *
 * When using cpm, set the following in your project file:
 *  - ByteCode: path for the main bytecode file (e.g. {Project}*app.bc)
 *  - CodeGen/GenerateByteCode: True
 *  - CodeGen/OptimiseABC: True (unless you suspect a bug in the ABC optimiser)
 *  - Link/StripByteCode: False (because we need symbols in the bytecode)
 *  - Link/GenerateSymbolTable: True (because we need symbols in the executable)
 *
 * In the Clean IDE, you can set these options in two panes:
 *  - Project options > Linker (GenerateSymbolTable)
 *  - Project options > Bytecode (all other options)
 *
 * There is currently no support for bytecode generation in clm.
 */

from StdMaybe import :: Maybe

/**
 * This type describes settings used by the interpreter to deserialize
 * expressions. You may also use {{`defaultDeserializationSettings`}}.
 */
:: DeserializationSettings =
	{ heap_size  :: !Int //* Heap size for the interpreter, in bytes (default: 2M)
	, stack_size :: !Int //* Stack size for the interpreter, in bytes (default: 1M in total; 500k for A and 500k for BC stack)
	}

defaultDeserializationSettings :: DeserializationSettings

/**
 * A serialized expression.
 * Use {{`graphToString`}} and {{`graphFromString`}} to convert this to and
 * from strings, or use {{`graphToFile`}} and {{`graphFromFile`}} to read/write
 * this from/to files.
 */
:: *SerializedGraph

/**
 * This is an internal type.
 */
:: InterpretedExpression

/**
 * This is an internal type.
 */
:: *InterpretationEnvironment

/**
 * Serialize an expression for interpretation.
 *
 * @param The value to serialize.
 * @param The path to the executable's bytecode (set by the `ByteCode` option in the project file).
 * @result The result may be `Nothing` if the bytecode could not be parsed.
 */
serialize :: a !String !*World -> *(!Maybe SerializedGraph, !*World)

/**
 * Deserialize an expression using the ABC interpreter.
 * This version copies nodes as soon as they are in head normal form.
 * Therefore, the result is only `Nothing` when pre-interpretation validation
 * has failed (e.g., when the bytecode could not be parsed). If an
 * interpretation error occurs this will most likely crash the host program.
 * For more safety, see {{`deserialize_strict`}}.
 *
 * @param Settings for the interpreter.
 * @param The graph to deserialize. Should be obtained using {{`serialize`}}.
 * @param The path to the current executable (needed to resolve symbols when copying the result).
 * @result The result may be `Nothing` when pre-interpretation validation has failed.
 */
deserialize :: !DeserializationSettings !SerializedGraph !String !*World -> *(!Maybe a, !*World)

/**
 * This type represents several different run-time errors that may occur when
 * deserializing an expression in strict mode (see {{`deserialize_strict`}}).
 */
:: DeserializedValue a
	= DV_ParseError
		//* The bytecode or serialized graph could not be parsed.

	| DV_HeapFull
		//* The interpreter had not enough heap to evaluate the expression.
	| DV_StackOverflow
		//* The interpreter had not enough stack to evaluate the expression.
		//* NB: on Windows, not all stack overflows can be caught.
	| DV_Halt
		//* The ABC instruction `halt` was encountered.
	| DV_IllegalInstruction
		//* A forbidden (ccall, etc.) or unknown ABC instruction was encountered.
	| DV_HostHeapFull
		//* The heap of the host application has not enough space to copy the result.

	| DV_Ok !a
		//* Deserialization succeeded.

/**
 * Deserialize an expression using the ABC interpreter.
 * This version evaluates the node to a normal form (unlike {{`deserialize`}},
 * which copies the result as soon as it is in head normal form). This allows
 * the interpreter to catch run-time errors (signaled in the
 * {{`DeserializedValue`}} type), so you can use this function for sandboxing.
 *
 * @param Settings for the interpreter.
 * @param The graph to deserialize. Should be obtained using {{`serialize`}}.
 * @param The path to the current executable (needed to resolve symbols when copying the result).
 */
deserialize_strict :: !DeserializationSettings !SerializedGraph !String !*World -> *(!DeserializedValue a, !*World)

/**
 * Deserialize the `Start` rule of a Clean application in bytecode format.
 * This is essentially the same as {{`deserialize`}}, but it always interprets
 * the start rule instead of a custom serialized graph.
 *
 * @param Settings for the interpreter.
 * @param The path to the bytecode file to run (set by the `ByteCode` option in the project file).
 * @param The path to the current executable (needed to resolve symbols when copying the result).
 * @result The result may be `Nothing` when the bytecode cannot be parsed.
 */
get_start_rule_as_expression :: !DeserializationSettings !String !String !*World -> *(Maybe a, !*World)

graph_to_string :: !*SerializedGraph -> *(!.String, !*SerializedGraph)
graph_from_string :: !String -> Maybe *SerializedGraph

graph_to_file :: !*SerializedGraph !*File -> *(!*SerializedGraph, !*File)
graph_from_file :: !*File -> *(!Maybe *SerializedGraph, !*File)

/**
 * This type holds a state that can be used to serialize expressions for
 * deserialization in a prelinked interpreter (one where code and data
 * addresses are fixed). This is the case for the WebAssembly interpreter.
 * The environment can be initialized with {{`prepare_prelinked_interpretation`}}
 * and used with {{`serialize_for_prelinked_interpretation`}}.
 */
:: PrelinkedInterpretationEnvironment

/**
 * See {{`PrelinkedInterpretationEnvironment`}} for documentation.
 *
 * @param The path to the executable's bytecode (set by the `ByteCode` option in the project file).
 * @result The result may be `Nothing` if the bytecode could not be parsed.
 */
prepare_prelinked_interpretation :: !String !*World -> *(!Maybe PrelinkedInterpretationEnvironment, !*World)

/**
 * See {{`PrelinkedInterpretationEnvironment`}} for documentation.
 *
 * @param The value to serialize.
 * @param The environment.
 */
serialize_for_prelinked_interpretation :: a !PrelinkedInterpretationEnvironment -> String
