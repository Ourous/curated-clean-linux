definition module System._Finalized

/**
 * This module provides support for 'finalized' values. These are values that
 * have a finalizer attached to them: a C function pointer and an integer
 * argument for that function. When the value is discarded by the garbage
 * collector, the C function will be called with that argument. Because garbage
 * collection runs periodically, this may not be directly after all references
 * to the value are lost (and may not happen at all, depending on the heap
 * usage of the application).
 *
 * An example of C functions defining a finalizer is given below:
 *
 * ```c
 * void finalizer(int arg) {
 *    ...
 * }
 *
 * void (*finalizerPtr())(int) {
 *    return &finalizer;
 * }
 * ```
 *
 * `finalizer` is the finalizer itself, while `finalizerPtr` provides the
 * pointer to the finalizer. In Clean you only need the latter function:
 *
 * ```clean
 * finalizerPtr :: Pointer
 * finalizerPtr = code {
 *     ccall finalizerPtr ":p"
 * }
 * ```
 *
 * The Clean expression `finalize someVal finalizerPtr someInt` provides access
 * to the value `someVal` and calls the C function `finalizer` with argument
 * `someInt` if no reference to the finalizer is left.
 *
 */

from System._Pointer import :: Pointer

:: Finalized a = Finalized a !Finalizer

:: Finalizer  = {finalizer_implementation :: !FinalizerT}
:: FinalizerT = DummyFinalizer !Int !Int !Int

/**
 * Attach a finalizer to a value.
 *
 * @param The value
 * @param A pointer to the finalizer C function
 * @param An integer argument for the C function
 * @result The value with a finalizer attached
 */
finalize :: a !Pointer !Int -> Finalized a

/**
 * Perfoms an operation on a finalized value.
 * All operations on a finalized value must use this function in order for the
 * system to work (otherwise, a reference to the value may still exist while
 * all references to the {{`Finalizer`}} are lost).
 *
 * @param The operation to perform
 * @param The finalized value
 * @result The operation's result
 */
withFinalizedValue :: !(a -> b) !(Finalized a) -> (!b, !Finalized a)

/**
 * In the special case that one wants to finalize an integer, this function and
 * the corresponding {{`withFinalizedInt`}} use less memory than {{`finalize`}}
 * and {{`withFinalizedValue`}}.
 * The C function will be called with the integer itself as its argument.
 *
 * @param The integer
 * @param A pointer to the finalizer C function
 * @result The finalized integer
 */
finalizeInt :: !Int !Pointer -> Finalizer

/**
 * Specialised case of {{`withFinalizedValue`}} for use with finalized integers
 * (see {{`finalizeInt`}}).
 *
 * @param The operation to perform
 * @param The finalized integer
 * @result The operation's result
 */
withFinalizedInt :: !(Int -> a) !Finalizer -> (!a, !Finalizer)
