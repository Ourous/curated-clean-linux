definition module Data.Error

from Data.Functor import class Functor
from Control.Monad import class Monad
from Control.Applicative import class pure, class <*>, class Applicative
from Data.Maybe import :: Maybe
from Data.GenEq import generic gEq

/**
 * A type representing something that may have failed.
 * @var The error type.
 * @var The value type.
 */
:: MaybeError a b = Error a | Ok b 

/**
 * Like {{`MaybeError`}} but with {{`String`}} as error type.
 * @var The value type.
 */
:: MaybeErrorString a :== MaybeError String a

instance Functor (MaybeError a)
instance pure (MaybeError a)
instance <*> (MaybeError a)
instance Monad (MaybeError a)

derive gEq MaybeError

/**
 * Return True when the argument is an Ok value and return False otherwise.
 */
isOk			:: !(MaybeError a b) -> Bool
/**
 * Return True when the argument is an Error value and return False otherwise.
 */
isError			:: !(MaybeError a b) -> Bool

/**
 * Return the contents of an Ok value and abort at run-time otherwise.
 */
fromOk			:: !(MaybeError .a .b) -> .b

/**
 * Return the contents of an Error value and abort at run-time otherwise.
 */
fromError		:: !(MaybeError .a .b) -> .a

/**
 * Lifts a (MaybeError a b) to another MaybeError
 * @precondition: isError x == True
 */
liftError 		:: !(MaybeError .a .b) -> (MaybeError .a .c)

/**
 * Converts a Maybe value into a MaybeError value.
 *
 * @param The error used if the input is Nothing
 * @param The Maybe value to be converted
 * @return The resulting MaybeError value
 */
mb2error		:: !e !(Maybe a) -> MaybeError e a

/**
 * Returns st on Error, state continuation on OK
 */
okSt :: *st (.a *st -> *st) !(MaybeError .e .a) -> *st

/**
* Converts a MaybeError value into a Maybe value.
* 
* @param The MaybeError value
* @return The converted value. Ok maps to Just and Error maps to Nothing.
*/
error2mb   			:: !(MaybeError e a) -> Maybe a

/**
 * Sequences an operation on a MaybeError value.
 * If the input is already an Error the operation is not performed.
 *
 * @param The input
 * @param The operation on the value (performed if input is Ok)
 * @return The error of the input or the result of the operation
 */
seqErrors :: !(MaybeError e a) (a -> MaybeError e b) -> MaybeError e b

/**
 * Combines two MaybeError values.
 * If one of the input is an Error, this Error is given as result (If both are, the first is given).
 *
 * @param The first input
 * @param The second input
 * @param A combination function for the inputs if they are Ok
 * @return The error of one of the inputs or the result of the combination
 */
combineErrors :: !(MaybeError e a) (MaybeError e b) (a b -> MaybeError e c) -> MaybeError e c

/**
 * Sequences two operations on a state, yielding MaybeError values.
 * If the first operation already yields an error, the second is not performed.
 *
 * @param The first operation
 * @param The second operation, getting the result of the first as input
 * @return The Error of the first or the second operation
 */
seqErrorsSt :: !(.st -> (MaybeError e a,!.st)) (a .st -> u:(!MaybeError e b, !.st)) !.st -> v:(MaybeError e b, !.st), [u <= v]	

/**
 * Combines two MaybeError values, resulting from two operations on a state.
 * If one of the operations yields an Error, this Error is given as result (If both are, the first is given).
 *
 * @param The first operation
 * @param The second operation
 * @param A combination function for the inputs if they are Ok
 * @return The error of one of the operations or the result of the combination
 */
combineErrorsSt :: !(.st -> (!MaybeError e a, !.st)) (.st -> (!MaybeError e b, !.st)) (a b -> MaybeError e c) !.st -> (!MaybeError e c, !.st)
