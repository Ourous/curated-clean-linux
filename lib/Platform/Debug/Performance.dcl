definition module Debug.Performance

from System.Time import :: Clock, :: Timespec

/**
* Measures the duration of the computation on a unique environment.
* The time is measured in ms. Both the time spent in the process and the wall time is measured.
* The latter is useful in case the process spends a lot of time waiting for IO (e.g. HDD accesses).
* @param computation on the unique environment
* @return function that additionally measures the duration of the given computation (second result integer is wall time)
*/
measureTime :: !u:(*env -> *env) -> v:(*env -> (!Int, !Int, !*env)), [v <= u]

/**
* Measures the duration of the computation on a unique environment, returning an additional result.
* The time is measured in ms. Both the time spent in the process and the wall time is measured.
* The latter is useful in case the process spends a lot of time waiting for IO (e.g. HDD accesses).
* @param computation on the unique environment
* @return function that additionally measures the duration of the given computation (second result integer is wall time)
*/
measureTimeRes :: !u:(*env -> (!.a, !*env)) -> v:(*env -> (!Int, !Int, !.a, !*env)), [v <= u]

/**
* Prints the duration of the computation on a unique environment.
* Before the computation, a given label is printed, afterwards the time is printed (in ms).
* If this function is used in a nested way, indentations are added automatically.
* @param the label to print
* @param computation on the unique environment
* @return function that additionally prints the duration of the given computation
*/
printTime :: !String !u:(*env -> *env) -> v:(*env -> *env), [v <= u]

/**
* Prints the duration of the computation on a unique environment, returning an additional result.
* Before the computation, a given label is printed, afterwards the time is printed (in ms).
* If this function is used in a nested way, indentations are added automatically.
* @param the label to print
* @param computation on the unique environment
* @return function that additionally prints the duration of the given computation
*/
printTimeRes :: !String !u:(*env -> (!.a, !*env)) -> v:(*env -> (!.a, !*env)), [v <= u]
