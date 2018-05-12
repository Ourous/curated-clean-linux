definition module Debug.Performance

from System.Time import :: Clock

/**
* Measures the duration of the computation on a unique environment.
* The time is measure as the delta of clock ticks as defined in System.Time. 
* @param computation on the unique environment
* @return function that additionally measures the duration of the given computation
*/
measureTime :: !u:(*env -> *env) -> v:(*env -> (!Clock, !*env)), [v <= u]

/**
* Measures the duration of the computation on a unique environment, returning an additional result.
* The time is measure as the delta of clock ticks as defined in System.Time. 
* @param computation on the unique environment
* @return function that additionally measures the duration of the given computation
*/
measureTimeRes :: !u:(*env -> (!.a, !*env)) -> v:(*env -> (!Clock, !.a, !*env)), [v <= u]

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
