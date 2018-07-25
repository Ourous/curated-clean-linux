implementation module Debug.Performance

import System.Environment, StdDebug, StdList, System.Time, StdString, System._Unsafe, Data.Func, Text
from StdFunc import seq, o

// stores the indentation level in global environment variable
NIDENT_ENV_VAR :== "CLEAN_PLATFORM_DEBUG_PERFORMANCE_INDENT_LEVEL"

measureTime :: !u:(*env -> *env) -> v:(*env -> (!Int, !Int, !*env)), [v <= u]
measureTime f = g
where
    g env
        # (process, wall, _, env) = measureTimeRes h env
        = (process, wall, env)

    h env
        # env = f env
        = ((), env)

measureTimeRes :: !u:(*env -> (!.a, !*env)) -> v:(*env -> (!Int, !Int, !.a, !*env)), [v <= u]
measureTimeRes f = g
where
    // unsafe operations used, because we don't have *World, but arbitrary env
    g env
        // compute & measure time
        # (Clock cBefore, env) = accUnsafe` clock env
		# (tBefore, env)       = accUnsafe` nsTime env
        # (x, env)             = f env
        # (Clock cAfter, env)  = accUnsafe` clock env
		# (tAfter, env)        = accUnsafe` nsTime env
		# wall                 = tAfter - tBefore
        = ((cAfter - cBefore) / (CLK_PER_SEC / 1000), wall.tv_sec * 1000 + wall.tv_nsec / 1000000, x, env)

printTime :: !String !u:(*env -> *env) -> v:(*env -> *env), [v <= u]
printTime str f = g
where
    g env
        # (_, env) = printTimeRes str h env
        = env

    h env
        # env = f env
        = ((), env)

printTimeRes :: !String !u:(*env -> (!.a, !*env)) -> v:(*env -> (!.a, !*env)), [v <= u]
printTimeRes str f = g
where
    // unsafe operations used, because we don't have *World, but arbitrary env
    g env
        // get indentation level
        # (mbNIdent, env) = accUnsafe` (getEnvironmentVariable NIDENT_ENV_VAR) env
        # nIdent = maybe 0 toInt mbNIdent
        // increase indentation level
        # env = appUnsafe (setEnvironmentVariable NIDENT_ENV_VAR (toString (inc nIdent))) env
        // print label
        # identStr = seq (repeatn nIdent ((+++) "    ")) ""
        # env = trace_n (identStr +++ str) env
        // compute & measure time
        # (process, wall, x, env) = measureTimeRes f env
        // print time
        # env = trace_n (concat [ identStr, toString process, "ms", " (", toString wall, "ms wall)"]) env
        // set indentation level back
        # env = appUnsafe (setEnvironmentVariable NIDENT_ENV_VAR (toString nIdent)) env
        = (x, env)

// used instead of accUnsafe to make sure that *env used by timing functions is evaluated
accUnsafe` :: !*(*World -> *(.a, !*World)) !*env -> (.a, !*env)
accUnsafe` f env = accUnsafe (g env)
where
    g env world
        # (res, world) = f world
        = ((res, env), world)
