implementation module sapldebug

import StdEnv,graph_to_sapl_string, StdDebug

sapldebug :: !a .b -> .b
sapldebug a b = trace_n ("DEBUG: " +++ graph_to_sapl_string a) b

