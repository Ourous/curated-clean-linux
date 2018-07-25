implementation module Clean.PrettyPrint.Util

import StdEnv

instance zero CPPState
where
	zero = { cpp_indent = 0
	       , cpp_parens = False
	       }

instance print String where print _ s = s
instance print Int where print _ i = toString i
instance print [t] | print t where print st ts = join st " " ts
instance print CPPState where print _ st = {'\t' \\ _ <- [1..st.cpp_indent]}

instance print PrintList
where
	print _ PrintNil   = ""
	print st (a :+: b) = print st a +++ print st b

instance Join [u] | print u
where
	join _  _    []     = ""
	join st _    [e]    = print st e
	join st glue [e:es] = print st e +++ print st glue +++ join st glue es

	isNil [] = True
	isNil _  = False
