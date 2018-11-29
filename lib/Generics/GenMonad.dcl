definition module GenMonad

import StdGeneric, StdMaybe, StdList

class Monad m where
	ret :: a:a -> m:(m a:a), [m <= a]
 	(>>=) infixl 5 :: u:(m .a) v:(.a -> u:(m .b))  -> u:(m .b), [u <= v]
	
:: StMonad s a = { st_monad :: .(s -> *(a, s)) }
derive bimap StMonad
instance Monad Maybe, [], (StMonad .s)

generic gMapLM a b :: a:a -> m:(m b:b) | Monad m, [m <= b]
derive gMapLM c, PAIR, EITHER, CONS, FIELD, OBJECT
derive gMapLM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRM a b :: a:a -> m:(m b:b) | Monad m, [m <= b]
derive gMapRM c, PAIR, EITHER, CONS, FIELD, OBJECT
derive gMapRM [], Maybe, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

