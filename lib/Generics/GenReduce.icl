implementation module GenReduce

import StdGeneric, _Array

// or crush
generic gReduce t :: (a a -> a) a  t -> a
gReduce{|c|} op e x 					= e
gReduce{|PAIR|} fx fy op e (PAIR x y) 	= op (fx op e x) (fy op e y)
gReduce{|EITHER|} fl fr op e (LEFT x) 	= fl op e x
gReduce{|EITHER|} fl fr op e (RIGHT x) 	= fr op e x
gReduce{|CONS|} f op e (CONS x) 		= f op e x 
gReduce{|FIELD|} f op e (FIELD x) 		= f op e x
gReduce{|OBJECT|} f op e (OBJECT x) 	= f op e x
gReduce{|{}|} f op e x					= reduceArray f op e x
gReduce{|{!}|} f op e x					= reduceArray f op e x
derive gReduce [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gReduceRSt t :: .t .st -> .st
gReduceRSt{|c|} x st 					= st
gReduceRSt{|PAIR|} fx fy (PAIR x y) st 	= fx x (fy y st)
gReduceRSt{|EITHER|} fl fr x st 		= reduceEITHER fl fr x st
gReduceRSt{|CONS|} f (CONS x) st 		= f x st
gReduceRSt{|FIELD|} f (FIELD x) st 		= f x st
gReduceRSt{|OBJECT|} f (OBJECT x) st 	= f x st
gReduceRSt{|{}|} f xs st				= reduceArrayRSt f xs st	
gReduceRSt{|{!}|} f xs st				= reduceArrayRSt f xs st	
derive gReduceRSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gReduceLSt t :: .t .st -> .st
gReduceLSt{|c|} x st 						= st
gReduceLSt{|PAIR|} fx fy (PAIR x y) st 	= fy y (fx x st)
gReduceLSt{|EITHER|} fl fr x st 		= reduceEITHER fl fr x st
gReduceLSt{|CONS|} f (CONS x) st 		= f x st
gReduceLSt{|FIELD|} f (FIELD x) st 		= f x st
gReduceLSt{|OBJECT|} f (OBJECT x) st 	= f x st
gReduceLSt{|{}|} f xs st				= reduceArrayLSt f xs st	
gReduceLSt{|{!}|} f xs st				= reduceArrayLSt f xs st	
derive gReduceLSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

reduceEITHER fl fr (LEFT x) st 			= fl x st
reduceEITHER fl fr (RIGHT x) st 		= fr x st

		  