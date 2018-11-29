implementation module GenDefault

import StdGeneric

generic gDefault a ::  a

gDefault{|Int|}  			= 0
gDefault{|Real|}  			= 0.0
gDefault{|String|}  		= ""
gDefault{|UNIT|} 		 	= UNIT
gDefault{|EITHER|} dl dr  	= RIGHT  dr
gDefault{|EITHER|} dl dr   	= LEFT   dl
gDefault{|PAIR|}   dl dr  	= PAIR   dl dr
gDefault{|CONS|}   dc     	= CONS   dc
gDefault{|FIELD|}  df     	= FIELD  df
gDefault{|OBJECT|} do     	= OBJECT do
gDefault{|RECORD|} dr     	= RECORD dr

derive gDefault [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
