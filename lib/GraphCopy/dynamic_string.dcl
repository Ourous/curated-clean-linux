definition module dynamic_string

import StdDynamic

dynamic_to_string 	:: !Dynamic -> *{#Char}
string_to_dynamic 	:: *{#Char} -> .Dynamic

copy_to_string 		:: !.a -> *{#Char}
copy_from_string 	:: !*{#Char} -> (.a,!Int)
