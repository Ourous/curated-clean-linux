implementation module dynamic_string

import StdDynamic
import StdEnv

import code from "copy_graph_to_string_interface."
import code from "copy_graph_to_string."
import code from "copy_string_to_graph_interface."
import code from "copy_string_to_graph."

copy_to_string :: !.a -> *{#Char}
copy_to_string g = code {
	.d 1 0
		jsr _copy_graph_to_string
	.o 1 0
}

copy_from_string :: !*{#Char} -> (.a,!Int)
copy_from_string g = code {
	.d 1 0
		jsr _copy_string_to_graph
	.o 1 0
		pushI 0
}

dynamic_to_string :: !Dynamic -> *{#Char}
dynamic_to_string d
	= copy_to_string d

string_to_dynamic :: *{#Char} -> .Dynamic
string_to_dynamic s
	# (d,_) = copy_from_string s
	= d


mk_unique :: !{#Char} -> *{#Char}
mk_unique s = {s` \\ s` <-: s}
