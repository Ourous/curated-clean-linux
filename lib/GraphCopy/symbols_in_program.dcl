definition module symbols_in_program;

from StdFile import ::Files;
import _SystemStrictLists;

:: Symbol = { symbol_name :: !String, symbol_value :: !Int};

read_symbols :: !{#Char} !*Files -> (!{#Symbol},!*Files);

get_symbol_value :: !{#Char} !{#Symbol} -> Int;

