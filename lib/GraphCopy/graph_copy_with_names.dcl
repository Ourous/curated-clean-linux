definition module graph_copy_with_names;

import StdString,_SystemStrictLists;
from symbols_in_program import ::Symbol;

:: DescInfo = {di_prefix_arity_and_mod :: !Int, di_name :: !{#Char}};

copy_to_string_with_names :: a -> (!*{#Char},!*{#DescInfo},!*{#String});
copy_from_string_with_names :: !*{#Char} !*{#DescInfo} !*{#String} !{#Symbol} -> (.a,!Int);

// Internal functions (normally you should not use them directly):
replace_desc_numbers_by_descs :: !Int !*{#Char} !{#Int} !Int !Int -> *{#Char};
make_symbol_name :: !String !String !Int -> String;
