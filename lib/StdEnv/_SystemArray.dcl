definition module _SystemArray

class Array .a e where
	select				:: !.(a .e) !Int	-> .e
	uselect				:: !u:(a e) !Int	-> *(e, !u:(a e))
	size				:: !.(a .e)			-> Int
	usize				:: !u:(a .e)		-> *(!Int, !u:(a .e))
	update				:: !*(a .e) !Int .e -> *(a .e)	
	createArray			:: !Int e			-> *(a e)
	_createArray		:: !Int				-> *(a .e)
	replace				:: !*(a .e) !Int .e -> *(.e, !*(a .e))

instance Array {!} a where
	uselect :: !u:{! e} !Int -> *(!e, !u:{! e})
	update :: !*{! .e} !Int !.e -> *{! .e}
	createArray :: !Int !e -> *{! e}
	replace :: !*{! .e} !Int !.e -> *(!.e, !*{! .e})

instance Array {#} Int where
	uselect :: !u:{# Int} !Int -> *(!Int, !u:{# Int})
	update :: !*{# e:Int} !Int !e:Int -> *{# e:Int}
	createArray :: !Int !Int -> *{# Int}
	replace :: !*{# e:Int} !Int !e:Int -> *(!e:Int, !*{# e:Int})

instance Array {#} Char where
	uselect :: !u:{# Char} !Int -> *(!Char, !u:{# Char})
	update :: !*{# e:Char} !Int !e:Char -> *{# e:Char}
	createArray :: !Int !Char -> *{# Char}
	replace :: !*{# e:Char} !Int !e:Char -> *(!e:Char, !*{# e:Char})

instance Array {#} Real where
	uselect :: !u:{# Real} !Int -> *(!Real, !u:{# Real})
	update :: !*{# e:Real} !Int !e:Real -> *{# e:Real}
	createArray :: !Int !Real -> *{# Real}
	replace :: !*{# e:Real} !Int !e:Real -> *(!e:Real, !*{# e:Real})

instance Array {#} Bool where
	uselect :: !u:{# Bool} !Int -> *(!Bool, !u:{# Bool})
	update :: !*{# e:Bool} !Int !e:Bool -> *{# e:Bool}
	createArray :: !Int !Bool -> *{# Bool}
	replace :: !*{# e:Bool} !Int !e:Bool -> *(!e:Bool, !*{# e:Bool})

instance Array {#} {#.a} where
	uselect :: !u:{#{#.a}} !Int -> *(!{#.a},!u:{#{#.a}})
	update :: !*{#u:{#.a}} !Int !u:{#.a} -> *{#u:{#.a}}
	createArray :: !Int !{#.a} -> *{# {#.a}}
	replace :: !*{#u:{#.a}} !Int !u:{#.a} -> *(!u:{#.a},!*{#u:{#.a}})

instance Array {#} {!.a} where
	uselect :: !u:{#{!.a}} !Int -> *(!{!.a},!u:{#{!.a}})
	update :: !*{#u:{!.a}} !Int !u:{!.a} -> *{#u:{!.a}}
	createArray :: !Int !{!.a} -> *{# {!.a}}
	replace :: !*{#u:{!.a}} !Int !u:{!.a} -> *(!u:{!.a},!*{#u:{!.a}})

instance Array {#} {.a} where
	uselect :: !u:{#{.a}} !Int -> *(!{.a},!u:{#{.a}})
	update :: !*{#u:{.a}} !Int !u:{.a} -> *{#u:{.a}}
	createArray :: !Int !{.a} -> *{# {.a}}
	replace :: !*{#u:{.a}} !Int !u:{.a} -> *(!u:{.a},!*{#u:{.a}})

instance Array {#} a where
	uselect :: !u:{# e} !Int -> *(!e, !u:{# e})
	update :: !*{# .e} !Int !.e -> *{# .e}
	createArray :: !Int !e -> *{# e}
	replace :: !*{# .e} !Int !.e -> *(!.e, !*{# .e})

instance Array {} a
