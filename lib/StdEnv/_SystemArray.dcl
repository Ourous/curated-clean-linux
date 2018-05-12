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


instance Array {!} a

instance Array {#} Int
instance Array {#} Char
instance Array {#} Real
instance Array {#} Bool

instance Array {#} {#.a}
instance Array {#} {!.a}
instance Array {#} {.a}

instance Array {#} a

//instance Array {#} File

instance Array {} a
