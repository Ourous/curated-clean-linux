implementation module _SystemArray

class Array .a e where
	select				:: !.(a .e) !Int	-> .e
	uselect				:: !u:(a e) !Int	-> *(e, !u:(a e))
	size				:: !.(a .e)			-> Int
	usize				:: !u:(a .e)		-> *(!Int, !u:(a .e))
	update				:: !*(a .e) !Int .e -> *(a .e)	
	createArray			:: !Int e			-> *(a e)
	_createArray		:: !Int				-> *(a .e)
	replace				:: !*(a .e) !Int .e -> *(.e, !*(a .e))

instance Array {#} Int where
	select arr index =
		code
		{
			select INT 0 1
		}
	uselect arr index =
		code
		{
			push_a 0
			select INT 0 1
		}
	size arr =
		code
		{
			push_arraysize INT 0 1
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize INT 0 1
		}
	update arr index el =
		code
		{	
			update INT 0 1
		}
	createArray size el =
		code
		{	
			create_array INT 0 1
		}		
	replace arr index el =
		code
		{	
			replace INT 0 1
		}
	_createArray size =
		code
		{	
			create_array_ INT 0 1
		}

instance Array {#} Char where
	select arr index =
		code
		{
			select CHAR 0 1
		}
	uselect arr index =
		code
		{
			push_a 0
			select CHAR 0 1
		}
	size arr =
		code
		{
			push_arraysize CHAR 0 1
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize CHAR 0 1
		}
	update arr index el =
		code
		{	
			update CHAR 0 1
		}
	createArray size el =
		code
		{	
			create_array CHAR 0 1
		}		
	replace arr index el =
		code
		{	
			replace CHAR 0 1
		}
	_createArray size =
		code
		{	
			create_array_ CHAR 0 1
		}

instance Array {#} Real where
	select arr index =
		code
		{
			select REAL 0 1
		}
	uselect arr index =
		code
		{
			push_a 0
			select REAL 0 1
		}
	size arr =
		code
		{
			push_arraysize REAL 0 1
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize REAL 0 1
		}
	update arr index el =
		code
		{	
			update REAL 0 1
		}
	createArray size el =
		code
		{	
			create_array REAL 0 1
		}		
	replace arr index el =
		code
		{	
			replace REAL 0 1
		}
	_createArray size =
		code
		{	
			create_array_ REAL 0 1
		}

instance Array {#} Bool where
	select arr index =
		code
		{
			select BOOL 0 1
		}
	uselect arr index =
		code
		{
			push_a 0
			select BOOL 0 1
		}
	size arr =
		code
		{
			push_arraysize BOOL 0 1
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize BOOL 0 1
		}
	update arr index el =
		code
		{	
			update BOOL 0 1
		}
	createArray size el =
		code
		{	
			create_array BOOL 0 1
		}		
	replace arr index el =
		code
		{	
			replace BOOL 0 1
		}
	_createArray size =
		code
		{	
			create_array_ BOOL 0 1
		}

instance Array {#} {#.a} where
	select arr index =
		code
		{
			select _ 1 0
		}
	uselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}
	size arr =
		code
		{
			push_arraysize _ 1 0
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize _ 1 0
		}
	update arr index el =
		code
		{	
			update _ 1 0
		}
	createArray size el =
		code
		{	
			create_array _ 1 0
		}		
	replace arr index el =
		code
		{	
			replace _ 1 0
		}
	_createArray size =
		code
		{	
			create_array_ _ 1 0
		}

instance Array {#} {!.a} where
	select arr index =
		code
		{
			select _ 1 0
		}
	uselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}
	size arr =
		code
		{
			push_arraysize _ 1 0
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize _ 1 0
		}
	update arr index el =
		code
		{	
			update _ 1 0
		}
	createArray size el =
		code
		{	
			create_array _ 1 0
		}		
	replace arr index el =
		code
		{	
			replace _ 1 0
		}
	_createArray size =
		code
		{	
			create_array_ _ 1 0
		}

instance Array {#} {.a} where
	select arr index =
		code
		{
			select _ 1 0
		}
	uselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}
	size arr =
		code
		{
			push_arraysize _ 1 0
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize _ 1 0
		}
	update arr index el =
		code
		{	
			update _ 1 0
		}
	createArray size el =
		code
		{	
			create_array _ 1 0
		}		
	replace arr index el =
		code
		{	
			replace _ 1 0
		}
	_createArray size =
		code
		{	
			create_array_ _ 1 0
		}

instance Array {#} a where
	select arr index =
		code
		{
			buildAC "StdArray:select ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	uselect arr index =
		code
		{
			buildAC "StdArray:uselect ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	size arr =
		code
		{
			buildAC "StdArray:size ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	usize arr =
		code
		{
			buildAC "StdArray:usize ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	update arr index el =
		code
		{
			buildAC "StdArray:update ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	createArray size el =
		code
		{
			buildAC "StdArray:createArray ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}		
	replace arr index el =
		code
		{
			buildAC "StdArray:replace ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}
	_createArray size =
		code
		{
			buildAC "StdArray:_createArray ({#} a) should not be called"
		.d 1 0
			jsr print_string_
		.o 0 0
			halt
		}

instance Array {!} a where
	select arr index =
		code
		{
			select _ 1 0
		}
	uselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}
	size arr =
		code
		{
			push_arraysize _ 1 0
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize _ 1 0
		}
	update arr index el =
		code
		{	
			update _ 1 0
		}
	createArray size el =
		code
		{	
			create_array _ 1 0
		}		
	replace arr index el =
		code
		{	
			replace _ 1 0
		}
	_createArray size =
		code
		{	
			create_array_ _ 1 0
		}

instance Array {} a where
	select arr index =
		code
		{
			select _ 1 0
			jsr_eval 0
		}
	uselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}
	size arr =
		code
		{
			push_arraysize _ 1 0
		}
	usize arr =
		code
		{
			push_a 0
			push_arraysize _ 1 0
		}
	update arr index el =
		code
		{	
			update _ 1 0
		}
	createArray size el =
		code
		{	
			create_array _ 1 0
		}		
	replace arr index el =
		code
		{	
			replace _ 1 0
		}
	_createArray size =
		code
		{	
			create_array_ _ 1 0
		}
