implementation module unsafe_heap;

import StdOverloaded,StdMisc;

:: Heap v = {heap::!.(HeapN v)};
:: HeapN v = Heap !Int;
:: Ptr v = {pointer::!.(PtrN v)};
:: PtrN v = Ptr !v;

newHeap :: .Heap v;
newHeap = {heap=Heap 0};

newPtr :: !v !*(Heap v) -> (!.Ptr v,!.Heap v);
newPtr v h = code {
	build_r e_unsafe_heap_kPtr 1 0 0 0
	update_a 0 1
	pop_a 1
};

nilPtr :: Ptr v;
nilPtr =: make_nilPtr;

make_nilPtr :: Ptr v;
make_nilPtr = code {
	build _Nil 0 _hnf
	build_r e_unsafe_heap_kPtr 1 0 0 0
	update_a 0 1
	pop_a 1
};

isNilPtr :: !(Ptr v) -> Bool;
isNilPtr p = code {
	repl_args 1 1
	eq_desc _Nil 0 0
	pop_a 1
};

allocPtr :: Ptr v;
allocPtr = code {
	build _Cons 0 _hnf
	build_r e_unsafe_heap_kPtr 1 0 0 0
	update_a 0 1
	pop_a 1
};

initPtr :: !(Ptr v) !v !*(Heap v) !*World -> (!.Heap v,!*World);
initPtr p v h w
 = code {
	push_args 0 1 1
	eq_desc _Cons 0 0
	pop_a 1
	jmp_false init_pointer_error
	push_a 2
	fill1_r e_unsafe_heap_kPtr 1 0 1 01
.keep 0 2
	pop_a 2
.d 2 0
	rtn
:init_pointer_error
	pop_a 3
	print "initPtr: Pointer already initialized"
	halt
};

readPtr :: !(Ptr v) !u:(Heap v) -> (!v,!u:Heap v);
readPtr p h = code {
	repl_r_args_a 1 0 1 1
 };

sreadPtr :: !(Ptr v) !(Heap v) -> v;
sreadPtr p h = code {
	repl_r_args_a 1 0 1 1
	update_a 0 1
	pop_a 1
};

writePtr :: !(Ptr v) !v !*(Heap v) -> .Heap v;
writePtr p v h
 = code {
	push_a 1
	fill1_r e_unsafe_heap_kPtr 1 0 1 01
.keep 0 2
	pop_a 2 
};

(<:=) infixl;
(<:=) heap ptr_and_val :== writePtr ptr val heap ;
{
	(ptr, val) = ptr_and_val;
}

ptrToInt :: !(Ptr v) -> Int;
ptrToInt p
	| isNilPtr p
		= 0;
		= ptrToInt2 p;

ptrToInt2 :: !(Ptr v) -> Int;
ptrToInt2 p = code {
	push_a_b 0
	pop_a 1
	build _Nil 0 _hnf
	push_a_b 0
	pop_a 1
	push_b 1
	eqI
	jmp_false not_nil
	pop_b 1
	pushI 0
.d 0 1 b
	rtn
:not_nil
.d 0 1 b
	rtn	
};

instance == (Ptr a)
where
{	(==) p1 p2 = code {
	push_a_b 1
	push_a_b 0
	pop_a 2
	eqI
	}
};
