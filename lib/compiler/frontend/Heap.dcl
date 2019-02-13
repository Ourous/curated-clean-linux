definition module Heap

import StdClass

:: Heap v = {heap::!.HeapN v}
:: .HeapN v
:: Ptr v = {pointer::!.(PtrN v)};
:: PtrN v = Ptr !v !(HeapN v);

newHeap		:: .Heap v

nilPtr		:: Ptr v

isNilPtr 	:: !(Ptr v) -> Bool

newPtr		:: !v !*(Heap v) -> (!.Ptr v,!.Heap v)

readPtr		:: !(Ptr v) !u:(Heap v) -> (!v,!u:Heap v)

writePtr	:: !(Ptr v) !v !*(Heap v) -> .Heap v

sreadPtr	:: !(Ptr v) !(Heap v) -> v

allocPtr :: Ptr v;

initPtr :: !(Ptr v) !v !*(Heap v) !*World -> (!.Heap v,!*World);

ptrToInt 	:: !(Ptr w) -> Int

(<:=) infixl 
(<:=) heap ptr_and_val :== writePtr ptr val heap 
where
	(ptr, val) = ptr_and_val

instance == (Ptr a)
