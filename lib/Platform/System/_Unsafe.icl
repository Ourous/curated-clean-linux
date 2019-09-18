implementation module System._Unsafe

import StdMisc

appUnsafe :: !(*World -> *World) !.a -> .a
appUnsafe f a
	| world_to_true (f newWorld) = a
	| otherwise                  = abort "error in appUnsafe\n"
	
accUnsafe :: !*(*World -> *(.a, *World)) -> .a
accUnsafe f
	# (a, world) = f newWorld
	| world_to_true world = a
	| otherwise                  = abort "error in accUnsafe\n"

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

world_to_true :: !*World -> Bool;
world_to_true w = True

unsafeCoerce :: !.a -> .b
unsafeCoerce a = code {
	no_op
}
