implementation module System._Unsafe

appUnsafe :: !(*World -> *World) !.a -> .a
appUnsafe f a
	| world_to_true (f newWorld) = a
	
accUnsafe :: !*(*World -> *(.a, !*World)) -> .a
accUnsafe f
	# (a, world) = f newWorld
	| world_to_true world = a

newWorld :: *World
newWorld
	= code inline {
		  fillI 65536 0 
	}

world_to_true :: !*World -> Bool;
world_to_true w = True
