implementation module compare_constructor;

equal_constructor :: !a !a ->Bool;
equal_constructor _ _ = code {
	.inline equal_constructor
		pushD_a 1
		pushD_a 0
		pop_a 2
		eqI
	.end
};

less_constructor :: !a !a ->Bool;
less_constructor _ _ = code {
	.inline less_constructor
		pushD_a 1
		pushD_a 0
		pop_a 2
		ltI
	.end
};

greater_constructor :: !a !a ->Bool;
greater_constructor _ _ = code {
	.inline greater_constructor
		pushD_a 1
		pushD_a 0
		pop_a 2
		gtI
	.end
};





