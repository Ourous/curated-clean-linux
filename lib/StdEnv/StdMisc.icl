implementation module StdMisc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1995 University of Nijmegen
// ****************************************************************************************

abort :: !{#Char} -> .a		// stop reduction and print argument
abort a = code inline {
	.d 1 0
	jsr print_string_
	.o 0 0
	halt
	}

undef:: .a
undef = abort "Run-time error! Program evaluated undefined value?!"
