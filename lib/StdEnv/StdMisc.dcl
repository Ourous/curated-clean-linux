system module StdMisc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

abort		:: !{#Char} -> .a				:== code { .d 1 0 ; jsr print_string_ ; .o 0 0 ; halt }
											// stop reduction and print argument
undef		:: .a							// fatal error, stop reduction.
