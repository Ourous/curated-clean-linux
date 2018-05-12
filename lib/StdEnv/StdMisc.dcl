system module StdMisc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

abort		:: !{#Char} -> .a				// stop reduction and print argument
undef		:: .a							// fatal error, stop reduction.
