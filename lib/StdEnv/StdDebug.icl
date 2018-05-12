implementation module StdDebug;

// ********************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ********************************************************

import StdClass,StdFile,StdMisc;

from StdString import instance toString	{#Char},instance toString Int;

non_strict_abort :: !{#Char} -> .a;
non_strict_abort a = code  {
	.d 1 0
		jsr print_string_
	.o 0 0
		halt
	}

impossible :: .a;
impossible
	= non_strict_abort "impossible";

// The following functions should only be used for debugging,
// because these functions have side effects.

trace :: !msg .a -> .a | toString msg;
trace message a
  | file_to_true (fwrites (toString message) stderr)
      = a ;
      = impossible;

trace_n :: !msg .a -> .a | toString msg;
trace_n message a
  | file_to_true (fwritec '\n' (fwrites (toString message) stderr))
      = a  ;
      = impossible;

trace_t :: !msg -> Bool | toString msg;
trace_t message
	= file_to_true (fwrites (toString message) stderr);

trace_tn :: !msg -> Bool | toString msg;
trace_tn message
	= file_to_true (fwritec '\n' (fwrites (toString message) stderr));

file_to_true :: !File -> Bool;
file_to_true file
	= code inline {
          pop_b 2
          pushB TRUE
	}
