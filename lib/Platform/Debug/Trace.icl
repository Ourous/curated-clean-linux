implementation module Debug.Trace

trace_stdout :: !.a -> .a
trace_stdout _ = code {
	push_a 0
	.d 1 0
	jsr _print_graph
	.o 0 0
}
