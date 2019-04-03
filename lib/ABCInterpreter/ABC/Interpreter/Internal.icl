implementation module ABC.Interpreter.Internal

import StdArray
import StdClass
import StdInt

import ABC.Interpreter
import ABC.Interpreter.Util

add_shared_node :: !Int !*{a} a -> *(!Int, !*{a}, !Int)
add_shared_node ptr nodes node
# (arraysize,nodes) = usize nodes
# (spot,nodes) = find_empty_spot ptr nodes
| spot == -1
	= (arraysize, {copy (arraysize-1) nodes (create_array_ (arraysize+100)) & [arraysize]=node}, arraysize+1)
| otherwise
	= (spot, {nodes & [spot]=node}, if (spot+1 >= arraysize) 0 (spot+1))
where
	copy :: !Int !*{a} !*{a} -> *{a}
	copy -1 _ to = to
	copy i fr to
	# (x,fr) = fr![i]
	# to & [i] = x
	= copy (i-1) fr to

	find_empty_spot :: !Int !*{a} -> (!Int, !*{a})
	find_empty_spot start nodes = code {
		push_a 0
		push_arraysize _ 1 0
		push_b 1
	:find_empty_spot_loop
		push_b 0
		push_a 0
		select _ 1 0
		eq_desc _Nil 0 0
		pop_a 1
		jmp_true find_empty_spot_found
		incI
		push_b 0
		push_b 2
		eqI
		jmp_true find_empty_spot_overflow
		push_b 0
		push_b 3
		eqI
		jmp_true find_empty_spot_full
		jmp find_empty_spot_loop
	:find_empty_spot_overflow
		eqI_b 0 2
		jmp_true find_empty_spot_full
		pop_b 1
		pushI 0
		jmp find_empty_spot_loop
	:find_empty_spot_full
		pop_b 1
		pushI -1
	:find_empty_spot_found
		updatepop_b 0 2
	}

interpret :: InterpretationEnvironment !InterpretedExpression -> .a
interpret ie fin = code {
	.d 2 0
	jsr _interpret_copy_node_asm
	.o 1 0
}

interpret_1 :: InterpretationEnvironment !InterpretedExpression b -> a
interpret_1 ie fin arg = code {
	jsr_eval 0
	pushI 0
	.d 3 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_2  :: InterpretationEnvironment !InterpretedExpression b b -> .a
interpret_2  _ _ _ _ = code {
	jsr_eval 0
	pushI 1
	.d 4 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_3  :: InterpretationEnvironment !InterpretedExpression b b b -> .a
interpret_3  _ _ _ _ _ = code {
	jsr_eval 0
	pushI 2
	.d 5 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_4  :: InterpretationEnvironment !InterpretedExpression b b b b -> .a
interpret_4  _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 3
	.d 6 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_5  :: InterpretationEnvironment !InterpretedExpression b b b b b -> .a
interpret_5  _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 4
	.d 6 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_6  :: InterpretationEnvironment !InterpretedExpression b b b b b b -> .a
interpret_6  _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 5
	.d 7 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_7  :: InterpretationEnvironment !InterpretedExpression b b b b b b b -> .a
interpret_7  _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 6
	.d 8 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_8  :: InterpretationEnvironment !InterpretedExpression b b b b b b b b -> .a
interpret_8  _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 7
	.d 9 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_9  :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b -> .a
interpret_9  _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 8
	.d 10 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_10 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b -> .a
interpret_10 _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 9
	.d 11 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_11 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b -> .a
interpret_11 _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 10
	.d 12 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_12 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b -> .a
interpret_12 _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 11
	.d 13 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_13 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b -> .a
interpret_13 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 12
	.d 14 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_14 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b -> .a
interpret_14 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 13
	.d 15 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_15 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b -> .a
interpret_15 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 14
	.d 16 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_16 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b -> .a
interpret_16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 15
	.d 17 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_17 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b -> .a
interpret_17 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 16
	.d 18 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_18 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b -> .a
interpret_18 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 17
	.d 19 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_19 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b -> .a
interpret_19 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 18
	.d 20 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_20 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b -> .a
interpret_20 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 19
	.d 21 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_21 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_21 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 20
	.d 22 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_22 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_22 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 21
	.d 23 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_23 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_23 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 22
	.d 24 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_24 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_24 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 23
	.d 25 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_25 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_25 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 24
	.d 26 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_26 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_26 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 25
	.d 27 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_27 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_27 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 26
	.d 28 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_28 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_28 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 27
	.d 29 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_29 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_29 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 28
	.d 30 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_30 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_30 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 29
	.d 31 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}

interpret_31 :: InterpretationEnvironment !InterpretedExpression b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b -> .a
interpret_31 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = code {
	jsr_eval 0
	pushI 30
	.d 32 1 i
	jsr _interpret_copy_node_asm_n
	.o 1 0
}
