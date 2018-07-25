	.file	"copy_graph_to_string.c"
	.text
	.type	copy, @function
copy:
.LFB17:

	testq	%rdx, %rdx
	jle	.L1
	movl	$0, %eax
.L3:
	movq	(%rsi,%rax,8), %rcx
	movq	%rcx, (%rdi,%rax,8)
	addq	$1, %rax
	cmpq	%rax, %rdx
	jne	.L3
.L1:
	rep ret

.LFE17:
	.size	copy, .-copy
	.globl	is_using_desc_relative_to_array
	.type	is_using_desc_relative_to_array, @function
is_using_desc_relative_to_array:
.LFB15:

	movl	$0, %eax
	ret

.LFE15:
	.size	is_using_desc_relative_to_array, .-is_using_desc_relative_to_array
	.globl	size_element_descriptor_currying
	.type	size_element_descriptor_currying, @function
size_element_descriptor_currying:
.LFB16:

	movl	$8, %eax
	ret

.LFE16:
	.size	size_element_descriptor_currying, .-size_element_descriptor_currying
	.globl	copy_graph_to_string
	.type	copy_graph_to_string, @function
copy_graph_to_string:
.LFB18:

	pushq	%r15


	pushq	%r14


	pushq	%r13


	pushq	%r12


	pushq	%rbp


	pushq	%rbx


	subq	$72, %rsp

	movq	%rsi, %rax
	movq	%rsi, 16(%rsp)
	movq	%rdx, (%rsp)
	leaq	16(%rsi), %rbp
	cmpq	%rbp, %rdx
	jb	.L64
	movq	%rdi, %r14
	movq	$__STRING__+2, (%rax)
	movq	$0, 8(%rax)
	movq	%rdx, %r15
	movq	%rdx, %r13
.L9:
	movq	(%r14), %r12
	cmpq	%rbp, %r15
	jbe	.L65
	testb	$1, %r12b
	jne	.L10
	leaq	1(%rbp), %rax
	movq	%rax, (%r14)
	leaq	8(%rbp), %rdi
	movq	%r12, 0(%rbp)
	testb	$2, %r12b
	je	.L11
	movzwl	-2(%r12), %eax
	testq	%rax, %rax
	jne	.L12
	cmpq	$dINT+2, %r12
	je	.L13
	cmpq	$CHAR+2, %r12
	je	.L13
	cmpq	$BOOL+2, %r12
	je	.L13
	cmpq	$REAL+2, %r12
	jne	.L14
.L13:
	cmpq	%rdi, %r15
	jbe	.L66
	movq	8(%r14), %rax
	movq	%rax, 8(%rbp)
	leaq	16(%rbp), %rbp
	jmp	.L15
.L14:
	movl	$__STRING__+2, %eax
	cmpq	%r12, %rax
	jne	.L16
	movq	8(%r14), %rax
	leaq	16(%r14), %rsi
	leaq	7(%rax), %rdx
	shrq	$3, %rdx
	leaq	0(,%rdx,8), %rbx
	addq	%rbx, %rdi
	cmpq	%rdi, %r15
	jbe	.L67
	leaq	16(%rbp), %r12
	movq	%rax, 8(%rbp)
	movq	%r12, %rdi
	call	copy
	leaq	(%r12,%rbx), %rbp
	jmp	.L15
.L16:
	cmpq	$__ARRAY__+2, %r12
	jne	.L68
	leaq	24(%rbp), %rax
	movq	%rax, 8(%rsp)
	cmpq	%rax, %r15
	jnb	.L17
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L17:
	movq	8(%r14), %rbx
	movq	16(%r14), %rax
	leaq	24(%r14), %rcx
	movq	%rcx, %rdi
	movq	%rbx, 8(%rbp)
	movq	%rax, 16(%rbp)
	testq	%rax, %rax
	jne	.L18
	leaq	0(,%rbx,8), %rax
	subq	%rax, %r13
	cmpq	%r13, %r15
	jbe	.L19
	cmpq	%r13, 8(%rsp)
	ja	.L20
	movq	%r13, %r15
.L19:
	subq	$1, %rbx
	movq	8(%rsp), %rbp
	js	.L15
	jmp	.L86
.L20:
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L86:
	movq	24(%r14,%rbx,8), %rax
	movq	%rax, 0(%r13,%rbx,8)
	subq	$1, %rbx
	cmpq	$-1, %rbx
	jne	.L86
	movq	8(%rsp), %rbp
	jmp	.L15
.L18:
	movl	$REAL+2, %edx
	cmpq	%rax, %rdx
	je	.L85
	movl	$dINT+2, %ecx
	cmpq	%rax, %rcx
	jne	.L22
.L85:
	movq	8(%rsp), %rax
	leaq	(%rax,%rbx,8), %rbp
	cmpq	%rbp, %r15
	jb	.L69
	movq	%rbx, %rdx
	movq	%rdi, %rsi
	movq	%rax, %rdi
	call	copy
	jmp	.L15
.L22:
	movl	$BOOL+2, %edx
	cmpq	%rax, %rdx
	jne	.L24
	leaq	7(%rbx), %rdx
	sarq	$3, %rdx
	movq	8(%rsp), %rax
	leaq	(%rax,%rdx,8), %rbp
	cmpq	%rbp, %r15
	jb	.L70
	movq	%rdi, %rsi
	movq	%rax, %rdi
	call	copy
	jmp	.L15
.L24:
	movzwl	(%rax), %edx
	movzwl	%dx, %ecx
	movq	%rcx, 24(%rsp)
	movzwl	-2(%rax), %eax
	subq	$256, %rax
	testq	%rcx, %rcx
	jne	.L25
	movq	%rbx, %rdx
	imulq	%rax, %rdx
	movq	8(%rsp), %rax
	leaq	(%rax,%rdx,8), %rbp
	cmpq	%rbp, %r15
	jnb	.L26
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L26:
	movq	%rdi, %rsi
	movq	8(%rsp), %rdi
	call	copy
	jmp	.L15
.L25:
	cmpq	%rax, 24(%rsp)
	jne	.L27
	movq	24(%rsp), %rax
	imulq	%rbx, %rax
	leaq	0(,%rax,8), %rdx
	subq	%rdx, %r13
	cmpq	%r13, %r15
	jbe	.L28
	cmpq	%r13, 8(%rsp)
	ja	.L29
	movq	%r13, %r15
.L28:
	subq	$1, %rax
	movq	8(%rsp), %rbp
	js	.L15
	jmp	.L87
.L29:
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L87:
	movq	24(%r14,%rax,8), %rdx
	movq	%rdx, 0(%r13,%rax,8)
	subq	$1, %rax
	cmpq	$-1, %rax
	jne	.L87
	movq	8(%rsp), %rbp
	jmp	.L15
.L27:
	movq	24(%rsp), %rsi
	subq	%rsi, %rax
	movq	%rax, %rcx
	movq	%rax, 32(%rsp)
	movq	%rsi, %rax
	imulq	%rbx, %rax
	imulq	%rbx, %rcx
	movq	8(%rsp), %rsi
	leaq	(%rsi,%rcx,8), %rcx
	cmpq	%rcx, %r15
	jnb	.L31
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L31:
	salq	$3, %rax
	subq	%rax, %r13
	cmpq	%r13, %r15
	jbe	.L32
	cmpq	%r13, %rcx
	jbe	.L71
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L71:
	movq	%r13, %r15
.L32:
	testq	%rbx, %rbx
	jle	.L72
	movzwl	%dx, %edx
	leaq	0(,%rdx,8), %rax
	movq	32(%rsp), %rcx
	salq	$3, %rcx
	movq	%rcx, %rsi
	movq	%rcx, 40(%rsp)
	movq	%r13, %r12
	movq	8(%rsp), %r14
	movl	$0, %ebp
	movq	%rax, %rcx
	addq	%rsi, %rax
	movq	%rax, 48(%rsp)
	movq	%r13, 56(%rsp)
	movq	%r15, 64(%rsp)
	movq	%rdi, %r13
	movq	%rcx, %r15
.L33:
	movq	24(%rsp), %rdx
	movq	%r13, %rsi
	movq	%r12, %rdi
	call	copy
	addq	%r15, %r12
	leaq	0(%r13,%r15), %rsi
	movq	32(%rsp), %rdx
	movq	%r14, %rdi
	call	copy
	addq	40(%rsp), %r14
	addq	48(%rsp), %r13
	addq	$1, %rbp
	cmpq	%rbp, %rbx
	jne	.L33
	movq	56(%rsp), %r13
	movq	64(%rsp), %r15
	imulq	40(%rsp), %rbx
	movq	8(%rsp), %rbp
	addq	%rbx, %rbp
	jmp	.L15
.L12:
	cmpq	$1, %rax
	jne	.L34
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	jmp	.L9
.L34:
	cmpq	$2, %rax
	jne	.L36
	cmpq	%r15, %r13
	ja	.L37
	cmpq	%rdi, %r13
	ja	.L38
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L38:
	subq	$8, %r15
.L37:
	movq	16(%r14), %rax
	movq	%rax, -8(%r13)
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	leaq	-8(%r13), %r13
	jmp	.L9
.L36:
	cmpq	$255, %rax
	ja	.L39
	movq	16(%r14), %rcx
	subq	$1, %rax
	leaq	0(,%rax,8), %rdx
	subq	%rdx, %r13
	cmpq	%r13, %r15
	jbe	.L40
	cmpq	%r13, %rdi
	jbe	.L73
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L73:
	movq	%r13, %r15
.L40:
	leaq	-1(%rax), %rdx
	leaq	0(,%rdx,8), %rsi
	movq	(%rcx,%rdx,8), %r8
	movq	%r8, 0(%r13,%rdx,8)
	movq	-8(%rcx,%rsi), %rdx
	movq	%rdx, -8(%r13,%rsi)
	subq	$3, %rax
	js	.L41
.L88:
	movq	(%rcx,%rax,8), %rdx
	movq	%rdx, 0(%r13,%rax,8)
	subq	$1, %rax
	jns	.L88
.L41:
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	jmp	.L9
.L39:
	movzwl	(%r12), %ecx
	movzwl	%cx, %ebx
	leaq	-256(%rax), %rdx
	cmpq	$1, %rdx
	jne	.L43
	testq	%rbx, %rbx
	jne	.L44
	cmpq	%rdi, %r15
	jbe	.L74
	movq	8(%r14), %rax
	movq	%rax, 8(%rbp)
	leaq	16(%rbp), %rbp
	jmp	.L15
.L44:
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	jmp	.L9
.L43:
	cmpq	$2, %rdx
	jne	.L45
	testq	%rbx, %rbx
	jne	.L46
	leaq	24(%rbp), %rax
	cmpq	%rax, %r15
	jb	.L75
	movq	8(%r14), %rdx
	movq	%rdx, 8(%rbp)
	movq	16(%r14), %rdx
	movq	%rdx, 16(%rbp)
	movq	%rax, %rbp
	jmp	.L15
.L46:
	cmpq	$1, %rbx
	jne	.L47
	cmpq	%rdi, %r15
	jbe	.L76
	movq	16(%r14), %rax
	movq	%rax, 8(%rbp)
	leaq	16(%rbp), %rbp
	jmp	.L48
.L47:
	cmpq	%r15, %r13
	ja	.L49
	cmpq	%rdi, %r13
	ja	.L50
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L50:
	subq	$8, %r15
.L49:
	movq	16(%r14), %rax
	movq	%rax, -8(%r13)
	movq	%rdi, %rbp
	leaq	-8(%r13), %r13
.L48:
	movq	8(%r14), %r14
	jmp	.L9
.L45:
	movq	16(%r14), %rsi
	movq	%rsi, 8(%rsp)
	testq	%rbx, %rbx
	jne	.L51
	leaq	0(,%rdx,8), %rbx
	addq	%rbx, %rdi
	cmpq	%rdi, %r15
	jbe	.L77
	movq	8(%r14), %rdx
	movq	%rdx, 8(%rbp)
	addq	$16, %rbp
	leaq	-257(%rax), %rdx
	movq	%rbp, %rdi
	call	copy
	leaq	-8(%rbp,%rbx), %rbp
	jmp	.L15
.L51:
	movzwl	%cx, %ecx
	subq	%rcx, %rdx
	testq	%rdx, %rdx
	jle	.L78
	leaq	(%rdi,%rdx,8), %rbp
	cmpq	%rbp, %r15
	jb	.L79
	movq	8(%rsp), %rax
	leaq	-8(%rax,%rcx,8), %rsi
	call	copy
	jmp	.L52
.L78:
	movq	%rdi, %rbp
.L52:
	leaq	-1(%rbx), %rdx
	testq	%rdx, %rdx
	jle	.L53
	leaq	0(,%rdx,8), %rax
	subq	%rax, %r13
	cmpq	%r13, %r15
	jbe	.L54
	cmpq	%r13, %rbp
	jbe	.L80
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L80:
	movq	%r13, %r15
.L54:
	movq	8(%rsp), %rsi
	movq	%r13, %rdi
	call	copy
.L53:
	movq	8(%r14), %r14
	jmp	.L9
.L11:
	movl	-4(%r12), %ebx
	movslq	%ebx, %rdx
	cmpq	$1, %rdx
	jle	.L55
	cmpq	$255, %rdx
	jg	.L56
	leaq	-8(,%rdx,8), %rax
	subq	%rax, %r13
	cmpq	%r13, %r15
	jbe	.L57
	cmpq	%r13, %rdi
	jbe	.L81
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L81:
	movq	%r13, %r15
.L57:
	movq	8(%r14,%rax), %rcx
	movq	%rcx, -8(%r13,%rax)
	subq	$3, %rdx
	js	.L58
	movslq	%ebx, %rbx
.L59:
	movq	-8(%r14,%rbx,8), %rax
	movq	%rax, -24(%r13,%rbx,8)
	subq	$1, %rbx
	cmpq	$2, %rbx
	jne	.L59
.L58:
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	jmp	.L9
.L56:
	sarq	$8, %rdx
	movzbl	%bl, %ebx
	subq	%rdx, %rbx
	leaq	(%rdi,%rdx,8), %rbp
	cmpq	%rbp, %r15
	jb	.L82
	movq	%rbx, 8(%rsp)
	leaq	8(,%rbx,8), %rbx
	leaq	(%r14,%rbx), %rsi
	call	copy
	movq	8(%rsp), %rax
	testq	%rax, %rax
	je	.L15
	cmpq	$1, %rax
	jle	.L60
	leaq	-16(%rbx), %rax
	subq	%rax, %r13
	cmpq	%r13, %r15
	jbe	.L61
	cmpq	%r13, %rbp
	jbe	.L83
	movq	%r12, (%r14)
	movl	$0, %eax
	jmp	.L8
.L83:
	movq	%r13, %r15
.L61:
	movq	-8(%r14,%rbx), %rax
	movq	%rax, -24(%r13,%rbx)
	movq	8(%rsp), %rbx
	subq	$3, %rbx
	js	.L60
.L89:
	movq	16(%r14,%rbx,8), %rax
	movq	%rax, 0(%r13,%rbx,8)
	subq	$1, %rbx
	cmpq	$-1, %rbx
	jne	.L89
.L60:
	movq	8(%r14), %r14
	jmp	.L9
.L55:
	testq	%rdx, %rdx
	je	.L84
	movq	8(%r14), %r14
	movq	%rdi, %rbp
	jmp	.L9
.L10:
	subq	%rbp, %r12
	movq	%r12, 0(%rbp)
	addq	$8, %rbp
	jmp	.L15
.L68:
	movq	%rdi, %rbp
	jmp	.L15
.L72:
	movq	8(%rsp), %rbp
	jmp	.L15
.L84:
	movq	%rdi, %rbp
.L15:
	cmpq	(%rsp), %r13
	jne	.L63
	movq	16(%rsp), %rcx
	subq	%rcx, %rbp
	leaq	-16(%rbp), %rax
	movq	%rax, 8(%rcx)
	movq	%rcx, %rax
	jmp	.L8
.L63:
	movq	0(%r13), %r14
	leaq	8(%r13), %r13
	jmp	.L9
.L64:
	movl	$0, %eax
	jmp	.L8
.L65:
	movl	$0, %eax
	jmp	.L8
.L66:
	movl	$0, %eax
	jmp	.L8
.L67:
	movl	$0, %eax
	jmp	.L8
.L69:
	movl	$0, %eax
	jmp	.L8
.L70:
	movl	$0, %eax
	jmp	.L8
.L74:
	movl	$0, %eax
	jmp	.L8
.L75:
	movl	$0, %eax
	jmp	.L8
.L76:
	movl	$0, %eax
	jmp	.L8
.L77:
	movl	$0, %eax
	jmp	.L8
.L79:
	movl	$0, %eax
	jmp	.L8
.L82:
	movl	$0, %eax
.L8:
	addq	$72, %rsp

	popq	%rbx

	popq	%rbp

	popq	%r12

	popq	%r13

	popq	%r14

	popq	%r15

	ret

.LFE18:
	.size	copy_graph_to_string, .-copy_graph_to_string
	.globl	remove_forwarding_pointers_from_graph
	.type	remove_forwarding_pointers_from_graph, @function
remove_forwarding_pointers_from_graph:
.LFB19:

	pushq	%r15


	pushq	%r14


	pushq	%r13


	pushq	%r12


	pushq	%rbp


	pushq	%rbx


	subq	$24, %rsp

	movq	%rsi, %rbp
	movq	%rsi, %rbx
.L100:
	movq	(%rdi), %rax
	testb	$1, %al
	je	.L101
	movq	-1(%rax), %rdx
	movq	%rdx, (%rdi)
	testb	$2, %dl
	je	.L102
	movzwl	-2(%rdx), %eax
	testq	%rax, %rax
	jne	.L103
	cmpq	$__ARRAY__+2, %rdx
	jne	.L101
	movq	16(%rdi), %rax
	testq	%rax, %rax
	jne	.L104
	movq	8(%rdi), %rax
	leaq	0(,%rax,8), %rdx
	subq	%rdx, %rbx
	subq	$1, %rax
	js	.L101
.L126:
	movq	24(%rdi,%rax,8), %rdx
	movq	%rdx, (%rbx,%rax,8)
	subq	$1, %rax
	cmpq	$-1, %rax
	jne	.L126
	jmp	.L101
.L104:
	cmpq	$dINT+2, %rax
	je	.L101
	cmpq	$REAL+2, %rax
	je	.L101
	cmpq	$BOOL+2, %rax
	je	.L101
	movzwl	(%rax), %edx
	movzwl	%dx, %r12d
	testq	%r12, %r12
	je	.L101
	movzwl	-2(%rax), %eax
	subq	$256, %rax
	movq	8(%rdi), %rsi
	movq	%rsi, (%rsp)
	leaq	24(%rdi), %r13
	cmpq	%rax, %r12
	jne	.L106
	movq	%rsi, %rax
	imulq	%r12, %rax
	leaq	0(,%rax,8), %rdx
	subq	%rdx, %rbx
	subq	$1, %rax
	js	.L101
.L127:
	movq	24(%rdi,%rax,8), %rdx
	movq	%rdx, (%rbx,%rax,8)
	subq	$1, %rax
	cmpq	$-1, %rax
	jne	.L127
	jmp	.L101
.L106:
	movq	(%rsp), %rsi
	movq	%rsi, %rcx
	imulq	%r12, %rcx
	salq	$3, %rcx
	subq	%rcx, %rbx
	testq	%rsi, %rsi
	jle	.L101
	movzwl	%dx, %edx
	leaq	0(,%rdx,8), %rsi
	movq	%rsi, 8(%rsp)
	salq	$3, %rax
	movq	%rax, 16(%rsp)
	movq	%rbx, %r15
	movl	$0, %r14d
.L108:
	movq	%r12, %rdx
	movq	%r13, %rsi
	movq	%r15, %rdi
	call	copy
	addq	8(%rsp), %r15
	addq	16(%rsp), %r13
	addq	$1, %r14
	cmpq	%r14, (%rsp)
	jne	.L108
	jmp	.L101
.L103:
	cmpq	$1, %rax
	jne	.L109
	movq	8(%rdi), %rdi
	jmp	.L100
.L109:
	cmpq	$2, %rax
	jne	.L111
	movq	16(%rdi), %rax
	movq	%rax, -8(%rbx)
	movq	8(%rdi), %rdi
	leaq	-8(%rbx), %rbx
	jmp	.L100
.L111:
	cmpq	$255, %rax
	ja	.L112
	movq	16(%rdi), %rcx
	leaq	-8(,%rax,8), %rdx
	subq	%rdx, %rbx
	leaq	-2(%rax), %rdx
	movq	(%rcx,%rdx,8), %rsi
	movq	%rsi, (%rbx,%rdx,8)
	subq	$3, %rax
	js	.L113
.L128:
	movq	(%rcx,%rax,8), %rdx
	movq	%rdx, (%rbx,%rax,8)
	subq	$1, %rax
	jns	.L128
.L113:
	movq	8(%rdi), %rdi
	jmp	.L100
.L112:
	movzwl	(%rdx), %edx
	testq	%rdx, %rdx
	je	.L101
	cmpq	$1, %rdx
	jle	.L115
	cmpq	$2, %rdx
	jne	.L116
	cmpq	$258, %rax
	jne	.L117
	movq	16(%rdi), %rax
	movq	%rax, -8(%rbx)
	leaq	-8(%rbx), %rbx
	jmp	.L115
.L117:
	movq	16(%rdi), %rax
	movq	(%rax), %rax
	movq	%rax, -8(%rbx)
	leaq	-8(%rbx), %rbx
	jmp	.L115
.L116:
	movq	16(%rdi), %rcx
	leaq	-8(,%rdx,8), %rax
	subq	%rax, %rbx
	movq	-8(%rcx,%rax), %rsi
	movq	%rsi, -8(%rbx,%rax)
	subq	$3, %rdx
	js	.L115
	leaq	(%rbx,%rdx,8), %rax
	leaq	-8(%rbx), %r8
	movq	%rbx, %rsi
	negq	%rsi
.L118:
	leaq	(%rcx,%rax), %rdx
	movq	(%rdx,%rsi), %rdx
	movq	%rdx, (%rax)
	subq	$8, %rax
	cmpq	%rax, %r8
	jne	.L118
.L115:
	movq	8(%rdi), %rdi
	jmp	.L100
.L102:
	movl	-4(%rdx), %eax
	movslq	%eax, %rdx
	cmpq	$1, %rdx
	jle	.L119
	cmpq	$255, %rdx
	jg	.L120
	leaq	-8(,%rdx,8), %rcx
	subq	%rcx, %rbx
	movq	8(%rdi,%rcx), %rsi
	movq	%rsi, -8(%rbx,%rcx)
	subq	$3, %rdx
	js	.L121
	cltq
.L122:
	movq	-8(%rdi,%rax,8), %rdx
	movq	%rdx, -24(%rbx,%rax,8)
	subq	$1, %rax
	cmpq	$2, %rax
	jne	.L122
.L121:
	movq	8(%rdi), %rdi
	jmp	.L100
.L120:
	movzbl	%al, %eax
	sarq	$8, %rdx
	subq	%rdx, %rax
	je	.L101
	cmpq	$1, %rax
	jle	.L123
	leaq	-8(,%rax,8), %rdx
	subq	%rdx, %rbx
	movq	8(%rdi,%rdx), %rcx
	movq	%rcx, -8(%rbx,%rdx)
	subq	$3, %rax
	js	.L123
.L129:
	movq	16(%rdi,%rax,8), %rdx
	movq	%rdx, (%rbx,%rax,8)
	subq	$1, %rax
	cmpq	$-1, %rax
	jne	.L129
.L123:
	movq	8(%rdi), %rdi
	jmp	.L100
.L119:
	testq	%rdx, %rdx
	je	.L101
	movq	8(%rdi), %rdi
	jmp	.L100
.L101:
	cmpq	%rbp, %rbx
	je	.L99
	movq	(%rbx), %rdi
	leaq	8(%rbx), %rbx
	jmp	.L100
.L99:
	addq	$24, %rsp

	popq	%rbx

	popq	%rbp

	popq	%r12

	popq	%r13

	popq	%r14

	popq	%r15

	ret

.LFE19:
	.size	remove_forwarding_pointers_from_graph, .-remove_forwarding_pointers_from_graph
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.10) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
