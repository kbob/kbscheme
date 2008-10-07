	.file	"q2.c"
	.text
	.p2align 4,,15
.globl f
	.type	f, @function
f:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	movl	$100, (%eax)
	movl	$200, %eax
	popl	%ebp
	ret
	.size	f, .-f
	.p2align 4,,15
.globl g
	.type	g, @function
g:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	movl	$200, 4(%eax)
	movl	$100, (%eax)
	popl	%ebp
	ret	$4
	.size	g, .-g
	.p2align 4,,15
.globl callg
	.type	callg, @function
callg:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	leal	-8(%ebp), %eax
	movl	%eax, (%esp)
	call	g
	movl	-8(%ebp), %eax
	movl	-4(%ebp), %edx
	subl	$4, %esp
	movl	%edx, 4(%esp)
	movl	%eax, (%esp)
	call	x
	leave
	ret
	.size	callg, .-callg
	.p2align 4,,15
.globl callf
	.type	callf, @function
callf:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	leal	-4(%ebp), %eax
	movl	%eax, (%esp)
	call	f
	movl	-4(%ebp), %edx
	movl	%edx, 4(%esp)
	movl	%eax, (%esp)
	call	x
	leave
	ret
	.size	callf, .-callf
	.ident	"GCC: (GNU) 4.2.3 (Ubuntu 4.2.3-2ubuntu7)"
	.section	.note.GNU-stack,"",@progbits
