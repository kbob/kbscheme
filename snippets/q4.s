	.file	"q4.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"pthread_create"
.LC1:
	.string	"pthread_join"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	xorl	%ebx, %ebx
	pushl	%ecx
	subl	$44, %esp
	leal	-16(%ebp), %esi
.L2:
	movl	$0, 12(%esp)
	movl	$target, 8(%esp)
	movl	$0, 4(%esp)
	movl	%esi, (%esp)
	call	pthread_create
	testl	%eax, %eax
	jne	.L16
	addl	$1, %ebx
	cmpl	$3, %ebx
	jne	.L2
	xorb	%bl, %bl
	leal	-20(%ebp), %esi
.L6:
	movl	%esi, 4(%esp)
	movl	-16(%ebp), %eax
	movl	%eax, (%esp)
	call	pthread_join
	testl	%eax, %eax
	jne	.L17
	addl	$1, %ebx
	cmpl	$3, %ebx
	jne	.L6
	addl	$44, %esp
	xorl	%eax, %eax
	popl	%ecx
	popl	%ebx
	popl	%esi
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
.L16:
	movl	$.LC0, (%esp)
	call	perror
	movl	$1, (%esp)
	call	exit
.L17:
	movl	$.LC1, (%esp)
	call	perror
	movl	$1, (%esp)
	call	exit
	.size	main, .-main
	.section	.rodata.str1.1
.LC2:
	.string	"x = %d; &x = %p\n"
	.text
	.p2align 4,,15
.globl target
	.type	target, @function
target:
	movl	%gs:0, %eax
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	leal	x@NTPOFF(%eax), %eax
	movl	%eax, 8(%esp)
	movl	%gs:x@NTPOFF, %eax
	movl	$.LC2, (%esp)
	movl	%eax, 4(%esp)
	call	printf
	xorl	%eax, %eax
	leave
	ret
	.size	target, .-target
.globl x
	.section	.tdata,"awT",@progbits
	.align 4
	.type	x, @object
	.size	x, 4
x:
	.long	3
	.ident	"GCC: (GNU) 4.2.3 (Ubuntu 4.2.3-2ubuntu7)"
	.section	.note.GNU-stack,"",@progbits
