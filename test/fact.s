.data
.balign	8
.text
fact.7:
	cmpl	$0, %eax
	jne	je_else.17
	movl	$1, %eax
	ret
je_else.17:
	movl	%eax, %ebx
	subl	$1, %ebx
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	fact.7
	subl	$8, %ebp
	movl	0(%ebp), %ebx
