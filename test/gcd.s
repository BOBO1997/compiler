.data
.balign	8
.text
gcd.4:
	cmpl	$0, %eax
	jne	je_else.11
	movl	%ebx, %eax
	ret
je_else.11:
	cmpl	%ebx, %eax
	jg	jle_else.12
	subl	%eax, %ebx
	jmp	gcd.4
jle_else.12:
	subl	%ebx, %eax
	movl	%ebx, 0(%ebp)
	movl	%eax, %ebx
	movl	0(%ebp), %eax
	jmp	gcd.4
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$21600, %eax
	movl	$337500, %ebx
	call	gcd.4
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
