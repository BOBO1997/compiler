.data
.balign	8
.text
sum.4:
	cmpl	$0, %ebx
	jg	jle_else.11
	ret
jle_else.11:
	addl	%ebx, %eax
	subl	$1, %ebx
	jmp	sum.4
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
	movl	$0, %eax
	movl	$10000, %ebx
	call	sum.4
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
