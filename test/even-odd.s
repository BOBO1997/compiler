.data
.balign	8
.text
odd.11:
	cmpl	$0, %eax
	jg	jle_else.18
	cmpl	$0, %eax
	jl	jge_else.19
	movl	$456, %eax
	ret
jge_else.19:
	addl	$1, %eax
	jmp	even.8
jle_else.18:
	subl	$1, %eax
	jmp	even.8
even.8:
	cmpl	$0, %eax
	jg	jle_else.20
	cmpl	$0, %eax
	jl	jge_else.21
	movl	$123, %eax
	ret
jge_else.21:
	addl	$1, %eax
	jmp	odd.11
jle_else.20:
	subl	$1, %eax
	jmp	odd.11
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
	movl	$789, %eax
	call	even.8
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
