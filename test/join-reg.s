.data
.balign	8
.text
f.11:
	movl	$123, %eax
	ret
g.13:
	movl	$456, %eax
	ret
h.15:
	movl	$789, %eax
	ret
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
	call	f.11
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	g.13
	subl	$8, %ebp
	movl	%eax, 4(%ebp)
	addl	$8, %ebp
	call	h.15
	subl	$8, %ebp
	cmpl	$0, %eax
	jne	je_else.29
	movl	4(%ebp), %eax
	movl	0(%ebp), %ebx
	subl	%eax, %ebx
	jmp	je_cont.30
je_else.29:
	movl	0(%ebp), %eax
	movl	4(%ebp), %ebx
	subl	%eax, %ebx
je_cont.30:
	movl	0(%ebp), %eax
	addl	%eax, %ebx
	movl	4(%ebp), %eax
	addl	%ebx, %eax
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
