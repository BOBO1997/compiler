.data
.balign	8
l.26:	# -7.890000
	.long	0x28f5c28f
	.long	0xc01f8f5c
l.24:	# 4.560000
	.long	0xa3d70a3d
	.long	0x40123d70
l.22:	# 1.230000
	.long	0x7ae147ae
	.long	0x3ff3ae14
.text
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
	movl	$l.22, %eax
	movsd	0(%eax), %xmm0
	call	min_caml_truncate
	movl	$l.24, %ebx
	movsd	0(%ebx), %xmm0
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	min_caml_truncate
	subl	$8, %ebp
	movl	$l.26, %ebx
	movsd	0(%ebx), %xmm0
	movl	%eax, 4(%ebp)
	addl	$8, %ebp
	call	min_caml_truncate
	subl	$8, %ebp
	cmpl	$0, %eax
	jl	jge_else.31
	movl	0(%ebp), %ebx
	jmp	jge_cont.32
jge_else.31:
	movl	4(%ebp), %ebx
jge_cont.32:
	movl	0(%ebp), %ecx
	cmpl	$0, %ecx
	jg	jle_else.33
	movl	4(%ebp), %edx
	jmp	jle_cont.34
jle_else.33:
	movl	%eax, %edx
jle_cont.34:
	addl	%edx, %ebx
	movl	4(%ebp), %edx
	cmpl	$0, %edx
	jl	jge_else.35
	jmp	jge_cont.36
jge_else.35:
	movl	%ecx, %eax
jge_cont.36:
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
