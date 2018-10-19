.data
.balign	8
.text
g.3:
	movl	4(%edi), %ebx
	addl	%ebx, %eax
	ret
f.1:
	movl	min_caml_hp, %edi
	addl	$8, min_caml_hp
	movl	$g.3, %ebx
	movl	%ebx, 0(%edi)
	movl	%eax, 4(%edi)
	movl	$3, %eax
	jmp	*(%edi)
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
	movl	min_caml_hp, %eax
	addl	$8, min_caml_hp
	movl	$f.1, %ebx
	movl	%ebx, 0(%eax)
	movl	$2, %ebx
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
