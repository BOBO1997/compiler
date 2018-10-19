.data
.balign	8
l.237:	# 12.000000
	.long	0x0
	.long	0x40280000
l.235:	# 11.000000
	.long	0x0
	.long	0x40260000
l.233:	# 10.000000
	.long	0x0
	.long	0x40240000
l.231:	# 9.000000
	.long	0x0
	.long	0x40220000
l.229:	# 8.000000
	.long	0x0
	.long	0x40200000
l.227:	# 7.000000
	.long	0x0
	.long	0x401c0000
l.225:	# 6.000000
	.long	0x0
	.long	0x40180000
l.223:	# 5.000000
	.long	0x0
	.long	0x40140000
l.221:	# 4.000000
	.long	0x0
	.long	0x40100000
l.219:	# 3.000000
	.long	0x0
	.long	0x40080000
l.217:	# 2.000000
	.long	0x0
	.long	0x40000000
l.215:	# 1.000000
	.long	0x0
	.long	0x3ff00000
l.212:	# 0.000000
	.long	0x0
	.long	0x0
.text
loop3.86:
	cmpl	$0, %ebx
	jl	jge_else.274
	movl	%esi, 0(%ebp)
	movl	(%edi,%eax,4), %esi
	movl	%edi, 4(%ebp)
	movl	(%edi,%eax,4), %edi
	movsd	(%edi,%ecx,8), %xmm0
	movl	(%edx,%eax,4), %edi
	movsd	(%edi,%ebx,8), %xmm1
	movl	0(%ebp), %edi
	movl	(%edi,%ebx,4), %edi
	movsd	(%edi,%ecx,8), %xmm2
	mulsd	%xmm2, %xmm1
	addsd	%xmm1, %xmm0
	movsd	%xmm0, (%esi,%ecx,8)
	subl	$1, %ebx
	movl	0(%ebp), %esi
	movl	4(%ebp), %edi
	jmp	loop3.86
jge_else.274:
	ret
loop2.93:
	cmpl	$0, %ecx
	jl	jge_else.276
	movl	%ebx, 0(%ebp)
	subl	$1, %ebx
	movl	%edi, 4(%ebp)
	movl	%esi, 8(%ebp)
	movl	%edx, 12(%ebp)
	movl	%eax, 16(%ebp)
	movl	%ecx, 20(%ebp)
	addl	$24, %ebp
	call	loop3.86
	subl	$24, %ebp
	movl	20(%ebp), %eax
	movl	%eax, %ecx
	subl	$1, %ecx
	movl	16(%ebp), %eax
	movl	0(%ebp), %ebx
	movl	12(%ebp), %edx
	movl	8(%ebp), %esi
	movl	4(%ebp), %edi
	jmp	loop2.93
jge_else.276:
	ret
loop1.100:
	cmpl	$0, %eax
	jl	jge_else.278
	movl	%ecx, 0(%ebp)
	subl	$1, %ecx
	movl	%edi, 4(%ebp)
	movl	%esi, 8(%ebp)
	movl	%edx, 12(%ebp)
	movl	%ebx, 16(%ebp)
	movl	%eax, 20(%ebp)
	addl	$24, %ebp
	call	loop2.93
	subl	$24, %ebp
	movl	20(%ebp), %eax
	subl	$1, %eax
	movl	16(%ebp), %ebx
	movl	0(%ebp), %ecx
	movl	12(%ebp), %edx
	movl	8(%ebp), %esi
	movl	4(%ebp), %edi
	jmp	loop1.100
jge_else.278:
	ret
mul.107:
	subl	$1, %eax
	jmp	loop1.100
init.115:
	cmpl	$0, %eax
	jl	jge_else.280
	movl	$l.212, %edx
	movsd	0(%edx), %xmm0
	movl	%ebx, 0(%ebp)
	movl	%eax, 4(%ebp)
	movl	%ecx, 8(%ebp)
	movl	%ebx, %eax
	addl	$16, %ebp
	call	min_caml_create_float_array
	subl	$16, %ebp
	movl	4(%ebp), %ebx
	movl	8(%ebp), %ecx
	movl	%eax, (%ecx,%ebx,4)
	movl	%ebx, %eax
	subl	$1, %eax
	movl	0(%ebp), %ebx
	jmp	init.115
jge_else.280:
	ret
make.119:
	movl	%ebx, 0(%ebp)
	movl	%eax, 4(%ebp)
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	min_caml_create_array
	subl	$8, %ebp
	movl	%eax, %ecx
	movl	4(%ebp), %eax
	subl	$1, %eax
	movl	0(%ebp), %ebx
	movl	%ecx, 8(%ebp)
	addl	$16, %ebp
	call	init.115
	subl	$16, %ebp
	movl	8(%ebp), %eax
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
	movl	$0, %eax
	movl	$l.212, %ebx
	movsd	0(%ebx), %xmm0
	call	min_caml_create_float_array
	movl	%eax, %ecx
	movl	$2, %eax
	movl	$3, %ebx
	movl	%ecx, 0(%ebp)
	addl	$8, %ebp
	call	make.119
	subl	$8, %ebp
	movl	$3, %ebx
	movl	$2, %ecx
	movl	0(%ebp), %edx
	movl	%eax, 4(%ebp)
	movl	%ebx, %eax
	movl	%ecx, %ebx
	movl	%edx, %ecx
	addl	$8, %ebp
	call	make.119
	subl	$8, %ebp
	movl	$2, %ebx
	movl	0(%ebp), %ecx
	movl	%eax, 8(%ebp)
	movl	%ebx, %eax
	addl	$16, %ebp
	call	make.119
	subl	$16, %ebp
	movl	%eax, %edi
	movl	4(%ebp), %edx
	movl	0(%edx), %eax
	movl	$l.215, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	0(%edx), %eax
	movl	$l.217, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	0(%edx), %eax
	movl	$l.219, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 16(%eax)
	movl	4(%edx), %eax
	movl	$l.221, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	4(%edx), %eax
	movl	$l.223, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	4(%edx), %eax
	movl	$l.225, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 16(%eax)
	movl	8(%ebp), %esi
	movl	0(%esi), %eax
	movl	$l.227, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	0(%esi), %eax
	movl	$l.229, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	4(%esi), %eax
	movl	$l.231, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	4(%esi), %eax
	movl	$l.233, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	8(%esi), %eax
	movl	$l.235, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	8(%esi), %eax
	movl	$l.237, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	$3, %ebx
	movl	$2, %eax
	movl	%edi, 12(%ebp)
	movl	%eax, %ecx
	addl	$16, %ebp
	call	mul.107
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	0(%eax), %ebx
	movsd	0(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	0(%eax), %ebx
	movsd	8(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	4(%eax), %ebx
	movsd	0(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	4(%eax), %eax
	movsd	8(%eax), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
