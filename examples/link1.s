# usage: ./qkasm examples/link{1,2}.s
	.globl hoge
hoge:
	j fuga
	.space 8
	.align 8
	.long 0x55555555
	.long 0x77777777
	.long hoge
	.long fuga
	la $zero, hoge
	la $zero, fuga
