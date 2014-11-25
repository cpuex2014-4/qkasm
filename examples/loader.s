program_begin:
	# blah, blah...
# end of program
# start of loader
loader:
	li $s0, 0 # program position
loader_loop:
	li $s1, 0
	jal loader_recv_byte
	sll $v0, $v0, 24
	or $s1, $s1, $v0
	jal loader_recv_byte
	sll $v0, $v0, 16
	or $s1, $s1, $v0
	jal loader_recv_byte
	sll $v0, $v0, 8
	or $s1, $s1, $v0
	jal loader_recv_byte
	or $s1, $s1, $v0
	sw $s1, 0($s0)
	addiu $s0, $s0, 4
	addiu $s1, $s1, 1
	bne $s1, $zero, loader_loop
	addiu $s0, $s0, -4
	li $s1, 32
zerofill_loop:
	sw $zero, 0($s0)
	addiu $s0, $s0, 4
	addiu $s1, $s1, -1
	bne $s1, $zero, zerofill_loop
	li $t0, 0
	li $v0, 0
	li $s0, 0
	li $s1, 0
	li $ra, 0
	jr $zero # j program_begin
loader_recv_byte:
	li $t0, 0xffff0000 # MMIO memory position
rd_poll:
	lw $v0, 0($t0)
	andi $v0, $v0, 0x01
	beq $v0, $zero, rd_poll
	lw $v0, 4($t0)
	andi $v0, $v0, 0xff
	jr $ra
