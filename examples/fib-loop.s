start:
	jal	recv_byte
	sll	$a0, $v0, 24
	jal	recv_byte
	sll	$v0, $v0, 16
	or	$a0, $a0, $v0
	jal	recv_byte
	sll	$v0, $v0, 8
	or	$a0, $a0, $v0
	jal	recv_byte
	or	$a0, $a0, $v0
fib:
	addiu	$t0, $zero, 0
	addiu	$t1, $zero, 1
	addiu	$t2, $zero, 0
	addiu	$t3, $zero, 0
loop:
	slt	$t4, $t3, $a0
	beq	$t4, $zero, exit
	addiu	$t2, $t1, 0
	addu	$t1, $t0, $t1
	addiu	$t0, $t2, 0
	addiu	$t3, $t3, 1
	j	loop
exit:
	addiu	$s0, $t2, 0
	srl	$a0, $s0, 24
	jal	send_byte
	srl	$a0, $s0, 16
	jal	send_byte
	srl	$a0, $s0, 8
	jal	send_byte
	srl	$a0, $s0, 0
	jal	send_byte
	j	start
recv_byte:
	li $t0, 0xffff0000
rd_poll:
	lw $v0, 0($t0)
	andi $v0, $v0, 0x01
	beq $v0, $zero, rd_poll
	lw $v0, 4($t0)
	andi $v0, $v0, 0xff
	jr $ra
send_byte:
	li $t0, 0xffff0000
wr_poll:
	lw $v0, 8($t0)
	andi $v0, $v0, 0x01
	beq $v0, $zero, wr_poll
	sw $a0, 12($t0)
	jr $ra
