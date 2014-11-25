	li	$fp, 16384
	li	$sp, 16384
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
	jal	fib
	mov	$s0, $v0
	srl	$a0, $s0, 24
	jal	send_byte
	srl	$a0, $s0, 16
	jal	send_byte
	srl	$a0, $s0, 8
	jal	send_byte
	srl	$a0, $s0, 0
	jal	send_byte
	j	start
fib:
	addiu	$sp, $sp, -12
	sw	$ra, 8($sp)
	sw	$a0, 4($sp)
	mov	$t1, $a0
	li	$t2, 1
	slt	$t0, $t2, $a0
	beq	$t0, $zero, exit
	addiu	$a0, $a0, -1
	jal	fib
	sw	$v0, 0($sp)
	lw	$a0, 4($sp)
	addiu	$a0, $a0, -2
	jal	fib
	lw	$t1, 0($sp)
	addu	$t1, $t1, $v0
	lw	$ra, 8($sp)
exit:
	addiu	$sp, $sp, 12
	mov	$v0, $t1
	jr	$ra
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
