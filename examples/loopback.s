loopback:
	jal recv_byte
	mov $a0, $v0
	jal send_byte
	j loopback
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
