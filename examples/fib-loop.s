start:
	rrb	$t0
	sll	$a0, $t0, 24
	rrb	$t0
	sll	$t0, $t0, 16
	or	$a0, $a0, $t0
	rrb	$t0
	sll	$t0, $t0, 8
	or	$a0, $a0, $t0
	rrb	$t0
	or	$a0, $a0, $t0
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
	addiu	$v0, $t2, 0
	srl	$t0, $v0, 24
	rsb	$t0
	srl	$t0, $v0, 16
	rsb	$t0
	srl	$t0, $v0, 8
	rsb	$t0
	rsb	$v0
	j	start
