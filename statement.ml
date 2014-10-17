type operand =
  | OLabelRef of string
  | ORegister of int
  | OImmediate of Big_int.big_int
  | ODisplacement of Big_int.big_int * int

type optype =
  | OpTypeR of int
  | OpTypeR1 of int
  | OpTypeRL of int
  | OpTypeR2 of int
  | OpTypeRI of int
  | OpTypeRS of int
  | OpTypeI of int
  | OpTypeID of int
  | OpTypeI1 of int
  | OpTypeIJ of int
  | OpTypeJ of int

type gpr = int

type instruction =
  | InstR of int * gpr * gpr * gpr * int
  | InstI of int * gpr * gpr * Big_int.big_int
  | InstIJ of int * gpr * gpr * string
  | InstJ of int * string

let nop = InstR (0, 0, 0, 0, 0)

type statement =
  | SLabel of string
  | SInstruction of instruction

let optable =
  let tbl = Hashtbl.create 987 in
  List.iter (function (mnemonic, optype) ->
    Hashtbl.add tbl mnemonic optype
  ) [
    "sll"    , OpTypeRS  0b000000;
    "srl"    , OpTypeRS  0b000010;
    "sra"    , OpTypeRS  0b000011;
    "sllv"   , OpTypeR   0b000100;
    "srlv"   , OpTypeR   0b000110;
    "srav"   , OpTypeR   0b000111;
    "jr"     , OpTypeR1  0b001000;
    "jalr"   , OpTypeR1  0b001001;
    "syscall", OpTypeRI  0b001100;
    "break"  , OpTypeRI  0b001100;
    "mfhi"   , OpTypeRL  0b010000;
    "mthi"   , OpTypeR1  0b010001;
    "mflo"   , OpTypeRL  0b010010;
    "mtlo"   , OpTypeR1  0b010011;
    "mult"   , OpTypeR2  0b011000;
    "multu"  , OpTypeR2  0b011001;
    "div"    , OpTypeR2  0b011010;
    "divu"   , OpTypeR2  0b011011;
    "add"    , OpTypeR   0b100000;
    "addu"   , OpTypeR   0b100001;
    "sub"    , OpTypeR   0b100010;
    "subu"   , OpTypeR   0b100011;
    "and"    , OpTypeR   0b100100;
    "or"     , OpTypeR   0b100101;
    "nor"    , OpTypeR   0b100110;
    "xor"    , OpTypeR   0b100111;
    "slt"    , OpTypeR   0b101010;
    "sltu"   , OpTypeR   0b101011;
    "j"      , OpTypeJ  0b000010;
    "jal"    , OpTypeJ  0b000011;
    "beq"    , OpTypeIJ 0b000100;
    "bne"    , OpTypeIJ 0b000101;
    "blez"   , OpTypeIJ 0b000110;
    "bgtz"   , OpTypeIJ 0b000111;
    "addi"   , OpTypeI  0b001000;
    "addiu"  , OpTypeI  0b001001;
    "slti"   , OpTypeI  0b001010;
    "sltiu"  , OpTypeI  0b001011;
    "andi"   , OpTypeI  0b001100;
    "ori"    , OpTypeI  0b001101;
    "xori"   , OpTypeI  0b001110;
    "lui"    , OpTypeI  0b001111;
    "rrb"    , OpTypeI1 0b011100;
    "rsb"    , OpTypeI1 0b011101;
    "lb"     , OpTypeID 0b100000;
    "lh"     , OpTypeID 0b100001;
    "lwl"    , OpTypeID 0b100010;
    "lw"     , OpTypeID 0b100011;
    "lbu"    , OpTypeID 0b100100;
    "lhu"    , OpTypeID 0b100101;
    "lwr"    , OpTypeID 0b100110;
    "sb"     , OpTypeID 0b101000;
    "sh"     , OpTypeID 0b101001;
    "swl"    , OpTypeID 0b101010;
    "sw"     , OpTypeID 0b101011;
  ];
  tbl

let translate_instruction opname args =
  let optype =
    try Hashtbl.find optable opname
    with Not_found ->
      raise (Failure ("Unknown opname "^opname))
  in
  begin match optype with
  | OpTypeR funct ->
      begin match args with
      | [ ORegister rd; ORegister rs; ORegister rt ] ->
          InstR (funct, rs, rt, rd, 0)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeR1 funct ->
      begin match args with
      | [ ORegister rs ] ->
          InstR (funct, rs, 0, 0, 0)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeRS funct ->
      begin match args with
      | [ ORegister rd; ORegister rt; OImmediate shamt ] ->
          InstR (funct, 0, rt, rd, Big_int.int_of_big_int shamt)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeJ opcode ->
      begin match args with
      | [ OLabelRef l ] ->
          InstJ (opcode, l)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeI opcode ->
      begin match args with
      | [ ORegister rt; ORegister rs; OImmediate imm ] ->
          InstI (opcode, rs, rt, imm)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeIJ opcode ->
      begin match args with
      | [ ORegister rt; ORegister rs; OLabelRef l ] ->
          InstIJ (opcode, rs, rt, l)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeID opcode ->
      begin match args with
      | [ ORegister rt; ODisplacement (offset, base) ] ->
          InstI (opcode, base, rt, offset)
      | _ -> raise (Failure "Illegal Operand List")
      end
  | OpTypeI1 opcode ->
      begin match args with
      | [ ORegister rt ] ->
          InstI (opcode, 0, rt, Big_int.zero_big_int)
      | _ -> raise (Failure "Illegal Operand List")
      end
  end

let emit_instruction labels pos inst =
  begin match inst with
  | InstR (funct, rs, rt, rd, shamt) ->
      [|
        (rs lsr 3) land 255;
        ((rs lsl 5) lor rt) land 255;
        ((rd lsl 3) lor (shamt lsr 2)) land 255;
        ((shamt lsl 6) lor funct) land 255;
      |]
  | InstI (opcode, rs, rt, imm) ->
      let imm_int = Big_int.int_of_big_int imm in
      [|
        ((opcode lsl 2) lor (rs lsr 3)) land 255;
        ((rs lsl 5) lor rt) land 255;
        (imm_int lsr 8) land 255;
        imm_int land 255;
      |]
  | InstIJ (opcode, rs, rt, l) ->
      let jpos =
        try Hashtbl.find labels l
        with Not_found ->
          raise (Failure ("label "^l^" not found"))
      in
      let posdiff = jpos - (pos + 1) in
      [|
        ((opcode lsl 2) lor (rs lsr 3)) land 255;
        ((rs lsl 5) lor rt) land 255;
        (posdiff lsr 8) land 255;
        posdiff land 255;
      |]
  | InstJ (opcode, l) ->
      let jpos =
        try Hashtbl.find labels l
        with Not_found ->
          raise (Failure ("label "^l^" not found"))
      in
      (* let posdiff = jpos - (pos + 1) in *)
      [|
        ((opcode lsl 2) lor (jpos lsr 24)) land 255;
        (jpos lsr 16) land 255;
        (jpos lsr 8) land 255;
        jpos land 255;
      |]
  end

