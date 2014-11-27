open Big_int
open Loc

exception Assembly_error of string
exception Lexing_error of string loc
exception Parsing_error of string loc

type operand =
  | OLabelRef of string
  | ORegister of int
  | OFRegister of int
  | OImmediate of big_int
  | ODisplacement of big_int * int

type statement_desc =
  | SLabel of string
  | SInstruction of string * operand list

type statement = statement_desc loc

type preinstruction =
  | PILabel of string
  | PIConst of int array
  | PIJump of int * string
  | PIBranch of int array * string
  | PILoadAddress of int * string

let reg_zero = 0

let gen_itype_pre opcode rs rt imm =
  [|
    ((opcode lsl 2) lor (rs lsr 3)) land 255;
    ((rs lsl 5) lor rt) land 255;
    (imm lsr 8) land 255;
    imm land 255;
  |]

let generate_instruction lbl_tbl pc pi =
  match pi with
  | PILabel l -> []
  | PIConst x -> [x]
  | PIJump (opcode, jlabel) ->
      begin try
        let jtarget = Hashtbl.find lbl_tbl jlabel in
        assert (jtarget land 3 = 0);
        assert (jtarget lsr 28 = pc lsr 28);
        [[|
          ((opcode lsl 2) lor (jtarget lsr 26)) land 255;
          (jtarget lsr 18) land 255;
          (jtarget lsr 10) land 255;
          (jtarget lsr 2) land 255;
        |]]
      with Not_found ->
        failwith "label not found"
      end
  | PIBranch (upper_const, blabel) ->
      begin try
        let btarget = Hashtbl.find lbl_tbl blabel - (pc+4) in
        assert (btarget land 3 = 0);
        assert (-32768 <= btarget/4 && btarget/4 < 32768);
        [[|
          upper_const.(0);
          upper_const.(1);
          (btarget lsr 10) land 255;
          (btarget lsr 2) land 255;
        |]]
      with Not_found ->
        failwith "label not found"
      end
  | PILoadAddress (rt, lalabel) ->
      begin try
        let latarget = Hashtbl.find lbl_tbl lalabel in
        (* TODO: more assertion *)
        [gen_itype_pre 0b001111 reg_zero rt (latarget lsr 16);
         gen_itype_pre 0b001101 rt rt latarget]
      with Not_found ->
        failwith "label not found"
      end

let big32 = big_int_of_int 32
let bigU16 = shift_left_big_int unit_big_int 16
let bigS16 = shift_left_big_int unit_big_int 15
let bigMS16 = minus_big_int bigS16
let bigU32 = shift_left_big_int unit_big_int 32
let bigS32 = shift_left_big_int unit_big_int 31
let bigMS32 = minus_big_int bigS32
let sa_bound_check x =
  if not (le_big_int zero_big_int x && lt_big_int x big32) then
    raise (Assembly_error "Shift Amount is Too Large")
let s16_bound_check x =
  if not (le_big_int bigMS16 x && lt_big_int x bigS16) then
    raise (Assembly_error "Sign-Extended Immediate is Too Large")
let u16_bound_check x =
  if not (le_big_int zero_big_int x && lt_big_int x bigU16) then
    raise (Assembly_error "Zero-Extended Immediate is Too Large")
let s16_is_bounded x =
  le_big_int bigMS16 x && lt_big_int x bigS16
let u16_is_bounded x =
  le_big_int zero_big_int x && lt_big_int x bigU16
let u32_or_s32_bound_check x =
  if not (le_big_int bigMS32 x && lt_big_int x bigU32) then
    raise (Assembly_error "32-bit Immediate is Too Large")
let imm32_upper x =
  int_of_big_int (extract_big_int x 16 16)
let imm32_lower x =
  int_of_big_int (extract_big_int x 0 16)

let gen_rtype opcode rs rt rd sa funct =
  PIConst [|
    ((opcode lsl 2) lor (rs lsr 3)) land 255;
    ((rs lsl 5) lor rt) land 255;
    ((rd lsl 3) lor (sa lsr 2)) land 255;
    ((sa lsl 6) lor funct) land 255;
  |]
let gen_itype opcode rs rt imm =
  PIConst [|
    ((opcode lsl 2) lor (rs lsr 3)) land 255;
    ((rs lsl 5) lor rt) land 255;
    (imm lsr 8) land 255;
    imm land 255;
  |]
let gen_btype opcode rs rt l =
  PIBranch ([|
    ((opcode lsl 2) lor (rs lsr 3)) land 255;
    ((rs lsl 5) lor rt) land 255;
  |], l)

let translate opname operands =
  match opname, operands with
  (* SPECIAL instructions *)
  | "sll", [ORegister rd; ORegister rt; OImmediate sa] ->
      sa_bound_check sa;
      [gen_rtype 0b000000 reg_zero rt rd (int_of_big_int sa) 0b000000]
  | "srl", [ORegister rd; ORegister rt; OImmediate sa] ->
      sa_bound_check sa;
      [gen_rtype 0b000000 reg_zero rt rd (int_of_big_int sa) 0b000010]
  | "sra", [ORegister rd; ORegister rt; OImmediate sa] ->
      sa_bound_check sa;
      [gen_rtype 0b000000 reg_zero rt rd (int_of_big_int sa) 0b000011]
  | "sllv", [ORegister rd; ORegister rt; ORegister rs] ->
      [gen_rtype 0b000000 rs rt rd 0 0b000100]
  | "srlv", [ORegister rd; ORegister rt; ORegister rs] ->
      [gen_rtype 0b000000 rs rt rd 0 0b000110]
  | "srav", [ORegister rd; ORegister rt; ORegister rs] ->
      [gen_rtype 0b000000 rs rt rd 0 0b000111]
  | "jr", [ORegister rs] ->
      [gen_rtype 0b000000 rs reg_zero reg_zero 0 0b001000]
  | "jalr", [ORegister rs] ->
      [gen_rtype 0b000000 rs reg_zero reg_zero 0 0b001001]
  | "addu", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100001]
  | "subu", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100011]
  | "and", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100100]
  | "or", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100101]
  | "xor", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100110]
  | "nor", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b100111]
  | "slt", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b101010]
  | "sltu", [ORegister rd; ORegister rs; ORegister rt] ->
      [gen_rtype 0b000000 rs rt rd 0 0b101011]
  (* normal I-Type instructions *)
  | "j", [OLabelRef l] -> [PIJump (0b000010, l)]
  | "jal", [OLabelRef l] -> [PIJump (0b000011, l)]
  | "beq", [ORegister rs; ORegister rt; OLabelRef l] ->
      [gen_btype 0b000100 rs rt l]
  | "bne", [ORegister rs; ORegister rt; OLabelRef l] ->
      [gen_btype 0b000101 rs rt l]
  | "addiu", [ORegister rt; ORegister rs; OImmediate simm16] ->
      s16_bound_check simm16;
      [gen_itype 0b001001 rs rt (int_of_big_int simm16)]
  | "slti", [ORegister rt; ORegister rs; OImmediate simm16] ->
      s16_bound_check simm16;
      [gen_itype 0b001010 rs rt (int_of_big_int simm16)]
  | "sltiu", [ORegister rt; ORegister rs; OImmediate simm16] ->
      s16_bound_check simm16;
      [gen_itype 0b001011 rs rt (int_of_big_int simm16)]
  | "andi", [ORegister rt; ORegister rs; OImmediate uimm16] ->
      u16_bound_check uimm16;
      [gen_itype 0b001100 rs rt (int_of_big_int uimm16)]
  | "ori", [ORegister rt; ORegister rs; OImmediate uimm16] ->
      u16_bound_check uimm16;
      [gen_itype 0b001101 rs rt (int_of_big_int uimm16)]
  | "xori", [ORegister rt; ORegister rs; OImmediate uimm16] ->
      u16_bound_check uimm16;
      [gen_itype 0b001110 rs rt (int_of_big_int uimm16)]
  | "lui", [ORegister rt; OImmediate uimm16] ->
      u16_bound_check uimm16;
      [gen_itype 0b001111 reg_zero rt (int_of_big_int uimm16)]
  | "rrb", [ORegister rt] ->
      [gen_itype 0b011100 reg_zero rt 0]
  | "rsb", [ORegister rt] ->
      [gen_itype 0b011101 reg_zero rt 0]
  | "lw", [ORegister rt; ODisplacement (offset, base)] ->
      s16_bound_check offset;
      [gen_itype 0b100011 base rt (int_of_big_int offset)]
  | "sw", [ORegister rt; ODisplacement (offset, base)] ->
      s16_bound_check offset;
      [gen_itype 0b101011 base rt (int_of_big_int offset)]
  (* Floating-Point Instructions *)
  | "mfc1", [ORegister rt; OFRegister fs] ->
      [gen_rtype 0b010001 0b00000 rt fs 0b00000 0b000000]
  | "mtc1", [ORegister rt; OFRegister fs]
  | "mtc1", [OFRegister fs; ORegister rt] ->
      [gen_rtype 0b010001 0b00100 rt fs 0b00000 0b000000]
  | "bc1f", [OLabelRef l] ->
      [gen_btype 0b010001 0b01000 0b00000 l]
  | "bc1t", [OLabelRef l] ->
      [gen_btype 0b010001 0b01000 0b00001 l]
  | "add.s", [OFRegister fd; OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs fd 0b000000]
  | "sub.s", [OFRegister fd; OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs fd 0b000001]
  | "mul.s", [OFRegister fd; OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs fd 0b000010]
  | "div.s", [OFRegister fd; OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs fd 0b000011]
  | "sqrt.s", [OFRegister fd; OFRegister fs] ->
      [gen_rtype 0b010001 0b10000 0b00000 fs fd 0b000100]
  | "mov.s", [OFRegister fd; OFRegister fs] ->
      [gen_rtype 0b010001 0b10000 0b00000 fs fd 0b000110]
  | "c.eq.s", [OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs 0b00000 0b110010]
  | "c.olt.s", [OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs 0b00000 0b110100]
  | "c.ole.s", [OFRegister fs; OFRegister ft] ->
      [gen_rtype 0b010001 0b10000 ft fs 0b00000 0b110110]
  | "cvt.w.s", [OFRegister fd; OFRegister fs] ->
      [gen_rtype 0b010001 0b10000 0b00000 fs fd 0b100100]
  | "cvt.s.w", [OFRegister fd; OFRegister fs] ->
      [gen_rtype 0b010001 0b10100 0b00000 fs fd 0b100000]
  (* Pseudo-Instructions *)
  | "nop", [] ->
      [gen_rtype 0b000000 reg_zero reg_zero reg_zero 0 0b000000]
  | "mov", [ORegister rd; ORegister rs]
  | "move", [ORegister rd; ORegister rs] ->
      [gen_rtype 0b000000 rs reg_zero rd 0 0b100001]
  | "li", [ORegister rt; OImmediate imm32] ->
      if s16_is_bounded imm32 then
        [gen_itype 0b001001 reg_zero rt (int_of_big_int imm32)]
      else if u16_is_bounded imm32 then
        [gen_itype 0b001101 reg_zero rt (int_of_big_int imm32)]
      else (
        u32_or_s32_bound_check imm32;
        if imm32_lower imm32 = 0 then
          [gen_itype 0b001111 reg_zero rt (imm32_upper imm32)]
        else
          [gen_itype 0b001111 reg_zero rt (imm32_upper imm32);
           gen_itype 0b001101 rt rt (imm32_lower imm32)]
      )
  | "la", [ORegister rt; OLabelRef l] ->
      [PILoadAddress (rt, l)]
  | _, _ ->
      raise (Assembly_error "Unknown instruction name or operand type")

let translate_all tparser token =
  let rec translate_all buf =
    try
      match tparser token with
      | None -> buf
      | Some stmt ->
          try
            match stmt.loc_val with
            | SLabel l ->
                translate_all (PILabel l :: buf)
            | SInstruction (opname, operands) ->
                translate_all (List.rev_append (translate opname operands) buf)
          with Assembly_error e ->
            failwith (
              Printf.sprintf "line %d : %s"
                stmt.loc_start.Lexing.pos_lnum
                e
            )
    with
    | Lexing_error e ->
        failwith (
          Printf.sprintf "line %d : %s"
            e.loc_start.Lexing.pos_lnum
            e.loc_val
        )
    | Parsing_error e ->
        failwith (
          Printf.sprintf "line %d : %s"
            e.loc_start.Lexing.pos_lnum
            e.loc_val
        )
  in
  List.rev (translate_all [])
