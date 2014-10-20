open Big_int

exception Assembly_error of string

let check_signed_size l x =
  le_big_int (shift_left_big_int (minus_big_int unit_big_int) (l-1)) x &&
  lt_big_int x (shift_left_big_int unit_big_int (l-1))
let check_unsigned_size l x =
  le_big_int zero_big_int x &&
  lt_big_int x (shift_left_big_int unit_big_int l)

type gpr = int
type fpr = int

let reg_zero = (0 : gpr)
let reg_at = (1 : gpr)

type operand =
  | OLabelRef of string
  | ORegister of gpr
  | OFRegister of fpr
  | OImmediate of big_int
  | ODisplacement of big_int * gpr

type optype =
  | OptypeR | OptypeI | OptypeIB | OptypeJ | OptypeF
  | OptypeCustom of
      ((string, int) Hashtbl.t -> instruction -> int -> int) *
      ((string, int) Hashtbl.t -> instruction -> int -> int array list)

and instruction = {
  mutable optype : optype;
  mutable opcode : int;
  mutable fmt : int;
  mutable funct : int;
  mutable rs : gpr;
  mutable rt : gpr;
  mutable rd : gpr;
  mutable shamt : int;
  mutable imm : big_int;
  mutable zero_ext_imm : bool;
  mutable displacement : bool;
  mutable label : string;
  mutable expand_length : int;
}

type operand_place =
  | SetOptype of optype
  | SetOpcode of int
  | SetFmt of int
  | SetFunct of int
  | SetRS of int | SetRT of int
  | RS | RT | RD
  | FS | FT | FD
  | Shamt
  | Label
  | Displacement
  | Immediate
  | ZeroExtImm

let instruction_init () = {
  optype = OptypeI;
  opcode = 0;
  fmt = 0;
  funct = 0;
  rs = 0;
  rt = 0;
  rd = 0;
  shamt = 0;
  imm = zero_big_int;
  zero_ext_imm = false;
  displacement = false;
  label = "";
  expand_length = 1;
}

let process_operands places args =
  let inst = instruction_init () in
  let rec process_operands places args =
    begin match places, args with
    | [], [] -> ()
    | (SetOptype optype :: places), _ ->
        inst.optype <- optype; process_operands places args
    | (SetOpcode opcode :: places), _ ->
        inst.opcode <- opcode; process_operands places args
    | (SetFmt fmt :: places), _ ->
        inst.fmt <- fmt; process_operands places args
    | (SetFunct funct :: places), _ ->
        inst.funct <- funct; process_operands places args
    | (SetRS rs :: places), _ ->
        inst.rs <- rs; process_operands places args
    | (SetRT rt :: places), _ ->
        inst.rt <- rt; process_operands places args
    | (RS :: places), (ORegister rs :: args) ->
        inst.rs <- rs; process_operands places args
    | (RT :: places), (ORegister rt :: args) ->
        inst.rt <- rt; process_operands places args
    | (RD :: places), (ORegister rd :: args) ->
        inst.rd <- rd; process_operands places args
    | (FS :: places), (OFRegister fs :: args) ->
        inst.rs <- fs; process_operands places args
    | (FT :: places), (OFRegister ft :: args) ->
        inst.rt <- ft; process_operands places args
    | (FD :: places), (OFRegister fd :: args) ->
        inst.rd <- fd; process_operands places args
    | (Shamt :: places), (OImmediate shamt :: args) ->
        assert (check_unsigned_size 5 shamt);
        inst.shamt <- int_of_big_int shamt; process_operands places args
    | (Label :: places), (OLabelRef label :: args) ->
        inst.label <- label; process_operands places args
    | (Displacement :: places), (ODisplacement (offset, base) :: args) ->
        inst.rs <- base;
        inst.imm <- offset;
        inst.displacement <- true;
        process_operands places args
    | (Immediate :: places), (OImmediate imm :: args) ->
        inst.imm <- imm; process_operands places args
    | (ZeroExtImm :: places), _ ->
        inst.zero_ext_imm <- true; process_operands places args
    | _, [] ->
        raise (Failure "Too few operands")
    | [], _ ->
        raise (Failure "Too many operands")
    | _, _ ->
        raise (Failure "Wrong operand type")
    end
  in
  process_operands places args;
  inst

type statement =
  | SLabel of string
  | SInstruction of instruction

let optable = (Hashtbl.create 987 : (string, operand_place list) Hashtbl.t)

let translate_instruction opname args =
  let opinfo =
    try Hashtbl.find optable opname
    with Not_found ->
      raise (Failure ("Unknown opname "^opname))
  in
  process_operands opinfo args

let emit_655556 a b c d e f =
  [|
    ((a lsl 2) lor (b lsr 3)) land 255;
    ((b lsl 5) lor c) land 255;
    ((d lsl 3) lor (e lsr 2)) land 255;
    ((e lsl 6) lor f) land 255;
  |]

let emit_rtype rs rt rd shamt funct =
  emit_655556 0 rs rt rd shamt funct

let emit_jtype opcode jpos =
  [|
    ((opcode lsl 2) lor (jpos lsr 24)) land 255;
    (jpos lsr 16) land 255;
    (jpos lsr 8) land 255;
    jpos land 255;
  |]

let emit_itype opcode rs rt imm =
  [|
    ((opcode lsl 2) lor (rs lsr 3)) land 255;
    ((rs lsl 5) lor rt) land 255;
    (imm lsr 8) land 255;
    imm land 255;
  |]

let calculate_instruction_length_internal labels pos inst =
  begin match inst.optype with
  | OptypeR -> 1
  | OptypeJ -> 1
  | OptypeIB -> 1
  | OptypeI ->
      let medium =
        check_unsigned_size 32 inst.imm ||
        check_signed_size 32 inst.imm
      in
      let small =
        if inst.zero_ext_imm then
          check_unsigned_size 16 inst.imm
        else
          check_signed_size 16 inst.imm
      in
      if not medium then
        raise (Assembly_error "Immediate value too large")
      else if small || inst.opcode = 0b001111 then
        1
      else if inst.displacement then
        4
      else
        3
  | OptypeF -> 1
  | OptypeCustom (f, _) -> f labels inst pos
  end

let calculate_instruction_length labels pos inst =
  let old_len = inst.expand_length in
  let new_len = calculate_instruction_length_internal labels pos inst in
  inst.expand_length <- max old_len new_len;
  old_len < new_len

let instruction_length inst = inst.expand_length

let emit_instruction labels pos inst =
  begin match inst.optype with
  | OptypeR ->
      [emit_rtype inst.rs inst.rt inst.rd inst.shamt inst.funct]
  | OptypeJ ->
      let jpos =
        try Hashtbl.find labels inst.label
        with Not_found ->
          raise (Failure ("label "^inst.label^" not found"))
      in
      [emit_jtype inst.opcode jpos]
  | OptypeIB ->
      let jpos =
        try Hashtbl.find labels inst.label
        with Not_found ->
          raise (Failure ("label "^inst.label^" not found"))
      in
      let posdiff = jpos - (pos + 1) in
      [emit_itype inst.opcode inst.rs inst.rt posdiff]
  | OptypeI ->
      if inst.expand_length = 4 then
        let imm_int_u = int_of_big_int (shift_right_big_int inst.imm 16) in
        let imm_int_l = int_of_big_int (extract_big_int inst.imm 0 16) in
        [emit_itype 0b001111 0b00000 reg_at imm_int_u;
         emit_itype 0b001101 reg_at reg_at imm_int_l;
         emit_rtype inst.rs reg_at reg_at 0 0b100001;
         emit_itype inst.opcode reg_at inst.rt 0]
      else if inst.expand_length = 3 then
        let imm_int_u = int_of_big_int (shift_right_big_int inst.imm 16) in
        let imm_int_l = int_of_big_int (extract_big_int inst.imm 0 16) in
        [emit_itype 0b001111 0b00000 reg_at imm_int_u;
         emit_itype 0b001101 reg_at reg_at imm_int_l;
         emit_rtype inst.rs reg_at reg_at 0 inst.funct]
      else
        let imm_int = int_of_big_int inst.imm in
        [emit_itype inst.opcode inst.rs inst.rt imm_int]
  | OptypeF ->
      [emit_655556 0b010001 inst.fmt inst.rt inst.rs inst.rd inst.funct]
  | OptypeCustom (_, g) -> g labels inst pos
  end

let _ =
  List.iter (function (mnemonic, optype) ->
    Hashtbl.add optable mnemonic optype
  ) [
    "sll"    , [SetOptype OptypeR; SetFunct 0b000000; RD; RT; Shamt];
    "srl"    , [SetOptype OptypeR; SetFunct 0b000010; RD; RT; Shamt];
    "sra"    , [SetOptype OptypeR; SetFunct 0b000011; RD; RT; Shamt];
    "sllv"   , [SetOptype OptypeR; SetFunct 0b000100; RD; RT; RS];
    "srlv"   , [SetOptype OptypeR; SetFunct 0b000110; RD; RT; RS];
    "srav"   , [SetOptype OptypeR; SetFunct 0b000111; RD; RT; RS];
    "jr"     , [SetOptype OptypeR; SetFunct 0b001000; RS];
    "jalr"   , [SetOptype OptypeR; SetFunct 0b001001; RS];
    "mfhi"   , [SetOptype OptypeR; SetFunct 0b010000; RD];
    "mthi"   , [SetOptype OptypeR; SetFunct 0b010001; RS];
    "mflo"   , [SetOptype OptypeR; SetFunct 0b010010; RD];
    "mtlo"   , [SetOptype OptypeR; SetFunct 0b010011; RS];
    "mult"   , [SetOptype OptypeR; SetFunct 0b011000; RS; RT];
    "multu"  , [SetOptype OptypeR; SetFunct 0b011001; RS; RT];
    "div"    , [SetOptype OptypeR; SetFunct 0b011010; RS; RT];
    "divu"   , [SetOptype OptypeR; SetFunct 0b011011; RS; RT];
    "add"    , [SetOptype OptypeR; SetFunct 0b100000; RD; RS; RT];
    "addu"   , [SetOptype OptypeR; SetFunct 0b100001; RD; RS; RT];
    "sub"    , [SetOptype OptypeR; SetFunct 0b100010; RD; RS; RT];
    "subu"   , [SetOptype OptypeR; SetFunct 0b100011; RD; RS; RT];
    "and"    , [SetOptype OptypeR; SetFunct 0b100100; RD; RS; RT];
    "or"     , [SetOptype OptypeR; SetFunct 0b100101; RD; RS; RT];
    "xor"    , [SetOptype OptypeR; SetFunct 0b100110; RD; RS; RT];
    "nor"    , [SetOptype OptypeR; SetFunct 0b100111; RD; RS; RT];
    "slt"    , [SetOptype OptypeR; SetFunct 0b101010; RD; RS; RT];
    "sltu"   , [SetOptype OptypeR; SetFunct 0b101011; RD; RS; RT];
    "j"      , [SetOptype OptypeJ; SetOpcode 0b000010; Label];
    "jal"    , [SetOptype OptypeJ; SetOpcode 0b000011; Label];
    "beq"    , [SetOptype OptypeIB; SetOpcode 0b000100; RS; RT; Label];
    "bne"    , [SetOptype OptypeIB; SetOpcode 0b000101; RS; RT; Label];
    "blez"   , [SetOptype OptypeIB; SetOpcode 0b000101; RS; Label];
    "bgtz"   , [SetOptype OptypeIB; SetOpcode 0b000110; RS; Label];
    "addi"   , [SetOpcode 0b001000; RT; RS; Immediate; SetFunct 0b100000];
    "addiu"  , [SetOpcode 0b001001; RT; RS; Immediate; SetFunct 0b100001];
    "slti"   , [SetOpcode 0b001010; RT; RS; Immediate; SetFunct 0b101010];
    "sltiu"  , [SetOpcode 0b001011; RT; RS; Immediate; SetFunct 0b101011];
    "andi"   , [SetOpcode 0b001100; RT; RS; Immediate; SetFunct 0b100100;
                ZeroExtImm];
    "ori"    , [SetOpcode 0b001101; RT; RS; Immediate; SetFunct 0b100101;
                ZeroExtImm];
    "xori"   , [SetOpcode 0b001110; RT; RS; Immediate; SetFunct 0b100110;
                ZeroExtImm];
    "lui"    , [SetOpcode 0b001111; RT; RS; Immediate];
    "mov"    , [SetOpcode 0b001101; RT; RS];
    "rrb"    , [SetOpcode 0b011100; RT];
    "rsb"    , [SetOpcode 0b011101; RT];
    "lb"     , [SetOpcode 0b100000; RT; Displacement];
    "lh"     , [SetOpcode 0b100001; RT; Displacement];
    "lwl"    , [SetOpcode 0b100010; RT; Displacement];
    "lw"     , [SetOpcode 0b100011; RT; Displacement];
    "lbu"    , [SetOpcode 0b100100; RT; Displacement];
    "lhu"    , [SetOpcode 0b100101; RT; Displacement];
    "lwr"    , [SetOpcode 0b100110; RT; Displacement];
    "sb"     , [SetOpcode 0b101000; RT; Displacement];
    "sh"     , [SetOpcode 0b101001; RT; Displacement];
    "swl"    , [SetOpcode 0b101010; RT; Displacement];
    "sw"     , [SetOpcode 0b101011; RT; Displacement];
    "swr"    , [SetOpcode 0b101110; RT; Displacement];

    "bc1f"   , [SetOptype OptypeIB; SetOpcode 0b010001;
                SetRS 0b01000; SetRT 0b00000; Label;];
    "bc1t"   , [SetOptype OptypeIB; SetOpcode 0b010001;
                SetRS 0b01000; SetRT 0b00000; Label;];
    "mfc1"   , [SetOptype OptypeF; SetFmt  0; SetFunct 0b000000; RT; FS];
    "mtc1"   , [SetOptype OptypeF; SetFmt  4; SetFunct 0b000000; RT; FS];
    "add.s"  , [SetOptype OptypeF; SetFmt 16; SetFunct 0b000000; FD; FT; FS];
    "sub.s"  , [SetOptype OptypeF; SetFmt 16; SetFunct 0b000001; FD; FT; FS];
    "mul.s"  , [SetOptype OptypeF; SetFmt 16; SetFunct 0b000010; FD; FT; FS];
    "div.s"  , [SetOptype OptypeF; SetFmt 16; SetFunct 0b000011; FD; FT; FS];
    "mov.s"  , [SetOptype OptypeF; SetFmt 16; SetFunct 0b000110; FD; FS];
    "cvt.s.w", [SetOptype OptypeF; SetFmt 20; SetFunct 0b100000; FD; FS];
    "cvt.w.s", [SetOptype OptypeF; SetFmt 16; SetFunct 0b100100; FD; FS];
    "c.eq.s" , [SetOptype OptypeF; SetFmt 16; SetFunct 0b110010; FS; FT];
    "c.olt.s", [SetOptype OptypeF; SetFmt 16; SetFunct 0b110100; FS; FT];
    "c.ole.s", [SetOptype OptypeF; SetFmt 16; SetFunct 0b110110; FS; FT];
  ]

let _ =
  Hashtbl.add optable "li" [
    SetOptype (OptypeCustom ((fun labels inst pos ->
      if check_signed_size 32 inst.imm ||
         check_unsigned_size 32 inst.imm then
        1
      else 2
    ), (fun labels inst pos ->
      if check_signed_size 32 inst.imm then
        [emit_itype 0b001001 reg_zero inst.rt (int_of_big_int inst.imm)]
      else if check_unsigned_size 32 inst.imm then
        [emit_itype 0b001101 reg_zero inst.rt (int_of_big_int inst.imm)]
      else
        let imm_int_u = int_of_big_int (shift_right_big_int inst.imm 16) in
        let imm_int_l = int_of_big_int (extract_big_int inst.imm 0 16) in
        [emit_itype 0b001111 0 inst.rt imm_int_u;
         emit_itype 0b001101 inst.rt inst.rt imm_int_l]
    )));
    RT;
    Immediate
  ]
