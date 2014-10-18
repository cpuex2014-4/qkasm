open Big_int

let check_signed_size l x =
  le_big_int (shift_left_big_int (minus_big_int unit_big_int) (l-1)) x &&
  lt_big_int x (shift_left_big_int unit_big_int (l-1))
let check_unsigned_size l x =
  le_big_int zero_big_int x &&
  lt_big_int x (shift_left_big_int unit_big_int l)

type gpr = int

type operand =
  | OLabelRef of string
  | ORegister of gpr
  | OImmediate of big_int
  | ODisplacement of big_int * gpr

type optype =
  | OptypeR | OptypeI | OptypeIB | OptypeJ
  | OptypeCustom of
      ((string, int) Hashtbl.t -> instruction -> int -> int array)

and instruction = {
  mutable optype : optype;
  mutable opcode : int;
  mutable funct : int;
  mutable rs : gpr;
  mutable rt : gpr;
  mutable rd : gpr;
  mutable shamt : int;
  mutable imm : big_int;
  mutable zero_ext_imm : bool;
  mutable label : string;
  mutable expand_length : int;
}

type operand_place =
  | SetOptype of optype
  | SetOpcode of int
  | SetFunct of int
  | RS
  | RT
  | RD
  | Shamt
  | Label
  | Displacement
  | Immediate
  | ZeroExtImm

let instruction_init () = {
  optype = OptypeI;
  opcode = 0;
  funct = 0;
  rs = 0;
  rt = 0;
  rd = 0;
  shamt = 0;
  imm = zero_big_int;
  zero_ext_imm = false;
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
    | (SetFunct funct :: places), _ ->
        inst.funct <- funct; process_operands places args
    | (RS :: places), (ORegister rs :: args) ->
        inst.rs <- rs; process_operands places args
    | (RT :: places), (ORegister rt :: args) ->
        inst.rt <- rt; process_operands places args
    | (RD :: places), (ORegister rd :: args) ->
        inst.rd <- rd; process_operands places args
    | (Shamt :: places), (OImmediate shamt :: args) ->
        assert (check_unsigned_size 5 shamt);
        inst.shamt <- int_of_big_int shamt; process_operands places args
    | (Label :: places), (OLabelRef label :: args) ->
        inst.label <- label; process_operands places args
    | (Displacement :: places), (ODisplacement (offset, base) :: args) ->
        inst.rs <- base;
        inst.imm <- offset;
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

let emit_rtype rs rt rd shamt funct =
  [|
    (rs lsr 3) land 255;
    ((rs lsl 5) lor rt) land 255;
    ((rd lsl 3) lor (shamt lsr 2)) land 255;
    ((shamt lsl 6) lor funct) land 255;
  |]

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

let emit_instruction labels pos inst =
  begin match inst.optype with
  | OptypeR ->
      emit_rtype inst.rs inst.rt inst.rd inst.shamt inst.funct
  | OptypeJ ->
      let jpos =
        try Hashtbl.find labels inst.label
        with Not_found ->
          raise (Failure ("label "^inst.label^" not found"))
      in
      emit_jtype inst.opcode jpos
  | OptypeIB ->
      let jpos =
        try Hashtbl.find labels inst.label
        with Not_found ->
          raise (Failure ("label "^inst.label^" not found"))
      in
      let posdiff = jpos - (pos + 1) in
      emit_itype inst.opcode inst.rs inst.rt posdiff
  | OptypeI ->
      begin if inst.zero_ext_imm then
        assert (check_unsigned_size 16 inst.imm)
      else
        assert (check_signed_size 16 inst.imm)
      end;
      let imm_int = int_of_big_int inst.imm in
      emit_itype inst.opcode inst.rs inst.rt imm_int
  | OptypeCustom f -> f labels inst pos
  end

let _ =
  List.iter (function (mnemonic, optype) ->
    Hashtbl.add optable mnemonic optype
  ) [
    "sll"    , [SetOptype OptypeR; SetFunct 0b000000; RD; RT; Shamt];
    "srl"    , [SetOptype OptypeR; SetFunct 0b000010; RD; RT; Shamt];
    "jr"     , [SetOptype OptypeR; SetFunct 0b001000; RS];
    "addu"   , [SetOptype OptypeR; SetFunct 0b100001; RD; RS; RT];
    "or"     , [SetOptype OptypeR; SetFunct 0b100101; RD; RS; RT];
    "slt"    , [SetOptype OptypeR; SetFunct 0b101010; RD; RS; RT];
    "j"      , [SetOptype OptypeJ; SetOpcode 0b000010; Label];
    "jal"    , [SetOptype OptypeJ; SetOpcode 0b000011; Label];
    "beq"    , [SetOptype OptypeIB; SetOpcode 0b000100; RS; RT; Label];
    "addiu"  , [SetOpcode 0b001001; RT; RS; Immediate; SetFunct 0b100001];
    "rrb"    , [SetOpcode 0b011100; RT];
    "rsb"    , [SetOpcode 0b011101; RT];
    "lw"     , [SetOpcode 0b101011; RT; Displacement];
    "sw"     , [SetOpcode 0b101011; RT; Displacement];
  ]
