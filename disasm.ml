exception Unknown_instruction

let input_int32_be chnl =
  let b0 = int_of_char (input_char chnl) in
  let b1 = int_of_char (input_char chnl) in
  let b2 = int_of_char (input_char chnl) in
  let b3 = int_of_char (input_char chnl) in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let gprnames = [|
  "$zero"; "$at"; "$v0"; "$v1"; "$a0"; "$a1"; "$a2"; "$a3";
  "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7";
  "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7";
  "$t8"; "$t9"; "$k0"; "$k1"; "$gp"; "$sp"; "$fp"; "$ra";
|]

let labelname x = Printf.sprintf "L%08x" x
let hex4_string_of_int x = Printf.sprintf "0x%04x" x
let hex8_string_of_int x = Printf.sprintf "0x%08x" x
let string_of_disp base offset =
  Printf.sprintf "%d(%s)" offset base

let disasm () =
  let lbl_tbl = Hashtbl.create 1144 in
  let insts = Hashtbl.create 4344 in
  let rec loop1 pc =
    let opword = input_int32_be stdin in
    let opcode = (opword lsr 26) in
    let rs = (opword lsr 21) land 31 in
    let rt = (opword lsr 16) land 31 in
    let rd = (opword lsr 11) land 31 in
    let sa = (opword lsr 6) land 31 in
    let funct = opword land 63 in
    let uimm = opword land ((1 lsl 16) - 1) in
    let imm =
      if uimm < 32768 then uimm else uimm-65536 in
    let long_imm = opword land ((1 lsl 26) -1) in
    let opdata = begin try
      if opcode = 0b000000 then (
        if funct = 0b000000 && rs = 0 then (
          ("sll", [gprnames.(rd); gprnames.(rt); string_of_int sa])
        ) else if funct = 0b000010 && rs = 0 then (
          ("srl", [gprnames.(rd); gprnames.(rt); string_of_int sa])
        ) else if funct = 0b000011 && rs = 0 then (
          ("sra", [gprnames.(rd); gprnames.(rt); string_of_int sa])
        ) else if funct = 0b000100 && sa = 0 then (
          ("sllv", [gprnames.(rd); gprnames.(rt); gprnames.(rs)])
        ) else if funct = 0b000110 && sa = 0 then (
          ("srlv", [gprnames.(rd); gprnames.(rt); gprnames.(rs)])
        ) else if funct = 0b000111 && sa = 0 then (
          ("srav", [gprnames.(rd); gprnames.(rt); gprnames.(rs)])
        ) else if funct = 0b001000 && rt = 0 && rd = 0 && sa = 0 then (
          ("jr", [gprnames.(rs)])
        ) else if funct = 0b001001 && rt = 0 && rd = 0 && sa = 0 then (
          ("jalr", [gprnames.(rs)])
        ) else if funct = 0b100001 && sa = 0 then (
          ("addu", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b100011 && sa = 0 then (
          ("subu", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b100100 && sa = 0 then (
          ("and", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b100101 && sa = 0 then (
          ("or", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b100110 && sa = 0 then (
          ("xor", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b100111 && sa = 0 then (
          ("nor", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b101010 && sa = 0 then (
          ("slt", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else if funct = 0b101011 && sa = 0 then (
          ("sltu", [gprnames.(rd); gprnames.(rs); gprnames.(rt)])
        ) else (
          raise Unknown_instruction
        )
      ) else if opcode = 0b000001 then (
        raise Unknown_instruction
      ) else if opcode = 0b000010 then (
        let bt = long_imm lsl 2 in
        Hashtbl.add lbl_tbl bt true;
        ("j", [labelname bt])
      ) else if opcode = 0b000011 then (
        let bt = long_imm lsl 2 in
        Hashtbl.add lbl_tbl bt true;
        ("jal", [labelname bt])
      ) else if opcode = 0b000100 then (
        let bt = pc + 4 + (imm * 4) in
        Hashtbl.add lbl_tbl bt true;
        ("beq", [gprnames.(rs); gprnames.(rt); labelname bt])
      ) else if opcode = 0b000101 then (
        let bt = pc + 4 + (imm * 4) in
        Hashtbl.add lbl_tbl bt true;
        ("bne", [gprnames.(rs); gprnames.(rt); labelname bt])
      ) else if opcode = 0b001001 then (
        ("addiu", [gprnames.(rt); gprnames.(rs); string_of_int imm]);
      ) else if opcode = 0b001010 then (
        ("slti", [gprnames.(rt); gprnames.(rs); string_of_int imm]);
      ) else if opcode = 0b001011 then (
        ("sltiu", [gprnames.(rt); gprnames.(rs); string_of_int imm]);
      ) else if opcode = 0b001100 then (
        ("andi", [gprnames.(rt); gprnames.(rs); hex4_string_of_int uimm]);
      ) else if opcode = 0b001101 then (
        ("ori", [gprnames.(rt); gprnames.(rs); hex4_string_of_int uimm]);
      ) else if opcode = 0b001110 then (
        ("xori", [gprnames.(rt); gprnames.(rs); hex4_string_of_int uimm]);
      ) else if opcode = 0b001111 && rs = 0 then (
        ("lui", [gprnames.(rt); hex4_string_of_int uimm]);
      ) else if opcode = 0b100011 then (
        ("lw", [gprnames.(rt); string_of_disp gprnames.(rs) imm]);
      ) else if opcode = 0b101011 then (
        ("sw", [gprnames.(rt); string_of_disp gprnames.(rs) imm]);
      ) else (
        raise Unknown_instruction
      )
    with Unknown_instruction ->
      (".data", [hex8_string_of_int opword])
    end in
    Hashtbl.add insts pc opdata;
    loop1 (pc + 4)
  in
  begin try loop1 0 with End_of_file -> () end;
  let rec loop2 pc =
    (if Hashtbl.mem lbl_tbl pc then
      Printf.printf "%s:\n" (labelname pc)
    );
    let (opname, operands) = Hashtbl.find insts pc in
    Printf.printf "\t%s\t%s\n" opname (String.concat ", " operands);
    loop2 (pc +4)
  in
  begin try loop2 0 with Not_found -> () end
