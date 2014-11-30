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
let fprnames = [|
  "$f0"; "$f1"; "$f2"; "$f3"; "$f4"; "$f5"; "$f6"; "$f7";
  "$f8"; "$f9"; "$f10"; "$f11"; "$f12"; "$f13"; "$f14"; "$f15";
  "$f16"; "$f17"; "$f18"; "$f19"; "$f20"; "$f21"; "$f22"; "$f23";
  "$f24"; "$f25"; "$f26"; "$f27"; "$f28"; "$f29"; "$f30"; "$f31";
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
    let fmt = rs in
    let fs = rd in
    let ft = rt in
    let fd = sa in
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
        ) else if funct = 0b001001 && rt = 0 && rd = 31 && sa = 0 then (
          ("jalr", [gprnames.(rs)])
        ) else if funct = 0b001001 && rt = 0 && sa = 0 then (
          ("jalr", [gprnames.(rd); gprnames.(rs)])
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
      ) else if opcode = 0b010001 then (
        if fmt = 0b00000 && fd = 0 && funct = 0 then (
          ("mfc1", [gprnames.(rt); fprnames.(fs)])
        ) else if fmt = 0b00100 && fd = 0 && funct = 0 then (
          ("mtc1", [gprnames.(rt); fprnames.(fs)])
        ) else if fmt = 0b01000 && ft = 0 then (
          let bt = pc + 4 + (imm * 4) in
          Hashtbl.add lbl_tbl bt true;
          ("bc1f", [labelname bt])
        ) else if fmt = 0b01000 && ft = 1 then (
          let bt = pc + 4 + (imm * 4) in
          Hashtbl.add lbl_tbl bt true;
          ("bc1t", [labelname bt])
        ) else if fmt = 0b10000 then (
          if funct = 0b000000 then (
            ("add.s", [fprnames.(fd); fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b000001 then (
            ("sub.s", [fprnames.(fd); fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b000010 then (
            ("mul.s", [fprnames.(fd); fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b000011 then (
            ("div.s", [fprnames.(fd); fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b000100 && ft = 0 then (
            ("sqrt.s", [fprnames.(fd); fprnames.(fs)])
          ) else if funct = 0b000110 && ft = 0 then (
            ("mov.s", [fprnames.(fd); fprnames.(fs)])
          ) else if funct = 0b000111 && ft = 0 then (
            ("neg.s", [fprnames.(fd); fprnames.(fs)])
          ) else if funct = 0b110000 && fd = 0 then (
            ("c.f.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110001 && fd = 0 then (
            ("c.un.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110010 && fd = 0 then (
            ("c.eq.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110011 && fd = 0 then (
            ("c.ueq.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110100 && fd = 0 then (
            ("c.olt.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110101 && fd = 0 then (
            ("c.ult.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110110 && fd = 0 then (
            ("c.ole.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b110111 && fd = 0 then (
            ("c.ule.s", [fprnames.(fs); fprnames.(ft)])
          ) else if funct = 0b100100 && ft = 0 then (
            ("cvt.w.s", [fprnames.(fd); fprnames.(fs)])
          ) else (
            raise Unknown_instruction
          )
        ) else if fmt = 0b10100 then (
          if funct = 0b100000 && ft = 0 then (
            ("cvt.s.w", [fprnames.(fd); fprnames.(fs)])
          ) else (
            raise Unknown_instruction
          )
        ) else (
          raise Unknown_instruction
        )
      ) else if opcode = 0b011100 && rs = 0 && imm = 0 then (
        ("rrb", [gprnames.(rt)]);
      ) else if opcode = 0b011101 && rs = 0 && imm = 0 then (
        ("rsb", [gprnames.(rt)]);
      ) else if opcode = 0b100011 then (
        ("lw", [gprnames.(rt); string_of_disp gprnames.(rs) imm]);
      ) else if opcode = 0b101011 then (
        ("sw", [gprnames.(rt); string_of_disp gprnames.(rs) imm]);
      ) else if opcode = 0b110001 then (
        ("lwc1", [fprnames.(ft); string_of_disp gprnames.(rs) imm]);
      ) else if opcode = 0b111001 then (
        ("swc1", [fprnames.(ft); string_of_disp gprnames.(rs) imm]);
      ) else (
        raise Unknown_instruction
      )
    with Unknown_instruction ->
      (".long", [hex8_string_of_int opword])
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
