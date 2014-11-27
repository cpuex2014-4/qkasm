open Loc
open Statement

let use_disasm = ref false

let arg_spec = [
  "-disasm", Arg.Set use_disasm, "Use disassembler";
]
let anon_fun _ = failwith "Unknown Command Line Argument"
let usage_msg = "QKASM : Qnighy assembler for Kakeudon architecture"

let _ =
  Arg.parse arg_spec anon_fun usage_msg;
  if !use_disasm then
    Disasm.disasm ()
  else
    let lexbuf = Lexing.from_channel stdin in
    let translate_all =
      Statement.translate_all (Parser.statement_opt Lexer.token) in
    let preinstructions = translate_all lexbuf in
    let lbl_tbl = Hashtbl.create 9876 in
    let pc = ref 0 in
    List.iter (function
      | PILabel l -> Hashtbl.add lbl_tbl l !pc
      | PIConst _ | PIJump  _ | PIBranch _ -> pc := !pc + 4
      | PILoadAddress _ -> pc := !pc + 8
    ) preinstructions;
    pc := 0;
    List.iter (fun pi ->
      let a = generate_instruction lbl_tbl !pc pi in
      List.iter (fun inst ->
        Printf.printf "%c%c%c%c"
          (char_of_int inst.(0))
          (char_of_int inst.(1))
          (char_of_int inst.(2))
          (char_of_int inst.(3));
        pc := !pc + 4
      ) a
    ) preinstructions
