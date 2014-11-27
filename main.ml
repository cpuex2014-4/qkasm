open Lexing
open Loc
open Statement

let use_disasm = ref false
let files = ref []

let arg_spec = [
  "-disasm", Arg.Set use_disasm, "Use disassembler";
]
let anon_fun s = files := !files @ [s]
let usage_msg = "QKASM : Qnighy assembler for Kakeudon architecture"

let _ =
  Arg.parse arg_spec anon_fun usage_msg;
  if !use_disasm then
    Disasm.disasm ()
  else
    let channels =
      if !files = [] then
        ["<stdin>", from_channel stdin]
      else
        List.map (fun nam ->
          (nam, from_channel (open_in nam))
        ) !files
    in
    let pc = ref 0 in
    let lbl_tbl_g = Hashtbl.create 9876 in
    let translate_all =
      Statement.translate_all (Parser.statement_opt Lexer.token) in
    let pls = List.map (fun (fname, lexbuf) ->
      lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = fname };
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
      let preinstructions = translate_all lexbuf in
      let lbl_tbl_l = Hashtbl.create 9876 in
      List.iter (function
        | PILabel l -> Hashtbl.add lbl_tbl_l l !pc
        | PIConst _ | PIJump  _ | PIBranch _ -> pc := !pc + 4
        | PILoadAddress _ -> pc := !pc + 8
      ) preinstructions;
      (preinstructions, lbl_tbl_l)
    ) channels in
    pc := 0;
    List.iter (fun (preinstructions, lbl_tbl_l) ->
      List.iter (fun pi ->
        let a = generate_instruction lbl_tbl_g lbl_tbl_l !pc pi in
        List.iter (fun inst ->
          Printf.printf "%c%c%c%c"
            (char_of_int inst.(0))
            (char_of_int inst.(1))
            (char_of_int inst.(2))
            (char_of_int inst.(3));
          pc := !pc + 4
        ) a
      ) preinstructions
    ) pls
