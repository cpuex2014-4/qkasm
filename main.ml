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
      let exports = ref [] in
      List.iter (function
        | PIAlign a -> while !pc mod a != 0 do pc := !pc + 1 done
        | PIExport l -> exports := l :: !exports
        | PILabel l -> Hashtbl.add lbl_tbl_l l !pc
        | PIConst _ -> pc := !pc + 1
        | PIJump _ -> pc := !pc + 4
        | PIBranchLower _ -> pc := !pc + 2
        | PILoadAddress _ -> pc := !pc + 8
        | PIConstLabelRef _ -> pc := !pc + 4
      ) preinstructions;
      List.iter (fun ex ->
        try
          Hashtbl.add lbl_tbl_g ex (Hashtbl.find lbl_tbl_l ex)
        with Not_found ->
          failwith (Printf.sprintf "label %s not found" ex)
      ) !exports;
      (preinstructions, lbl_tbl_l)
    ) channels in
    pc := 0;
    List.iter (fun (preinstructions, lbl_tbl_l) ->
      List.iter (fun pi ->
        let a = generate_instruction lbl_tbl_g lbl_tbl_l !pc pi in
        List.iter (fun ch ->
          Printf.printf "%c" (char_of_int ch);
          pc := !pc + 1
        ) a
      ) preinstructions
    ) pls
