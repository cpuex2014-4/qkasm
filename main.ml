open Statement

let rec read_statements lexbuf a =
  begin match Parser.statement_opt Lexer.token lexbuf with
  | Some x -> read_statements lexbuf (x::a)
  | None -> a
  end

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let stmts = List.rev (read_statements lexbuf []) in
  let labels = Hashtbl.create 1042 in
  let pos = ref 0 in
  List.iter (fun stmt ->
    begin match stmt with
    | SLabel lbl ->
        Hashtbl.add labels lbl (!pos);
    | SInstruction _ ->
        pos := !pos + 1
    end
  ) stmts;
  pos := 0;
  List.iter (fun stmt ->
    begin match stmt with
    | SLabel _ -> ()
    | SInstruction inst ->
        let a = emit_instruction labels !pos inst in
        (* Printf.printf "%x %x %x %x\n" a.(0) a.(1) a.(2) a.(3); *)
        for i = 0 to 3 do
          output_char stdout (char_of_int a.(i))
        done;
        pos := !pos + 1
    end
  ) stmts

