open Statement

let rec read_statements lexbuf a =
  begin match Parser.statement_opt Lexer.token lexbuf with
  | Some x -> read_statements lexbuf (x::a)
  | None -> a
  end

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let stmts = List.rev (read_statements lexbuf []) in
  let rec expand_loop old_labels =
    let pos = ref 0 in
    let labels = Hashtbl.create 1042 in
    let need_update = ref false in
    List.iter (fun stmt ->
      begin match stmt with
      | SLabel lbl ->
          (* Format.eprintf "%s : %x\n" lbl !pos; *)
          Hashtbl.add labels lbl (!pos)
      | SInstruction inst ->
          need_update :=
            calculate_instruction_length old_labels !pos inst ||
            !need_update;
          pos := !pos + instruction_length inst
      end
    ) stmts;
    if !need_update then expand_loop labels else labels
  in
  let labels = expand_loop (Hashtbl.create 1042) in
  let pos = ref 0 in
  List.iter (fun stmt ->
    begin match stmt with
    | SLabel lbl ->
        (* Format.eprintf "%s : %x\n" lbl !pos; *)
        assert (Hashtbl.find labels lbl = !pos);
    | SInstruction inst ->
        let a = emit_instruction labels !pos inst in
        assert (List.length a = instruction_length inst);
        List.iter (fun aa ->
          (* Printf.printf "%x %x %x %x\n" aa.(0) aa.(1) aa.(2) aa.(3); *)
          for i = 0 to 3 do
            output_char stdout (char_of_int aa.(i))
          done
        ) a;
        pos := !pos + instruction_length inst
    end
  ) stmts

