{
  open Big_int
  open Lexing
  open Loc
  open Statement
  open Parser

  let big_int_of_hex_string s =
    let x = ref zero_big_int in
    String.iter (fun ch ->
      let digit =
        if '0' <= ch && ch <= '9' then int_of_char ch - int_of_char '0'
        else if 'A' <= ch && ch <= 'F'
          then int_of_char ch - int_of_char 'A' + 10
        else if 'a' <= ch && ch <= 'f'
          then int_of_char ch - int_of_char 'a' + 10
        else assert false
      in
      x := add_int_big_int digit (mult_int_big_int 16 !x)
    ) s;
    !x

  let regtable =
    let tbl = Hashtbl.create 357 in
    List.iter (function (k,v) ->
      Hashtbl.add tbl k v
    ) [
      "$zero", 0;
      "$at", 1;
      "$v0", 2;
      "$v1", 3;
      "$a0", 4;
      "$a1", 5;
      "$a2", 6;
      "$a3", 7;
      "$t0", 8;
      "$t1", 9;
      "$t2", 10;
      "$t3", 11;
      "$t4", 12;
      "$t5", 13;
      "$t6", 14;
      "$t7", 15;
      "$s0", 16;
      "$s1", 17;
      "$s2", 18;
      "$s3", 19;
      "$s4", 20;
      "$s5", 21;
      "$s6", 22;
      "$s7", 23;
      "$t8", 24;
      "$t9", 25;
      "$k0", 26;
      "$k1", 27;
      "$gp", 28;
      "$sp", 29;
      "$fp", 30;
      "$ra", 31;
    ];
    tbl
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'z' 'A'-'Z']
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_' '.']
let alnum = alpha | digit | '.'
let intstr = digit+ | ['+' '-'] digit+
let ident = alpha alnum*
let regident = '$' ident

rule token = parse
  | space+         { token lexbuf }
  | '\n'           { Lexing.new_line lexbuf; EOL }
  | "\\\n"         { Lexing.new_line lexbuf; token lexbuf }
  | '#'            { skip_line lexbuf; EOL }
  | ';'            { skip_line lexbuf; EOL }
  | "//"           { skip_line lexbuf; EOL }
  | ','            { COMMA }
  | ':'            { COLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ident as ident { IDENT ident }
  | "$f" (digit+ as n) { FREGNAME (int_of_string n) }
  | regident as regident {
      try
        REGNAME (Hashtbl.find regtable regident)
      with Not_found ->
        raise (Lexing_error {
          loc_val = Printf.sprintf "Unknown regname %s" regident;
          loc_start = lexeme_start_p lexbuf;
          loc_end = lexeme_end_p lexbuf
        })
    }
  | '$' (digit+ as n) { REGNAME (int_of_string n) }
  | intstr as n    { NUMBER (big_int_of_string n) }
  | "0x" (hexdigit+ as n) { NUMBER (big_int_of_hex_string n) }
  | eof            { EOF }
  | _ as ch        {
      raise (Lexing_error {
        loc_val = Printf.sprintf "Character '%c' cannot appear here" ch;
        loc_start = lexeme_start_p lexbuf;
        loc_end = lexeme_end_p lexbuf
      })
    }

and skip_line = parse
  | '\n' { Lexing.new_line lexbuf; () }
  | _ { skip_line lexbuf }
