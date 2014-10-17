{
  open Parser

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
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let intstr = digit+ | ['+' '-'] digit+
let ident = alpha alnum*
let regident = '$' ident

rule token = parse
  | space+         { token lexbuf }
  | '\n'           { Lexing.new_line lexbuf; EOL }
  | '#'            { skip_line lexbuf; EOL }
  | "//"           { skip_line lexbuf; EOL }
  | ','            { COMMA }
  | ':'            { COLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ident as ident { IDENT ident }
  | regident as regident { REGNAME (Hashtbl.find regtable regident) }
  | '$' (digit+ as n) { REGNAME (int_of_string n) }
  | intstr as n    { NUMBER (Big_int.big_int_of_string n) }
  | eof            { EOF }
  | _              { assert false }

and skip_line = parse
  | '\n' { Lexing.new_line lexbuf; () }
  | _ { skip_line lexbuf }
