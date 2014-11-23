type 'a loc = {
  loc_val : 'a;
  loc_start : Lexing.position;
  loc_end : Lexing.position
}
