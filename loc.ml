type 'a loc = {
  loc_val : 'a;
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

let loc_inherit l x = {
  loc_val = x;
  loc_start = l.loc_start;
  loc_end = l.loc_end
}
