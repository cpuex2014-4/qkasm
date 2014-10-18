open Big_int

type gpr = int

type operand =
  | OLabelRef of string
  | ORegister of gpr
  | OImmediate of big_int
  | ODisplacement of big_int * gpr

type instruction

type statement =
  | SLabel of string
  | SInstruction of instruction

val translate_instruction : string -> operand list -> instruction

val emit_instruction :
  (string, int) Hashtbl.t -> int -> instruction -> int array
