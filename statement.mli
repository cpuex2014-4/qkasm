open Big_int

exception Assembly_error of string

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

val calculate_instruction_length :
  (string, int) Hashtbl.t -> int -> instruction -> bool
val instruction_length : instruction -> int

val emit_instruction :
  (string, int) Hashtbl.t -> int -> instruction -> int array list
