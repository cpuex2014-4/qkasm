open Big_int
open Loc

exception Assembly_error of string
exception Lexing_error of string loc
exception Parsing_error of string loc

type operand =
  | OLabelRef of string
  | ORegister of int
  | OFRegister of int
  | OImmediate of big_int
  | ODisplacement of big_int * int

type statement_desc =
  | SLabel of string
  | SInstruction of string * operand list

type statement = statement_desc Loc.loc

type preinstruction =
  | PILabel of string
  | PIConst of int array
  | PIJump of int * string
  | PIBranch of int array * string
  | PILoadAddress of int * string

val generate_instruction :
  (string,int) Hashtbl.t -> int -> preinstruction -> int array list

val translate_all : ('a -> statement option) -> 'a -> preinstruction list
