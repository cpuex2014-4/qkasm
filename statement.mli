open Big_int
open Loc

exception Assembly_error of string
exception Lexing_error of string loc
exception Parsing_error of string loc

val show_line : bool ref

type operand =
  | OLabelRef of string
  | ORegister of int
  | OFRegister of int
  | OImmediate of big_int
  | ODisplacement of big_int * int

type statement_desc =
  | SLabel of string
  | SInstruction of string * operand list

type statement = statement_desc loc

type preinstruction =
  | PIAlign of int
  | PIExport of string
  | PILabel of string
  | PIConst of int
  | PIJump of int * string
  | PIBranchLower of string
  | PILoadAddress of int * string
  | PIConstLabelRef of string
  | PIPrintLocation of unit loc


val generate_instruction :
  (string,int) Hashtbl.t ->
  (string,int) Hashtbl.t -> int -> preinstruction -> int list

val translate_all : ('a -> statement option) -> 'a -> preinstruction list
