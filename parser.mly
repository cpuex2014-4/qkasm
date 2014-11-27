%{
  open Loc
  open Statement
%}
%token<string> IDENT
%token<int> REGNAME
%token<int> FREGNAME
%token COLON
%token COMMA
%token LPAREN RPAREN
%token<Big_int.big_int> NUMBER
%token EOL
%token EOF

%start<Statement.statement option> statement_opt
%type<Statement.statement> statement
%type<Statement.operand> operand

%%

statement_opt:
  | EOF { None }
  | EOL; s = statement_opt { s }
  | s = statement; EOL { Some s }

statement:
  | s = statement_desc {
      {
        loc_val = s;
        loc_start = $startpos;
        loc_end = $endpos
      }
    }
  | error {
      raise (Parsing_error {
        loc_val = "parse error";
        loc_start = $startpos;
        loc_end = $endpos
      })
    }

statement_desc:
  | l = IDENT; COLON { SLabel l }
  | opname = IDENT;
    operands = separated_list(COMMA, operand) {
      SInstruction (opname, operands)
    }

operand:
  | l = IDENT { OLabelRef l }
  | r = REGNAME { ORegister r }
  | fr = FREGNAME { OFRegister fr }
  | num = NUMBER { OImmediate num }
  | offset = NUMBER;
    LPAREN;
    base = REGNAME;
    RPAREN { ODisplacement (offset, base) }
  | error {
      raise (Parsing_error {
        loc_val = "parse error";
        loc_start = $startpos;
        loc_end = $endpos
      })
    }
