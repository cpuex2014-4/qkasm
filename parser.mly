%{
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
  | ident = IDENT; COLON { SLabel ident }
  | ident = IDENT;
    operands = separated_list(COMMA, operand)
    { SInstruction (translate_instruction ident operands) }

operand:
  | ident = IDENT { OLabelRef ident }
  | ident = REGNAME { ORegister ident }
  | ident = FREGNAME { OFRegister ident }
  | num = NUMBER { OImmediate num }
  | num = NUMBER;
    LPAREN;
    ident = REGNAME;
    RPAREN { ODisplacement (num, ident) }
