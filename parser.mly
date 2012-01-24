%{
  open Ast
  
  let addLabel name = 
    if Hashtbl.mem addresses name then (failwith "already defined");
    Hashtbl.add addresses name !pc;
    Empty
      
%}

%token <int> REG
%token <int> INT
%token <string> IDENT
%token SEMICOLON COMMA COLON EOF MAIN


%start prog
%type <Ast.instruction list> prog

%%

prog:
MAIN COLON li = line* EOF  { li }

line:
    | s = IDENT COLON    { addLabel s}
    | i = instr SEMICOLON  {incr pc;  i}

instr: 
    | s = IDENT rd = REG COMMA rt = REG COMMA rs = REG { R(s,rs,rt,rd) }
    | s = IDENT rd = REG COMMA n = INT {Load(s, rd,n) }
    | s = IDENT addr = IDENT           {Jump(s,addr) }
    | s = IDENT rt = REG COMMA rs = REG COMMA  addr = IDENT {Branch(s,rs,rt,addr) }
;
      
