{
  open Lexing
  open Parser

}

let newl = ['\n' '\r']
let space = ['\t' ' ' ]
let digit = [ '0'-'9']
let integer = '-'? digit+
let lowerletter = [ 'a'-'z' ]
 

rule instr = parse
  | "main"          { MAIN } 
  | newl+            {instr lexbuf  }
  | space+          { instr lexbuf }
  | lowerletter+  as i  {  IDENT i }
  | integer as n     {  INT (int_of_string n) }
  | '$' (digit+ as n)    {REG (int_of_string n) }
  | ','  { COMMA }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  | eof   { EOF }
  | _   { failwith "illegal character" }
