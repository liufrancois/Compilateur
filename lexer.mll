{
  open Lexing
  open Parser

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let str = (alpha | num | '-' | '_')*
let identifier = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| num+ as n       { Lint (int_of_string n) }
| "true"  as n        { Lbool (bool_of_string n) }
| "false" as n      { Lbool (bool_of_string n) }
| '"'(str)+'"' as n    { Lstr (n) }
| identifier as n  { Lvar (n) }
(*
| "int.input(" as n         {Lint (read_int (int_of_string n)) }
| "bool.input(" as n         {Lbool (bool_of_string n) }
| "str.input(" as n         {Lstr (n) }
*)
| "printi("       { Pint }
| "printb("       { Pbool}
| "prints("       { Pstr }
| "scanfi("       { Gint }
| ")"             { Close }
| "="             { Lassign }
| ';'             { Lsc }
| '*'             { Lmul }
| '+'             { Ladd }
| '-'             { Lsub }
| '/'             { Ldiv }
| '%'             { Lmod }
| "||"            { Lxor }
| "&&"            { Land }
| '!'             { Lnot }
| _ as c          { raise (Error c) }
