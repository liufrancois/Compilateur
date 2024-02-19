module Syntax = struct
  type ident = string
  type expr =
    | Int of  { value: int
             ; pos: Lexing.position }
    | Bool of { value: bool
             ; pos: Lexing.position }
    | Str of  { value: string
             ; pos: Lexing.position }
    | Call of { func: string
             ; args: expr list
             ; pos: Lexing.position }
             
  type lvalue =
    | LVar  of ident
    | LAddr of expr
    
    
  type instr =
    | Decl   of { var: ident
              ; expr: expr
              ; pos: Lexing.position }
    | Assign of { var: ident
              ; expr: expr
              ; pos: Lexing.position }
  and block = instr list

end

module IR = struct
  type ident = string
  type expr =
    | Int of int
    | Bool of int
    | Str of string
    | Call of string * expr list
  
  type lvalue =
    | LVar  of ident
    | LAddr of expr
    
  type instr =
    | Expr   of expr
    | Decl of ident
    | Assign of ident * expr
  and block = instr list
  
end

(*
type instr =
  | Return of expr
  | Assign of string * expr
  | Cond of expr * block * block
  | Loop of expr * block
  and block = instr list
  *)
