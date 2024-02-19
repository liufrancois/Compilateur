%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lstr Lvar
%token Ladd Lsub Lmul Ldiv Lmod Lxor Land Lnot

%token Pint Pbool Pstr Gint

%token Close Lsc

%token Lend
%token Lassign

%left Ladd Lsub
%left Lmul Ldiv

%start prog

%type <Ast.Syntax.expr> prog

%%

(*
%start block

%type <Ast.Syntax.block> block

%%
*)


prog:
  | e = expr; Lend { e }
;


(*
prog: (*block*)
  | Lend { [] }
  | i = instr; b = prog { i :: b};
*)

instr:
  | a = Lvar; Lassign; b = expr; Lsc { Assign {var = a; expr = b; pos = $startpos}}

expr:
| n = Lint {
  Int  { value = n ; pos = $startpos(n) }
}
| n = Lbool {
  Bool { value = n ; pos = $startpos(n) }
}
| n = Lstr {
  Str  { value = n ; pos = $startpos(n) }
}


| Pint ; a = expr; Close { Call {func = "puti"; args = [a]; pos = $startpos}}
| Pbool ; a = expr; Close { Call {func = "puti"; args = [a]; pos = $startpos}}
| Pstr ; a = expr; Close { Call {func = "puts"; args = [a]; pos = $startpos}}
| Gint ; Close { Call {func = "geti"; args = []; pos = $startpos}}


(*
| Pbool; a = expr; Close { Call {func = "putb"; args = [a]; pos = $startpos}}
;
| Pstr ; a = expr; Close { Call {func = "puts"; args = [a]; pos = $startpos}}
;
*)
| a = expr; Lmul; b = expr { Call {func = "mul_"; args = [a; b]; pos = $startpos}}
| a = expr; Ladd; b = expr { Call {func = "add_"; args = [a; b]; pos = $startpos}}
| a = expr; Lsub; b = expr { Call {func = "sub_"; args = [a; b]; pos = $startpos}}
| a = expr; Ldiv; b = expr { Call {func = "div_"; args = [a; b]; pos = $startpos}}
| a = expr; Lmod; b = expr { Call {func = "mod_"; args = [a; b]; pos = $startpos}}

| a = expr; Lxor; b = expr { Call {func = "xor_"; args = [a; b]; pos = $startpos}}
| a = expr; Land; b = expr { Call {func = "and_"; args = [a; b]; pos = $startpos}}
| Lnot; a = expr { Call {func = "not_"; args = [a]; pos = $startpos}}
;