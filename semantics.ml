open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position




let rec analyze_expr expr env =
  match expr with
  | Syntax.Int  n -> Int n.value(*, env*)
  | Syntax.Bool n -> (if n.value == false then Int 0 else Int 1)(*, env*)
  | Syntax.Str  n -> Str n.value(*, env*)
  | Syntax.Call n -> let args = List.map (fun arg -> (analyze_expr arg env))
      n.args in
      Call (n.func, args)

    (*
    let args, env' = List.split (List.map (fun arg -> analyze_expr arg env) n.args) in
      if n.func = "puti" then
        Call ("%puti", n.args)
      else
        Call (n.func, n.args)
        *)

(*
let analyze_instr instr env =
  match instr with
  | Syntax.Assign a ->
     let ae, et = analyze_expr a.expr env in
     Assign (a.var, ae), Env.add a.var ae env

let rec analyze_block block env =
match block with
| [] -> [], env
| instr :: rest ->
  let ai, new_env = analyze_instr instr env in
  let rest_instrs, final_env = analyze_block rest new_env in
  ai :: rest_instrs, final_env

*)
let analyze parsed =
  analyze_expr parsed Baselib._types_
  (*analyze_block parsed Baselib._types_*)

