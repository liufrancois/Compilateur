open Ast.IR
open Mips

module Env = Map.Make(String)


type cinfo = { code: Mips.instr list
             ; env: Mips.loc Env.t
             ; fpo: int
             ; counter: int
             ; return: string }



let rec compile_expr e =
  match e with
  | Int  n -> [ Li (V0, n) ]
  | Bool n -> [ Li (V0, n) ]
  | Str  n -> [ La (V0, Lbl n) ]
  
  | Call (f, args) ->
    let ca = List.rev_map (fun a ->
      compile_expr a
      @ [ Addi (SP, SP, -4)
        ; Sw (V0, Mem (SP, 0)) ])
      args in
      List.flatten ca
      @ [ Jal f
      ; Addi (SP, SP, 4 * (List.length args)) ]


let rec compile_instr i info =
  match i with
  | Expr e ->
    { info with
      code = info.code
              @ compile_expr e }

(*
  | Assign (lv, e) ->
    { info with
       code = info.code
              @ compile_expr e info.env
              @ (match lv with
                 | LVar  v -> [ Sw (V0, Env.find v info.env) ]
                 | LAddr a -> []
                              @ [ Addi (SP, SP, -4)
                                ; Sw (V0, Mem (SP, 0)) ]
                              @ compile_expr a info.env
                              @ [ Lw (T0, Mem (SP, 0))
                                ; Addi (SP, SP, 4)
                                ; Sw (T0, Mem (V0, 0)) ]) }
*)
      
      
and compile_block b info =
  match b with
  | [] -> info
  | i :: r ->
      compile_block r (compile_instr i info)


  
let compile ir=
  
  { text = Baselib.builtins @ [Label "main";] @ (compile_expr ir)
  (*{ text = Baselib.builtins @ [Label "main";] @ (compile_instr ir env)*)
  ; data = [] }
