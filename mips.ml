type reg =
  | Z0
  | V0
  | A0
  | SP
  | RA
  | T0
  | T1

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int = 5
  let read_str = 8
end

type label = string

type loc =
  | Lbl of label
  | Mem of reg * int

type instr =
  | Label of label
  | Syscall
  | Addi  of reg * reg * int
  | Add   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Mul   of reg * reg * reg
  | Div   of reg * reg * reg
  | Mfhi  of reg
  | Nor   of reg * reg * reg
  | Xor   of reg * reg * reg
  | Xori  of reg * reg * int
  | And   of reg * reg * reg
  | Andi  of reg * reg * int
  | Move  of reg * reg
  | Li    of reg * int
  | La    of reg * loc
  | Lw    of reg * loc
  | Sw    of reg * loc
  | Jal   of label
  | Jr    of reg

type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | Z0   -> "$zero"
  | V0   -> "$v0"
  | A0   -> "$a0"
  | SP   -> "$sp"
  | RA   -> "$ra"
  | T0   -> "$t0"
  | T1   -> "$t1"


let fmt_loc = function
  | Lbl (l)    -> l
  | Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)

let fmt_instr = function
  | Label (l)        -> ps "%s:" l
  | Li (r, i)        -> ps "  li %s, %d" (fmt_reg r) i
  | La (r, a)        -> ps "  la %s, %s" (fmt_reg r) (fmt_loc a)
  | Lw (r, a)        -> ps "  lw %s, %s" (fmt_reg r) (fmt_loc a)
  | Sw (r, a)        -> ps "  sw %s, %s" (fmt_reg r) (fmt_loc a)
  | Move (rd, rs)    -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) -> ps "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mfhi (rd)        -> ps "  mfhi %s" (fmt_reg rd)
  | Xori (rd, rs, i) -> ps "  xori %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Xor (rd, rs, rt) -> ps "  xor %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Nor (rd, rs, rt) -> ps "  nor %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | And (rd, rs, rt) -> ps "  and %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Andi (rt, rs, i) -> ps "  andi %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | Jal (l)          -> ps "  jal %s" l
  | Jr (r)           -> ps "  jr %s" (fmt_reg r)
  | Syscall          -> ps "  syscall"


let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  (*Printf.fprintf oc "  move $a0, $v0\n  li $v0, 1\n  syscall\n  jr $ra\n" ;*)
  Printf.fprintf oc "\n.data\n" ;
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
