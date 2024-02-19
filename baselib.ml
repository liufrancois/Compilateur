open Ast.Syntax
open Mips

module Env = Map.Make(String)

let _types_ = Env.empty

type native = expr list -> expr




let builtins = 
  
  [
  
  Label "puti";
  Lw (A0, Mem (SP, 0));
  Li (V0, Syscall.print_int);
  Syscall;
  Jr RA;

  Label "puts";
  Lw (A0, Mem (SP, 0));
  Li (V0, Syscall.print_str);
  Syscall;
  Jr RA;
  
  Label "geti";
  Lw (A0, Mem (SP, 0));
  Li (V0, Syscall.read_int);
  Syscall;
  Jr RA;

  Label "add_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Add (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;

  Label "mul_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Mul (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;

  Label "sub_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Sub (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;

  Label "div_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Div (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;
  
  Label "mod_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Div (V0, T0, T1);
  Mfhi (V0);
  Sw (V0, Mem (SP, 0));
  Jr RA;
  

  Label "xor_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  Xor (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;

  Label "and_";
  Lw (T0, Mem (SP, 0));
  Lw (T1, Mem (SP, 4));
  And (V0, T0, T1);
  Sw (V0, Mem (SP, 0));
  Jr RA;

  Label "not_";
  Lw (T0, Mem (SP, 0));
  Xori (V0, T0, 1);
  Sw (V0, Mem (SP, 0));
  Jr RA;
  ]
