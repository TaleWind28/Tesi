open Syntax
open Interpeters
open Abstract_domains.Signs

let test_prog = 
  Sequence(
    Sequence(
      Assign (
        "x", Const(0)
      ),
      Assign(
        "y", Const (-9)
      )
    ),
    Sequence(
      Assign(
      "z",
      BinaryOperation(
        Var "x", Add, Var "y"
      )
      ),
      Sequence(
        Filter(
        (
          Boolean (Var "z" == Var "y")
          ),
        Skip
        ),
        Assign(
          "z",
          BinaryOperation(
            Var "x", Add, Var "y"
          )
        )
      )
      
    )
    
  )
  

(* Salva il risultato in una variabile di tipo state *)
let risultato : SignInterp.state = SignInterp.eval test_prog

let string_of_sign v =
  match v with
  | Pos -> "+"
  | Neg -> "-"
  | Zero -> "0"
  | PosZero -> ">=0"
  | NegZero -> "<=0"
  | NonZero -> "!=0"
  | SignTop -> "T (Top)"
  | SignBottom -> "_|_ (Bottom)"

let () =
  Hashtbl.iter (fun var v ->
    Printf.printf "%s : %s\n" var (string_of_sign v)
  ) risultato