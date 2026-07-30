open Syntax
open Interpeters
open Abstract_domains.Signs

let test_prog =
  Sequence(
    Sequence(
      Assign ("x", Const(0)), Assign("y", Const (-9))
    ),
    Sequence(
      Assign("z", BinaryOperation(Var "x", Add, Var "y")),
      Sequence(
        Filter(Comparison(Var "x",Smaller,Var "y")),
        Assign("z", BinaryOperation(Var "x", Add, Var "y"))
      )
    )
  )
  
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
  let risultato : SignInterp.state = SignInterp.eval test_prog in
  match risultato with
  | SignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (string_of_sign v)
      ) tbl