open Syntax
open Interpeters
let test_prog =(
    (* Sequence(
      Assign("x",Const(5)),
      If (
        Comparison (Var "x", Bigger, Const 0), 
        Assign ("y", Const 1),
        Assign ("y", Const (-1))
      )
    ) *)
     Sequence (
      Assign ("x", Const (5)), 
      While (
        Comparison (Var "x", NotEquals, Const 0), 
        Assign ("x", Const 0))
      )
)


let () =
  let risultato = SignInterp.eval test_prog in
  print_string "Sign\t";
  SignInterp.outputStatePrinter risultato;;
  let risultato = SimpleSignInterp.eval test_prog in
  print_string "SimpleSign\t";
  SimpleSignInterp.outputStatePrinter risultato;;
  let risultato = StrangeSignInterp.eval test_prog in
  print_string "StrangeSign\t";
  StrangeSignInterp.outputStatePrinter risultato;;
  print_string "ReducedSign\t";
  let risultato = ReducedSignInterp.eval test_prog in
  ReducedSignInterp.outputStatePrinter risultato;;
  print_string "SimplifiedSign\t";
  let risultato = SimplifiedSignInterp.eval test_prog in
  SimplifiedSignInterp.outputStatePrinter risultato;;
  let risultato = IntervalInterp.eval test_prog in
  IntervalInterp.outputStatePrinter risultato;;

