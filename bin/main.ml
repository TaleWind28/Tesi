open Syntax
open Interpeters
let test_prog =(
       Sequence (
          Assign ("x", Const (-5)),
          While (
            Comparison (Var "x", Smaller, Const 0),
            Assign ("x", BinaryOperation (Var "x", Add, Const 1))
            )
          )
)


let () =
  let risultato = ExtendedSignInterp.eval test_prog in
  print_string "ExtendedSigns\t";
  ExtendedSignInterp.outputStatePrinter risultato;;
  let risultato = SimpleSignInterp.eval test_prog in
  print_string "SimpleSign\t";
  SimpleSignInterp.outputStatePrinter risultato;;
  let risultato = StrangeSignInterp.eval test_prog in
  print_string "StrangeSign\t";
  StrangeSignInterp.outputStatePrinter risultato;;
  print_string "ReducedSign\t";
  let risultato = SignInterp.eval test_prog in
  SignInterp.outputStatePrinter risultato;;
  print_string "Sign\t\t";
  let risultato = SimplifiedSignInterp.eval test_prog in
  SimplifiedSignInterp.outputStatePrinter risultato;;
  print_string "Intervals\t";
  let risultato = IntervalInterp.eval test_prog in
  IntervalInterp.outputStatePrinter risultato;;

