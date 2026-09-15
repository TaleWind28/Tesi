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

  
let test_completo =
  If (
    And (
      Comparison (Var "a", Equals, Const 0),
      Not (Comparison (Var "b", Bigger, Var "c"))
    ),
    Filter (Comparison (Var "d", Smaller, Const 5)),
    Assign ("e", UnaryOperation (Negation, Var "f"))
  )

let test_extraction test () =
  let vars = ZoneInterp.get_all_var test in
  let sorted_var = List.sort_uniq compare vars in 
  Printf.printf
    "Variabili estratte (%d): [%s]\n"
    (List.length sorted_var)
    (String.concat "; " sorted_var)

let test_cmds = 
  Sequence(
    Assign("x",Const 4),
    Sequence(
    Assign("y",Const(-3)),
    Assign("x",BinaryOperation(Var("x"),Add,Var("y"))
    )))
  
(* Invocazione *)
let () = test_extraction test_completo()
let () = test_extraction test_cmds ()
(* let () =  *)
let () = print_string "Dominio delle Zone: \n";
ZoneInterp.print_result (ZoneInterp.eval test_cmds); 
(* let () = ZoneInterp.print_result (ZoneInterp.eval test_completo) *)