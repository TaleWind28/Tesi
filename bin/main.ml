open Syntax
open Interpeters
let test_prog = 
  Sequence (
    (* x parte in un intervallo positivo [1, 5] *)
    Assign ("x", Random (1, 5)),
    Sequence (
      (* y = -x + 10  (relazione ottagonale: x + y = 10) *)
      Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10)),
      (* z = x + 2    (relazione zonale: z - x = 2) *)
      Assign ("z", BinaryOperation (Var "x", Add, Const 2))
    )
  )

let () =
  let risultato = ExtendedSignInterp.eval test_prog in
  print_string "\nExtendedSigns\n";
  ExtendedSignInterp.outputStatePrinter risultato;;

  let risultato = SimpleSignInterp.eval test_prog in
  print_string "\nSimpleSign\n";
  
  SimpleSignInterp.outputStatePrinter risultato;;
  let risultato = StrangeSignInterp.eval test_prog in
  print_string "\nStrangeSign\n";
  
  StrangeSignInterp.outputStatePrinter risultato;;
  print_string "\nReducedSign\n";
  
  let risultato = SignInterp.eval test_prog in
  SignInterp.outputStatePrinter risultato;;
  
  print_string "\nSign\n";
  let risultato = SimplifiedSignInterp.eval test_prog in
  SimplifiedSignInterp.outputStatePrinter risultato;;
  print_string "\nIntervals\n";
  let risultato = IntervalInterp.eval test_prog in
  IntervalInterp.outputStatePrinter risultato;;
  print_string "\nZones\n";
  ZoneInterp.print_result (ZoneInterp.eval test_prog);
  print_string "\nOctagones\n";
  OcatagonInterp.print_result (OcatagonInterp.eval test_prog)



(* let test_completo =
  Sequence(
    Assign("a",Const(0)),
    Sequence(
      Assign("b",Const(10)),
      Sequence(
        Assign("c",Const(20)),
        Sequence(
          Assign("d",Const(-10)),
          Sequence(
            Assign("e",Const(10)),
                  If (
                  And (
                    Comparison (Var "a", Equals, Const 0),
                    Not (Comparison (Var "b", Bigger, Var "c"))
                  ),
                  Filter (Comparison (Var "d", Smaller, Const 5)),(* then *)
                  Assign ("f", UnaryOperation (Negation, Var "e"))(* else *)
                )
      )))))
   *)

(* let test_extraction test () =
  let vars = ZoneInterp.get_all_var test in
  let sorted_vars = List.sort_uniq compare vars in 
  Printf.printf
    "Variabili estratte (%d): [%s]\n"
    (List.length sorted_vars)
    (String.concat "; " sorted_vars)

let test_cmds = 
  Sequence(
    Assign("x",Const (-4)),
    Sequence(
    Assign("y",Const(3)),
    Filter(Comparison (Var "x", Equals ,Var("x") )
    )))
     *)
(* Invocazione *)
(* let () = test_extraction test_completo()
let () = test_extraction test_cmds () *)
(* let () =  *)
(* ZoneInterp.print_result (ZoneInterp.eval test_cmds);  *)
(* ZoneInterp.print_result (ZoneInterp.eval test_completo); *)
(* ZoneInterp.print_result (ZoneInterp.eval test_prog); *)
(* OcatagonInterp.print_result (OcatagonInterp.eval test_prog) *)
(* OcatagonInterp.print_result (OcatagonInterp.eval mine_test); *)