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
  let test_prog = 
    Sequence(Filter(Comparison(Const 5, Smaller, Const 2)),Skip)

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
  OctagonInterp.print_result (OctagonInterp.eval test_prog)