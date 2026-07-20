open Syntax
open Abstract_domains
open Interpeters
 
(* ------------------------------------------------------------------ *)
(* Stato di test: associa nomi di variabili a valori nel dominio Signs *)
(* ------------------------------------------------------------------ *)
let test_st : (string, Signs.t) Hashtbl.t = Hashtbl.create 10
 
let () =
  Hashtbl.add test_st "x" Signs.Pos;       (* positivo stretto *)
  Hashtbl.add test_st "y" Signs.Neg;       (* negativo stretto *)
  Hashtbl.add test_st "z" Signs.Zero;      (* zero *)
  Hashtbl.add test_st "w" Signs.PosZero;   (* >= 0 *)
  Hashtbl.add test_st "k" Signs.NegZero;   (* <= 0 *)
  Hashtbl.add test_st "n" Signs.NonZero;   (* != 0, segno ignoto *)
  Hashtbl.add test_st "t" Signs.SignTop;   (* nessuna informazione *)
  Hashtbl.add test_st "b" Signs.SignBottom (* variabile irraggiungibile *)
 
let sign_to_string = function
  | Signs.SignTop    -> "Top (sconosciuto)"
  | Signs.Pos        -> "Positivo"
  | Signs.Neg        -> "Negativo"
  | Signs.Zero       -> "Zero"
  | Signs.SignBottom -> "Bottom (errore/irraggiungibile)"
  | Signs.PosZero    -> "Positivo o Zero"
  | Signs.NegZero    -> "Negativo o Zero"
  | Signs.NonZero    -> "Diverso da Zero"
 
(* ------------------------------------------------------------------ *)
(* Test tramite espressioni: (descrizione, espressione)                *)
(* ------------------------------------------------------------------ *)
let sumtests = [
  (* ---------- SUM ---------- *)
  "Sum: Pos + Pos",             BinaryOperation (Var "x", Add, Var "x");
  "Sum: Pos + Neg",             BinaryOperation (Var "x", Add, Var "y");
  "Sum: Neg + Neg",             BinaryOperation (Var "y", Add, Var "y");
  "Sum: Pos + Zero",            BinaryOperation (Var "x", Add, Var "z");
  "Sum: Zero + Zero",           BinaryOperation (Var "z", Add, Var "z");
  "Sum: PosZero + PosZero",     BinaryOperation (Var "w", Add, Var "w");
  "Sum: NegZero + NegZero",     BinaryOperation (Var "k", Add, Var "k");
  "Sum: PosZero + NegZero",     BinaryOperation (Var "w", Add, Var "k");
  "Sum: PosZero + Pos",         BinaryOperation (Var "w", Add, Var "x");
  "Sum: PosZero + Neg",         BinaryOperation (Var "w", Add, Var "y");
  "Sum: NegZero + Pos",         BinaryOperation (Var "k", Add, Var "x");
  "Sum: NegZero + Neg",         BinaryOperation (Var "k", Add, Var "y");
  "Sum: NonZero + Pos",         BinaryOperation (Var "n", Add, Var "x");
  "Sum: NonZero + Zero",        BinaryOperation (Var "n", Add, Var "z");
  "Sum: NonZero + NonZero",     BinaryOperation (Var "n", Add, Var "n");
  "Sum: Top + Pos",             BinaryOperation (Var "t", Add, Var "x");
  "Sum: Bottom + Pos",          BinaryOperation (Var "b", Add, Var "x");
  "Sum: 10 + (-20)",            BinaryOperation (Const 10, Add, Const (-20));
]

let subtest = [
  (* ---------- SUB ---------- *)
  "Sub: Pos - Neg",             BinaryOperation (Var "x", Sub, Var "y");
  "Sub: Pos - Pos (stessa var)",BinaryOperation (Var "x", Sub, Var "x");
  "Sub: 10 - 20",               BinaryOperation (Const 10, Sub, Const 20);
  "Sub: PosZero - PosZero",     BinaryOperation (Var "w", Sub, Var "w");
  "Sub: Zero - Neg",            BinaryOperation (Var "z", Sub, Var "y");
]

let multest = [
(* ---------- MUL ---------- *)
  "Mul: Pos * Pos",             BinaryOperation (Var "x", Mul, Var "x");
  "Mul: Pos * Neg",             BinaryOperation (Var "x", Mul, Var "y");
  "Mul: Neg * Neg",             BinaryOperation (Var "y", Mul, Var "y");
  "Mul: Pos * Zero",            BinaryOperation (Var "x", Mul, Var "z");
  "Mul: PosZero * Neg",         BinaryOperation (Var "w", Mul, Var "y");
  "Mul: NegZero * Pos",         BinaryOperation (Var "k", Mul, Var "x");
  "Mul: NonZero * Zero",        BinaryOperation (Var "n", Mul, Var "z");
  "Mul: NonZero * NonZero",     BinaryOperation (Var "n", Mul, Var "n");
  "Mul: Top * Zero",            BinaryOperation (Var "t", Mul, Var "z");
  "Mul: Bottom * Pos",          BinaryOperation (Var "b", Mul, Var "x");

]

let divtest = [
(* ---------- DIV ---------- *)
  "Div: Pos / Pos",             BinaryOperation (Var "x", Div, Var "x");
  "Div: Pos / Neg",             BinaryOperation (Var "x", Div, Var "y");
  "Div: Neg / Neg",             BinaryOperation (Var "y", Div, Var "y");
  "Div: Costante / Zero",       BinaryOperation (Const 10, Div, Var "z");
  "Div: Pos / PosZero (rischio 0)", BinaryOperation (Var "x", Div, Var "w");
  "Div: Pos / NegZero (rischio 0)", BinaryOperation (Var "x", Div, Var "k");
  "Div: Pos / NonZero",         BinaryOperation (Var "x", Div, Var "n");
  "Div: Zero / Pos",            BinaryOperation (Var "z", Div, Var "x");
  "Div: Zero / Neg",            BinaryOperation (Var "z", Div, Var "y");
  "Div: Top / Pos",             BinaryOperation (Var "t", Div, Var "x");

]

let negatetest = [
  (* ---------- NEGATE ---------- *)
  "Negate: Pos",                UnaryOperation (Negation, Var "x");
  "Negate: Neg",                UnaryOperation (Negation, Var "y");
  "Negate: Zero",               UnaryOperation (Negation, Var "z");
  "Negate: PosZero",            UnaryOperation (Negation, Var "w");
  "Negate: NegZero",            UnaryOperation (Negation, Var "k");
  "Negate: NonZero",            UnaryOperation (Negation, Var "n");
  "Negate: Top",                UnaryOperation (Negation, Var "t");
  "Negate: Bottom",             UnaryOperation (Negation, Var "b");
  "Doppia negazione: --Pos",    UnaryOperation (Negation, UnaryOperation (Negation, Var "x"));
  "Pos + (-Neg)",               BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y"));
]

let randomtest = [
  "Random(-1,10)",              Random (-1, 10);
  "Random(1,10)",               Random (1, 10);
  "Random(-10,-1)",             Random (-10, -1);
  "Random(0,10)",               Random (0, 10);
  "Random(-10,0)",              Random (-10, 0);
  "Random(0,0)",                Random (0, 0);
]

let signtests = sumtests @ subtest @ multest @ divtest @ negatetest @ randomtest
 
(* ------------------------------------------------------------------ *)
(* Runner: valuta ogni espressione con l'interprete astratto e stampa  *)
(* ------------------------------------------------------------------ *)
let run_signs_tests tests =
  Printf.printf "===== TEST DOMINIO DEI SEGNI =====\n\n";
  List.iter
    (fun (name, e) ->
      try
        let result = SignInterp.eval e test_st in
        Printf.printf "[%-35s] -> %s\n" name (sign_to_string result)
      with e ->
        Printf.printf "[%-35s] -> ECCEZIONE: %s\n" name (Printexc.to_string e))
    tests;
  print_newline ()

(* ------------------------------------------------------------------ *)
(* Test diretti su lub / leq (non passano dall'interprete)             *)
(* ------------------------------------------------------------------ *)
let run_lub_leq_tests () =
  let open Signs in
  Printf.printf "===== TEST lub =====\n\n";
  let check_lub name a b =
    Printf.printf "lub (%s) -> %s\n" name (sign_to_string (lub a b))
  in
  check_lub "Pos, Neg" Pos Neg;
  check_lub "Pos, Zero" Pos Zero;
  check_lub "Neg, Zero" Neg Zero;
  check_lub "PosZero, NegZero" PosZero NegZero;
  check_lub "Pos, Pos" Pos Pos;
  check_lub "Bottom, Pos" SignBottom Pos;
  check_lub "Top, Pos" SignTop Pos;
  check_lub "NonZero, Zero" NonZero Zero;
  print_newline ();
 
  Printf.printf "===== TEST leq =====\n\n";
  let check_leq name a b expected =
    let result = leq a b in
    let ok = result = expected in
    Printf.printf "[%s] leq (%s) = %b (atteso %b)\n"
      (if ok then "OK" else "FAIL") name result expected
  in
  check_leq "Bottom, Pos" SignBottom Pos true;
  check_leq "Pos, Top" Pos SignTop true;
  check_leq "Pos, PosZero" Pos PosZero true;
  check_leq "PosZero, Pos" PosZero Pos false;
  check_leq "Zero, PosZero" Zero PosZero true;
  check_leq "Zero, NegZero" Zero NegZero true;
  check_leq "Pos, NonZero" Pos NonZero true;
  check_leq "Top, Pos" SignTop Pos false;
  print_newline ()
 
let run_all_tests () =
  run_signs_tests (signtests);
  run_lub_leq_tests ()