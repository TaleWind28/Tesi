open Syntax
open Abstract_domains
open Interpeters

(* --- Test --- *)
let test_st : (string, Signs.t) Hashtbl.t = Hashtbl.create 10
let () =
  Hashtbl.add test_st "x" Signs.Pos;
  Hashtbl.add test_st "y" Signs.Neg;
  Hashtbl.add test_st "z" Signs.Zero;
  Hashtbl.add test_st "w" Signs.PosZero;
  Hashtbl.add test_st "k" Signs.NegZero

let sign_to_string = function
  | Signs.SignTop    -> "Top (sconosciuto)"
  | Signs.Pos        -> "Positivo"
  | Signs.Neg        -> "Negativo"
  | Signs.Zero       -> "Zero"
  | Signs.SignBottom -> "Bottom (errore/irraggiungibile)"
  | Signs.PosZero -> "Positivo o Zero"
  | Signs.NegZero -> "Negativo o Zero"

let signtests =
  [ 
  "Somma Pos+Neg",          BinaryOperation (Var "x", Add, Var "y"); 
  "Moltiplicazione Pos*Zero", BinaryOperation (Var "x", Mul, Var "z"); 
  "Divisione per Zero",     BinaryOperation (Const 10, Div, Var "z");
  "Input non deterministico", Random (-1, 10);
  "Negazione di Pos",       UnaryOperation (Negation, Var "x");
  "Pos + (-Neg) ",    BinaryOperation (Var "x", Add, UnaryOperation(Negation,Var "y"));
  "Pos - Neg ",    BinaryOperation (Var "x", Sub, Var "y");
  "Pos - Neg ",    BinaryOperation (Var "x", Sub, Var "x");
  "10 - 20", BinaryOperation(Const 10, Sub,Const 20 );
  "NegZero + Neg", BinaryOperation(Var "k", Add, Var "y");
  ]

let test_st_int : (string, Intervals.t) Hashtbl.t = Hashtbl.create 10
let () =
  Hashtbl.add test_st_int "x" (Intervals.abstract_range 1 5);   (* [1,5]  *)
  Hashtbl.add test_st_int "y" (Intervals.abstract_range (-3) (-1)); (* [-3,-1] *)
  Hashtbl.add test_st_int "z" (Intervals.abstract_int 0)         (* [0,0]  *)

let interval_to_string = function
  | Intervals.Bottom -> "Bottom (errore/irraggiungibile)"
  | Intervals.Interval (a, b) ->
    let bound_to_string = function
      | Intervals.Int n -> string_of_int n
      | Intervals.PosInf -> "+inf"
      | Intervals.NegInf -> "-inf"
    in
    Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

let intervaltests =
  [
  "Somma [1,5]+[-3,-1]",       BinaryOperation (Var "x", Add, Var "y");
  "Somma [1,5]+[0,0]",         BinaryOperation (Var "x", Add, Var "z");
  "Sottrazione [1,5]-[-3,-1]", BinaryOperation (Var "x", Sub, Var "y");
  "Sottrazione Const 42 -[-3,-1]", BinaryOperation (Const 42, Sub, Var "y");
  "Negazione di [1,5]",        UnaryOperation (Negation, Var "x");
  "Negazione di [-3,-1]",      UnaryOperation (Negation, Var "y");
  "Input non deterministico",  Random (1, 10);
  "Costante 42",               Const 42;
  "Div",          BinaryOperation (Const 0, Div, Var "z"); (* div per zero -> Bottom *)
  ]

(*Run test*)
let run_sign_tests () =
  Printf.printf "=== Inizio Test ===\n";

  List.iter (fun (name, e) ->
    let res = SignInterp.eval e test_st in
    Printf.printf "%-30s -> %s\n" name (sign_to_string res)
  ) signtests;
  Printf.printf "=== Fine Test ===\n";;

let () = run_sign_tests ()

let run_interval_tests () =
  Printf.printf "=== Inizio Test ===\n";

  List.iter (fun (name, e) ->
    let res = IntervalInterp.eval e test_st_int in
    Printf.printf "%-35s -> %s\n" name (interval_to_string res)
  ) intervaltests;

  Printf.printf "=== Fine Test ===\n";;

let () = run_interval_tests ()
