
(* --- Test --- *)
let test_st_int : (string, Abstract_domains.Intervals.t) Hashtbl.t = Hashtbl.create 200
let () =
  Hashtbl.add test_st_int "x" (Abstract_domains.Intervals.abstract_range 1 5);   (* [1,5]  *)
  Hashtbl.add test_st_int "y" (Abstract_domains.Intervals.abstract_range (-3) (-1)); (* [-3,-1] *)
  Hashtbl.add test_st_int "z" (Abstract_domains.Intervals.abstract_int 0);         (* [0,0]  *)
  Hashtbl.add test_st_int "t" (Abstract_domains.Intervals.top);         (* [0,0]  *)
  Hashtbl.add test_st_int "h" (Abstract_domains.Intervals.Interval (Abstract_domains.Intervals.Int(5),Abstract_domains.Intervals.PosInf));;

let interval_to_string = function
  | Abstract_domains.Intervals.Bottom -> "Bottom (errore/irraggiungibile)"
  | Abstract_domains.Intervals.Interval (a, b) ->
    let bound_to_string = function
      | Abstract_domains.Intervals.Int n -> string_of_int n
      | Abstract_domains.Intervals.PosInf -> "+inf"
      | Abstract_domains.Intervals.NegInf -> "-inf"
    in
    Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

let intervaltests =
  [
  "Somma [1,5]+[-3,-1]",       Syntax.BinaryOperation (Var "x", Add, Var "y");
  "Somma [1,5]+[0,0]",         Syntax.BinaryOperation (Var "x", Add, Var "z");
  "Sottrazione [1,5]-[-3,-1]", Syntax.BinaryOperation (Var "x", Sub, Var "y");
  "Sottrazione Const 42 -[-3,-1]", Syntax.BinaryOperation (Const 42, Sub, Var "y");
  "Negazione di [1,5]",        Syntax.UnaryOperation (Negation, Var "x");
  "Negazione di [-3,-1]",      Syntax.UnaryOperation (Negation, Var "y");
  "Input non deterministico",  Syntax.Random (1, 10);
  "Costante 42",               Syntax.Const 42;
  "Moltiplicazione [1,5]+[-3,-1]", Syntax.BinaryOperation(Var "x", Mul, Var "y");
  "Moltiplicazione [1,5]*[0,0]",         Syntax.BinaryOperation (Var "x", Mul, Var "z");
  "Moltiplicazione [1,5]*[5,+inf]",         Syntax.BinaryOperation (Var "x", Mul, Var "h");
  "Moltiplicazione [1,5]*[-inf,+inf]",         Syntax.BinaryOperation (Var "x", Mul, Var "t");
  "Moltiplicazione [1,5]*[42,42]",         Syntax.BinaryOperation (Var "x", Mul, Const 42);

  (*"Div",          Syntax.BinaryOperation (Const 0, Div, Var "z"); (* div per zero -> Bottom *) *)
  ]

(*Run test*)

let run_interval_tests () =
  Printf.printf "=== Inizio Test ===\n";

  List.iter (fun (name, e) ->
    let res = Interpeters.IntervalInterp.eval e test_st_int in
    Printf.printf "%-35s -> %s\n" name (interval_to_string res)
  ) intervaltests;

  Printf.printf "=== Fine Test ===\n";;

let () = run_interval_tests ()

