open Syntax
open Abstract_domains.Signs
open Interpeters

(* ------------------------------------------------------------------ *)
(* 1. Setup dello stato e del tipo Testable per Alcotest              *)
(* ------------------------------------------------------------------ *)

let sign_to_string = function
  | SignTop    -> "Top"
  | Pos        -> "Pos"
  | Neg        -> "Neg"
  | Zero       -> "Zero"
  | SignBottom -> "Bottom"
  | PosZero    -> "PosZero"
  | NegZero    -> "NegZero"
  | NonZero    -> "NonZero"

(* Costruiamo il testable per permettere ad Alcotest di confrontare e stampare i risultati *)
let sign_testable =
  let pp fmt s = Format.fprintf fmt "%s" (sign_to_string s) in
  Alcotest.testable pp ( = )

(* Helper per creare uno stato di ambiente fresco per ogni test *)
let make_test_state () =
  let st = Hashtbl.create 10 in
  Hashtbl.add st "x" Pos;        (* > 0 *)
  Hashtbl.add st "y" Neg;        (* < 0 *)
  Hashtbl.add st "z" Zero;       (* = 0 *)
  Hashtbl.add st "w" PosZero;    (* >= 0 *)
  Hashtbl.add st "k" NegZero;    (* <= 0 *)
  Hashtbl.add st "n" NonZero;    (* != 0 *)
  Hashtbl.add st "t" SignTop;    (* Top *)
  Hashtbl.add st "b" SignBottom; (* Bottom *)
  st

(* Helper per trasformare una tripla (descrizione, expr, atteso) in un test Alcotest *)
let make_case (desc, expr, expected) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      let res = SignInterp.eval expr st in
      Alcotest.(check sign_testable) desc expected res )

(* ------------------------------------------------------------------ *)
(* 2. Liste di Test riutilizzate dal tuo codice                       *)
(* ------------------------------------------------------------------ *)

let sumtests = List.map make_case [
  ("Sum: Pos + Pos",            BinaryOperation (Var "x", Add, Var "x"), Pos);
  ("Sum: Pos + Neg",            BinaryOperation (Var "x", Add, Var "y"), SignTop);
  ("Sum: Neg + Neg",            BinaryOperation (Var "y", Add, Var "y"), Neg);
  ("Sum: Pos + Zero",           BinaryOperation (Var "x", Add, Var "z"), Pos);
  ("Sum: Zero + Zero",          BinaryOperation (Var "z", Add, Var "z"), Zero);
  ("Sum: PosZero + PosZero",    BinaryOperation (Var "w", Add, Var "w"), PosZero);
  ("Sum: NegZero + NegZero",    BinaryOperation (Var "k", Add, Var "k"), NegZero);
  ("Sum: PosZero + NegZero",    BinaryOperation (Var "w", Add, Var "k"), SignTop);
  ("Sum: PosZero + Pos",        BinaryOperation (Var "w", Add, Var "x"), Pos);
  ("Sum: PosZero + Neg",        BinaryOperation (Var "w", Add, Var "y"), SignTop);
  ("Sum: NegZero + Pos",        BinaryOperation (Var "k", Add, Var "x"), SignTop);
  ("Sum: NegZero + Neg",        BinaryOperation (Var "k", Add, Var "y"), Neg);
  ("Sum: NonZero + Pos",        BinaryOperation (Var "n", Add, Var "x"), SignTop);
  ("Sum: NonZero + Zero",       BinaryOperation (Var "n", Add, Var "z"), NonZero);
  ("Sum: NonZero + NonZero",    BinaryOperation (Var "n", Add, Var "n"), SignTop);
  ("Sum: Top + Pos",            BinaryOperation (Var "t", Add, Var "x"), SignTop);
  ("Sum: Bottom + Pos",         BinaryOperation (Var "b", Add, Var "x"), SignBottom);
  ("Sum: 10 + (-20)",           BinaryOperation (Const 10, Add, Const (-20)), SignTop);
]

let subtests = List.map make_case [
  ("Sub: Pos - Neg",            BinaryOperation (Var "x", Sub, Var "y"), Pos);
  ("Sub: Pos - Pos (stessa var)",BinaryOperation (Var "x", Sub, Var "x"), SignTop);
  ("Sub: 10 - 20",              BinaryOperation (Const 10, Sub, Const 20), SignTop);
  ("Sub: PosZero - PosZero",    BinaryOperation (Var "w", Sub, Var "w"), SignTop);
  ("Sub: Zero - Neg",           BinaryOperation (Var "z", Sub, Var "y"), Pos);
]

let multests = List.map make_case [
  ("Mul: Pos * Pos",            BinaryOperation (Var "x", Mul, Var "x"), Pos);
  ("Mul: Pos * Neg",            BinaryOperation (Var "x", Mul, Var "y"), Neg);
  ("Mul: Neg * Neg",            BinaryOperation (Var "y", Mul, Var "y"), Pos);
  ("Mul: Pos * Zero",           BinaryOperation (Var "x", Mul, Var "z"), Zero);
  ("Mul: PosZero * Neg",        BinaryOperation (Var "w", Mul, Var "y"), NegZero);
  ("Mul: NegZero * Pos",        BinaryOperation (Var "k", Mul, Var "x"), NegZero);
  ("Mul: NonZero * Zero",       BinaryOperation (Var "n", Mul, Var "z"), Zero);
  ("Mul: NonZero * NonZero",    BinaryOperation (Var "n", Mul, Var "n"), NonZero);
  ("Mul: Top * Zero",           BinaryOperation (Var "t", Mul, Var "z"), Zero);
  ("Mul: Bottom * Pos",         BinaryOperation (Var "b", Mul, Var "x"), SignBottom);
]

let divtests = List.map make_case [
  ("Div: Pos / Pos",            BinaryOperation (Var "x", Div, Var "x"), PosZero);
  ("Div: Pos / Neg",            BinaryOperation (Var "x", Div, Var "y"), NegZero);
  ("Div: Neg / Neg",            BinaryOperation (Var "y", Div, Var "y"), PosZero);
  ("Div: Costante / Zero",      BinaryOperation (Const 10, Div, Var "z"), SignBottom);
  ("Div: Pos / PosZero (rischio 0)", BinaryOperation (Var "x", Div, Var "w"), SignTop);
  ("Div: Pos / NegZero (rischio 0)", BinaryOperation (Var "x", Div, Var "k"), SignTop);
  ("Div: Pos / NonZero",        BinaryOperation (Var "x", Div, Var "n"), SignTop);
  ("Div: Zero / Pos",           BinaryOperation (Var "z", Div, Var "x"), Zero);
  ("Div: Zero / Neg",           BinaryOperation (Var "z", Div, Var "y"), Zero);
  ("Div: Top / Pos",            BinaryOperation (Var "t", Div, Var "x"), SignTop);
  ("Div: PosZero / Neg",        BinaryOperation (Var "w", Div, Var "y"), NegZero);
  ("Div: NegZero / Pos",        BinaryOperation (Var "k", Div, Var "x"), NegZero);
]

let negatetests = List.map make_case [
  ("Negate: Pos",               UnaryOperation (Negation, Var "x"), Neg);
  ("Negate: Neg",               UnaryOperation (Negation, Var "y"), Pos);
  ("Negate: Zero",              UnaryOperation (Negation, Var "z"), Zero);
  ("Negate: PosZero",           UnaryOperation (Negation, Var "w"), NegZero);
  ("Negate: NegZero",           UnaryOperation (Negation, Var "k"), PosZero);
  ("Negate: NonZero",           UnaryOperation (Negation, Var "n"), NonZero);
  ("Negate: Top",               UnaryOperation (Negation, Var "t"), SignTop);
  ("Negate: Bottom",            UnaryOperation (Negation, Var "b"), SignBottom);
  ("Doppia negazione: --Pos",   UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Pos);
  ("Pos + (-Neg)",              BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Pos);
]

let randomtests = List.map make_case [
  ("Random(-1,10)",             Random (-1, 10), SignTop);
  ("Random(1,10)",              Random (1, 10), Pos);
  ("Random(-10,-1)",            Random (-10, -1), Neg);
  ("Random(0,10)",              Random (0, 10), PosZero);
  ("Random(-10,0)",             Random (-10, 0), NegZero);
  ("Random(0,0)",               Random (0, 0), Zero);
]

(* ------------------------------------------------------------------ *)
(* 3. Esportazione dei Gruppi di Test                                 *)
(* ------------------------------------------------------------------ *)

let tests = [
  "Somma", sumtests;
  "Sottrazione", subtests;
  "Moltiplicazione", multests;
  "Divisione", divtests;
  "Negazione", negatetests;
  "Random", randomtests;
]