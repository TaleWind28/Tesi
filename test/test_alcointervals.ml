open Abstract_domains.Intervals

(* ------------------------------------------------------------------ *)
(* 1. Setup dei Testable e Helper per la stampa                      *)
(* ------------------------------------------------------------------ *)

let bound_to_string = function
  | NegInf -> "-inf"
  | PosInf -> "+inf"
  | Int n  -> string_of_int n

let interval_to_string = function
  | Bottom -> "Bottom"
  | Interval (a, b) ->
      Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

(* Testable per Alcotest sugli intervalli *)
let interval_testable =
  let pp fmt i = Format.fprintf fmt "%s" (interval_to_string i) in
  Alcotest.testable pp ( = )

(* Helper per i test booleani (es. leq) *)
let make_bool_case (desc, op, a, b, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a b in
      Alcotest.(check bool) desc expected res )

(* Helper per i test unari sugli intervalli (es. negate) *)
let make_unary_case (desc, op, a, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a in
      Alcotest.(check interval_testable) desc expected res )

(* Helper per i test binari sugli intervalli (es. sum, mul, div, lub, glb) *)
let make_binary_case (desc, op, a, b, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a b in
      Alcotest.(check interval_testable) desc expected res )

(* ------------------------------------------------------------------ *)
(* 2. Liste di Test riutilizzate dal tuo codice                       *)
(* ------------------------------------------------------------------ *)

let leq_tests = List.map make_bool_case [
  ("Bottom, [1, 2]",               leq, Bottom, abstract_range 1 2, true);
  ("[1, 2], Bottom",               leq, abstract_range 1 2, Bottom, false);
  ("[2, 3] in [1, 5]",             leq, abstract_range 2 3, abstract_range 1 5, true);
  ("[1, 5] in [2, 3]",             leq, abstract_range 1 5, abstract_range 2 3, false);
  ("[1, 2] in [1, 2]",             leq, abstract_range 1 2, abstract_range 1 2, true);
  ("[0, 10] in Top",               leq, abstract_range 0 10, top, true);
  ("Top in [0, 10]",               leq, top, abstract_range 0 10, false);
]

let lub_tests = List.map make_binary_case [
  ("[1, 3], [5, 7]",               lub, abstract_range 1 3, abstract_range 5 7, abstract_range 1 7);
  ("[2, 5], [1, 3]",               lub, abstract_range 2 5, abstract_range 1 3, abstract_range 1 5);
  ("Bottom, [1, 2]",               lub, Bottom, abstract_range 1 2, abstract_range 1 2);
  ("[1, 2], Top",                  lub, abstract_range 1 2, top, top);
]

let glb_tests = List.map make_binary_case [
  ("[1, 5] e [3, 8]",              glb, abstract_range 1 5, abstract_range 3 8, abstract_range 3 5);
  ("[1, 3] e [5, 8] (disgiunti)",  glb, abstract_range 1 3, abstract_range 5 8, Bottom);
  ("[1, 5] e Bottom",              glb, abstract_range 1 5, Bottom, Bottom);
  ("[1, 5] e Top",                 glb, abstract_range 1 5, top, abstract_range 1 5);
]

let sum_tests = List.map make_binary_case [
  ("[1, 2] + [3, 4]",              sum, abstract_range 1 2, abstract_range 3 4, abstract_range 4 6);
  ("[-2, 5] + [10, 20]",           sum, abstract_range (-2) 5, abstract_range 10 20, abstract_range 8 25);
  ("[1, 2] + Bottom",              sum, abstract_range 1 2, Bottom, Bottom);
  ("[1, 2] + Top",                 sum, abstract_range 1 2, top, top);
]

let negate_tests = List.map make_unary_case [
  ("[1, 5]",                       negate, abstract_range 1 5, abstract_range (-5) (-1));
  ("[-3, 2]",                      negate, abstract_range (-3) 2, abstract_range (-2) 3);
  ("Bottom",                       negate, Bottom, Bottom);
]

let mul_tests = List.map make_binary_case [
  ("[2, 3] * [4, 5]",              mul, abstract_range 2 3, abstract_range 4 5, abstract_range 8 15);
  ("[-2, 3] * [-4, 5]",            mul, abstract_range (-2) 3, abstract_range (-4) 5, abstract_range (-12) 15);
  ("[-5, -2] * [-4, -1]",          mul, abstract_range (-5) (-2), abstract_range (-4) (-1), abstract_range 2 20);
  ("[-5, 2] * [4, 1]",             mul, abstract_range (-5) 2, abstract_range 4 1, abstract_range (-20) 8);
  ("[0, 5] * Bottom",              mul, abstract_range 0 5, Bottom, Bottom);
]

let div_tests = List.map make_binary_case [
  ("[10, 20] / [2, 5]",            div, abstract_range 10 20, abstract_range 2 5, abstract_range 2 10);
  ("[10, 20] / [-5, -2]",          div, abstract_range 10 20, abstract_range (-5) (-2), abstract_range (-10) (-2));
  ("[10, 20] / [0, 0]",            div, abstract_range 10 20, abstract_range 0 0, Bottom);
]

(* Lista globale dei test per gli Intervalli *)
let all_interval_cases =
  leq_tests @ lub_tests @ glb_tests @ sum_tests @ negate_tests @ mul_tests @ div_tests

(* Oppure raggruppati per operazione *)
let expr_tests = [
  "Intervals - leq", leq_tests;
  "Intervals - lub", lub_tests;
  "Intervals - glb", glb_tests;
  "Intervals - sum", sum_tests;
  "Intervals - negate", negate_tests;
  "Intervals - mul", mul_tests;
  "Intervals - div", div_tests;
]


open Syntax
open Abstract_domains.Intervals
open Interpeters

(* ------------------------------------------------------------------ *)
(* 1. Setup: stato, testable, helper per i casi di test               *)
(* ------------------------------------------------------------------ *)

let bound_to_string = function
  | NegInf -> "-inf"
  | PosInf -> "+inf"
  | Int n  -> string_of_int n

let interval_to_string = function
  | Bottom -> "Bottom"
  | Interval (a, b) ->
      Printf.sprintf "[%s,%s]" (bound_to_string a) (bound_to_string b)

let interval_testable =
  let pp fmt v = Format.fprintf fmt "%s" (interval_to_string v) in
  Alcotest.testable pp ( = )

(* Stato di test:
   x = [2,7]     positivo limitato
   y = [-8,-3]   negativo limitato
   z = [0,0]     zero esatto
   w = [0,5]     positivo-o-zero
   k = [-5,0]    negativo-o-zero
   n = [-3,4]    attraversa lo zero (nessun analogo diretto di NonZero)
   t = Top
   b = Bottom
   p = [1,+inf]  positivo illimitato
   m = [-inf,-1] negativo illimitato *)
let make_test_state () =
  let st = Hashtbl.create 10 in
  Hashtbl.add st "x" (Interval (Int 2, Int 7));
  Hashtbl.add st "y" (Interval (Int (-8), Int (-3)));
  Hashtbl.add st "z" (Interval (Int 0, Int 0));
  Hashtbl.add st "w" (Interval (Int 0, Int 5));
  Hashtbl.add st "k" (Interval (Int (-5), Int 0));
  Hashtbl.add st "n" (Interval (Int (-3), Int 4));
  Hashtbl.add st "t" top;
  Hashtbl.add st "b" bottom;
  Hashtbl.add st "p" (Interval (Int 1, PosInf));
  Hashtbl.add st "m" (Interval (NegInf, Int (-1)));
  st

let make_case (desc, expr, expected) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      let res = IntervalInterp.eval_exp expr st in
      Alcotest.(check interval_testable) desc expected res )

let check_vars desc final_env expected_vars =
  List.iter
    (fun (var, expected) ->
      let res =
        match Hashtbl.find_opt final_env var with
        | Some v -> v
        | None ->
            Alcotest.fail
              (Printf.sprintf "Variabile '%s' non trovata nello stato finale" var)
      in
      Alcotest.(check interval_testable) (desc ^ " - " ^ var) expected res)
    expected_vars

let make_prog_case (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () -> check_vars desc (IntervalInterp.eval prog) expected_vars )

let make_prog_case_with_env (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      check_vars desc (IntervalInterp.eval_cmd prog st) expected_vars )

(* ------------------------------------------------------------------ *)
(* 2. Test sulle espressioni (eval_exp)                               *)
(* ------------------------------------------------------------------ *)

let sumtests = List.map make_case [
  ("Sum: x+x",           BinaryOperation (Var "x", Add, Var "x"), Interval (Int 4, Int 14));
  ("Sum: x+y",           BinaryOperation (Var "x", Add, Var "y"), Interval (Int (-6), Int 4));
  ("Sum: y+y",           BinaryOperation (Var "y", Add, Var "y"), Interval (Int (-16), Int (-6)));
  ("Sum: x+z",           BinaryOperation (Var "x", Add, Var "z"), Interval (Int 2, Int 7));
  ("Sum: z+z",           BinaryOperation (Var "z", Add, Var "z"), Interval (Int 0, Int 0));
  ("Sum: w+w",           BinaryOperation (Var "w", Add, Var "w"), Interval (Int 0, Int 10));
  ("Sum: k+k",           BinaryOperation (Var "k", Add, Var "k"), Interval (Int (-10), Int 0));
  ("Sum: w+k",           BinaryOperation (Var "w", Add, Var "k"), Interval (Int (-5), Int 5));
  ("Sum: Top + x",       BinaryOperation (Var "t", Add, Var "x"), top);
  ("Sum: Bottom + x",    BinaryOperation (Var "b", Add, Var "x"), Bottom);
  ("Sum: [1,+inf] + x",  BinaryOperation (Var "p", Add, Var "x"), Interval (Int 3, PosInf));
  ("Sum: [-inf,-1] + y", BinaryOperation (Var "m", Add, Var "y"), Interval (NegInf, Int (-4)));
  ("Sum: 10 + (-20)",    BinaryOperation (Const 10, Add, Const (-20)), Interval (Int (-10), Int (-10)));
]

let subtests = List.map make_case [
  ("Sub: x-y",              BinaryOperation (Var "x", Sub, Var "y"), Interval (Int 5, Int 15));
  ("Sub: x-x (stessa var)", BinaryOperation (Var "x", Sub, Var "x"), Interval (Int (-5), Int 5));
  ("Sub: 10-20",            BinaryOperation (Const 10, Sub, Const 20), Interval (Int (-10), Int (-10)));
  ("Sub: w-w",              BinaryOperation (Var "w", Sub, Var "w"), Interval (Int (-5), Int 5));
  ("Sub: z-y",              BinaryOperation (Var "z", Sub, Var "y"), Interval (Int 3, Int 8));
  ("Sub: p-p (illimitato, perdita di precisione totale)",
   BinaryOperation (Var "p", Sub, Var "p"), top);
]

let multests = List.map make_case [
  ("Mul: x*x",                BinaryOperation (Var "x", Mul, Var "x"), Interval (Int 4, Int 49));
  ("Mul: x*y",                BinaryOperation (Var "x", Mul, Var "y"), Interval (Int (-56), Int (-6)));
  ("Mul: y*y",                BinaryOperation (Var "y", Mul, Var "y"), Interval (Int 9, Int 64));
  ("Mul: x*z",                BinaryOperation (Var "x", Mul, Var "z"), Interval (Int 0, Int 0));
  ("Mul: w*k",                BinaryOperation (Var "w", Mul, Var "k"), Interval (Int (-25), Int 0));
  ("Mul: Top * z",            BinaryOperation (Var "t", Mul, Var "z"), Interval (Int 0, Int 0));
  ("Mul: Bottom * x",         BinaryOperation (Var "b", Mul, Var "x"), Bottom);
  ("Mul: [1,+inf] * y",       BinaryOperation (Var "p", Mul, Var "y"), Interval (NegInf, Int (-3)));
  ("Mul: n*n (attraversa 0)", BinaryOperation (Var "n", Mul, Var "n"), Interval (Int (-12), Int 16));
  ("Mul: [-inf,-1] * y",      BinaryOperation (Var "m", Mul, Var "y"), Interval (Int 3, PosInf));
]

let divtests = List.map make_case [
  ("Div: x/x",                    BinaryOperation (Var "x", Div, Var "x"), Interval (Int 0, Int 3));
  ("Div: x/y",                    BinaryOperation (Var "x", Div, Var "y"), Interval (Int (-2), Int 0));
  ("Div: y/y",                    BinaryOperation (Var "y", Div, Var "y"), Interval (Int 0, Int 2));
  ("Div: Costante/z (0 esatto)",  BinaryOperation (Const 10, Div, Var "z"), Bottom);
  ("Div: x/w (rischio 0, w>=0)",  BinaryOperation (Var "x", Div, Var "w"), Interval (Int 0, Int 7));
  ("Div: x/k (rischio 0, k<=0)",  BinaryOperation (Var "x", Div, Var "k"), Interval (Int (-7), Int 0));
  ("Div: [1,+inf]/x",             BinaryOperation (Var "p", Div, Var "x"), Interval (Int 0, PosInf));
  ("Div: x/[-inf,-1]",            BinaryOperation (Var "x", Div, Var "m"), Interval (Int (-7), Int 0));
  ("Div: Top/x",                  BinaryOperation (Var "t", Div, Var "x"), top);
  ("Div: w/y",                    BinaryOperation (Var "w", Div, Var "y"), Interval (Int (-1), Int 0));
  ("Div: k/x",                    BinaryOperation (Var "k", Div, Var "x"), Interval (Int (-2), Int 0));
]

let negatetests = List.map make_case [
  ("Negate: x",               UnaryOperation (Negation, Var "x"), Interval (Int (-7), Int (-2)));
  ("Negate: y",               UnaryOperation (Negation, Var "y"), Interval (Int 3, Int 8));
  ("Negate: z",               UnaryOperation (Negation, Var "z"), Interval (Int 0, Int 0));
  ("Negate: w",               UnaryOperation (Negation, Var "w"), Interval (Int (-5), Int 0));
  ("Negate: k",               UnaryOperation (Negation, Var "k"), Interval (Int 0, Int 5));
  ("Negate: n",               UnaryOperation (Negation, Var "n"), Interval (Int (-4), Int 3));
  ("Negate: Top",             UnaryOperation (Negation, Var "t"), top);
  ("Negate: Bottom",          UnaryOperation (Negation, Var "b"), Bottom);
  ("Negate: [1,+inf]",        UnaryOperation (Negation, Var "p"), Interval (NegInf, Int (-1)));
  ("Negate: [-inf,-1]",       UnaryOperation (Negation, Var "m"), Interval (Int 1, PosInf));
  ("Doppia negazione: --x",   UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Interval (Int 2, Int 7));
  ("x + (-y)",                BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Interval (Int 5, Int 15));
]

let randomtests = List.map make_case [
  ("Random(-1,10)",  Random (-1, 10), Interval (Int (-1), Int 10));
  ("Random(1,10)",   Random (1, 10), Interval (Int 1, Int 10));
  ("Random(-10,-1)", Random (-10, -1), Interval (Int (-10), Int (-1)));
  ("Random(0,10)",   Random (0, 10), Interval (Int 0, Int 10));
  ("Random(-10,0)",  Random (-10, 0), Interval (Int (-10), Int 0));
  ("Random(0,0)",    Random (0, 0), Interval (Int 0, Int 0));
  ("Random(10,-5) (estremi invertiti)", Random (10, -5), Interval (Int (-5), Int 10));
]

(* ------------------------------------------------------------------ *)
(* 3. Test sui comandi (eval_cmd / eval)                              *)
(* ------------------------------------------------------------------ *)

let assigntests = List.map make_prog_case [
  ("Assign semplice: x = 5",  Assign ("x", Const 5), [ ("x", Interval (Int 5, Int 5)) ]);
  ("Assign semplice: x = -5", Assign ("x", Const (-5)), [ ("x", Interval (Int (-5), Int (-5))) ]);
  ("Assign semplice: x = 0",  Assign ("x", Const 0), [ ("x", Interval (Int 0, Int 0)) ]);
  ("Assign con variabile non definita: y = x (x non esiste -> Top)",
   Assign ("y", Var "x"), [ ("y", top) ]);
  ("Assign con Random: x = Random(1,10)",
   Assign ("x", Random (1, 10)), [ ("x", Interval (Int 1, Int 10)) ]);
]

let sequencetests = List.map make_prog_case [
  ("Sequence: x=5; y=-3",
   Sequence (Assign ("x", Const 5), Assign ("y", Const (-3))),
   [ ("x", Interval (Int 5, Int 5)); ("y", Interval (Int (-3), Int (-3))) ]);

  ("Sequence: usa il valore assegnato prima (y = x + x)",
   Sequence (
     Assign ("x", Const 5),
     Assign ("y", BinaryOperation (Var "x", Add, Var "x"))),
   [ ("x", Interval (Int 5, Int 5)); ("y", Interval (Int 10, Int 10)) ]);

  ("Sequence: catena di 3 assegnazioni con dipendenze",
   Sequence (
     Sequence (Assign ("x", Const 5), Assign ("y", Const (-5))),
     Assign ("z", BinaryOperation (Var "x", Add, Var "y"))),
   [ ("x", Interval (Int 5, Int 5)); ("y", Interval (Int (-5), Int (-5))); ("z", Interval (Int 0, Int 0)) ]);

  ("Sequence: z ricalcolato due volte dopo un Skip",
   Sequence (
     Sequence (
       Sequence (Assign ("x", Const 0), Assign ("y", Const (-9))),
       Sequence (
         Assign ("z", BinaryOperation (Var "x", Add, Var "y")),
         Sequence (Skip, Assign ("z", BinaryOperation (Var "x", Add, Var "y")))
       )
     ),
     Skip),
   [ ("x", Interval (Int 0, Int 0)); ("y", Interval (Int (-9), Int (-9))); ("z", Interval (Int (-9), Int (-9))) ]);
]

let overwritetests = List.map make_prog_case [
  ("Overwrite: x=5 poi x=-5",
   Sequence (Assign ("x", Const 5), Assign ("x", Const (-5))),
   [ ("x", Interval (Int (-5), Int (-5))) ]);

  ("Overwrite: x=5, x=0, x=x-1 -> [-1,-1]",
   Sequence (
     Sequence (Assign ("x", Const 5), Assign ("x", Const 0)),
     Assign ("x", BinaryOperation (Var "x", Sub, Const 1))),
   [ ("x", Interval (Int (-1), Int (-1))) ]);

  ("Overwrite tripla: y assegnata 3 volte, resta solo l'ultima",
   Sequence (
     Sequence (Assign ("y", Const 1), Assign ("y", Const 2)),
     Assign ("y", Const (-100))),
   [ ("y", Interval (Int (-100), Int (-100))) ]);
]

let skiptests =
  [ ( "Skip da solo non modifica lo stato (stato vuoto)",
      `Quick,
      fun () ->
        let final_env = IntervalInterp.eval Skip in
        Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length final_env) );

    ( "Skip in mezzo a una sequenza non altera i valori",
      `Quick,
      fun () ->
        let final_env = make_test_state () in
        let prog = Sequence (Assign ("x", Const 42), Skip) in
        let res = IntervalInterp.eval_cmd prog final_env in
        Alcotest.(check interval_testable) "x resta [42,42]"
          (Interval (Int 42, Int 42)) (Hashtbl.find res "x") );
  ]

let envtests = List.map make_prog_case_with_env [
  ("Riassegna x usando y già presente: x = y + y",
   Assign ("x", BinaryOperation (Var "y", Add, Var "y")),
   [ ("x", Interval (Int (-16), Int (-6))) ]);

  ("z = w * k (rischio 0 su entrambi i lati)",
   Assign ("z", BinaryOperation (Var "w", Mul, Var "k")),
   [ ("z", Interval (Int (-25), Int 0)) ]);

  ("Programma multi-step su stato precompilato",
   Sequence (
     Assign ("x", BinaryOperation (Var "x", Add, Var "z")), (* [2,7]+[0,0] = [2,7] *)
     Assign ("y", UnaryOperation (Negation, Var "y"))),      (* -[-8,-3] = [3,8] *)
   [ ("x", Interval (Int 2, Int 7)); ("y", Interval (Int 3, Int 8)) ]);

  ("Divisione con rischio zero su stato precompilato: z = x / w",
   Assign ("z", BinaryOperation (Var "x", Div, Var "w")),
   [ ("z", Interval (Int 0, Int 7)) ]);
]

(* ------------------------------------------------------------------ *)
(* 4. Esportazione unica di tutti i gruppi                            *)
(* ------------------------------------------------------------------ *)

let tests = expr_tests @ [
  "Somma", sumtests;
  "Sottrazione", subtests;
  "Moltiplicazione", multests;
  "Divisione", divtests;
  "Negazione", negatetests;
  "Random", randomtests;
  "Assegnazioni", assigntests;
  "Sequenze", sequencetests;
  "Overwrite", overwritetests;
  "Skip", skiptests;
  "Stato precompilato", envtests;
] 