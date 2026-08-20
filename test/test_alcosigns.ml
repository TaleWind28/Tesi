open Syntax
open Abstract_domains.Signs
open Interpeters

(* ------------------------------------------------------------------ *)
(* 1. Setup: stato, testable, helper per i casi di test               *)
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

let sign_testable =
  let pp fmt s = Format.fprintf fmt "%s" (sign_to_string s) in
  Alcotest.testable pp ( = )

(* Restituisce una Hashtbl "grezza"; chi la usa deve wrapparla in Env(...) *)
let make_test_state () =
  let st = Hashtbl.create 10 in
  Hashtbl.add st "x" Pos;
  Hashtbl.add st "y" Neg;
  Hashtbl.add st "z" Zero;
  Hashtbl.add st "w" PosZero;
  Hashtbl.add st "k" NegZero;
  Hashtbl.add st "n" NonZero;
  Hashtbl.add st "t" SignTop;
  Hashtbl.add st "b" SignBottom;
  st

(* Caso di test su una singola espressione, con stato precompilato *)
let make_case (desc, expr, expected) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      let res = SignInterp.eval_exp expr (Env st) in
      Alcotest.(check sign_testable) desc expected res )

(* Verifica una o più variabili in uno stato finale.
   Fallisce esplicitamente se lo stato finale è BottomEnv,
   perché in quel caso non esiste alcuna variabile da controllare. *)
let check_vars desc (final_env : Interpeters.SignInterp.state) expected_vars =
  match final_env with
  | BottomEnv ->
      Alcotest.fail
        (Printf.sprintf
           "%s: stato finale è BottomEnv, impossibile verificare variabili"
           desc)
  | Env tbl ->
      List.iter
        (fun (var, expected) ->
          let res =
            match Hashtbl.find_opt tbl var with
            | Some v -> v
            | None ->
                Alcotest.fail
                  (Printf.sprintf
                     "Variabile '%s' non trovata nello stato finale" var)
          in
          Alcotest.(check sign_testable) (desc ^ " - " ^ var) expected res)
        expected_vars

(* Caso di test su un programma, partendo da stato VUOTO *)
let make_prog_case (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () -> check_vars desc (SignInterp.eval prog) expected_vars )

(* Caso di test su un programma, partendo da stato PRECOMPILATO *)
let make_prog_case_with_env (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      check_vars desc (SignInterp.eval_cmd prog (Env st)) expected_vars )

(* ------------------------------------------------------------------ *)
(* 2. Test sulle espressioni (eval_exp)                               *)
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
  ("Sub: Pos - Neg",             BinaryOperation (Var "x", Sub, Var "y"), Pos);
  ("Sub: Pos - Pos (stessa var)",BinaryOperation (Var "x", Sub, Var "x"), SignTop);
  ("Sub: 10 - 20",               BinaryOperation (Const 10, Sub, Const 20), SignTop);
  ("Sub: PosZero - PosZero",     BinaryOperation (Var "w", Sub, Var "w"), SignTop);
  ("Sub: Zero - Neg",            BinaryOperation (Var "z", Sub, Var "y"), Pos);
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
  ("Div: Pos / Pos",                 BinaryOperation (Var "x", Div, Var "x"), PosZero);
  ("Div: Pos / Neg",                 BinaryOperation (Var "x", Div, Var "y"), NegZero);
  ("Div: Neg / Neg",                 BinaryOperation (Var "y", Div, Var "y"), PosZero);
  ("Div: Costante / Zero",           BinaryOperation (Const 10, Div, Var "z"), SignBottom);
  ("Div: Pos / PosZero (rischio 0)", BinaryOperation (Var "x", Div, Var "w"), SignTop);
  ("Div: Pos / NegZero (rischio 0)", BinaryOperation (Var "x", Div, Var "k"), SignTop);
  ("Div: Pos / NonZero",             BinaryOperation (Var "x", Div, Var "n"), SignTop);
  ("Div: Zero / Pos",                BinaryOperation (Var "z", Div, Var "x"), Zero);
  ("Div: Zero / Neg",                BinaryOperation (Var "z", Div, Var "y"), Zero);
  ("Div: Top / Pos",                 BinaryOperation (Var "t", Div, Var "x"), SignTop);
  ("Div: PosZero / Neg",             BinaryOperation (Var "w", Div, Var "y"), NegZero);
  ("Div: NegZero / Pos",             BinaryOperation (Var "k", Div, Var "x"), NegZero);
]

let negatetests = List.map make_case [
  ("Negate: Pos",             UnaryOperation (Negation, Var "x"), Neg);
  ("Negate: Neg",             UnaryOperation (Negation, Var "y"), Pos);
  ("Negate: Zero",            UnaryOperation (Negation, Var "z"), Zero);
  ("Negate: PosZero",         UnaryOperation (Negation, Var "w"), NegZero);
  ("Negate: NegZero",         UnaryOperation (Negation, Var "k"), PosZero);
  ("Negate: NonZero",         UnaryOperation (Negation, Var "n"), NonZero);
  ("Negate: Top",             UnaryOperation (Negation, Var "t"), SignTop);
  ("Negate: Bottom",          UnaryOperation (Negation, Var "b"), SignBottom);
  ("Doppia negazione: --Pos", UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Pos);
  ("Pos + (-Neg)",            BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Pos);
]

let randomtests = List.map make_case [
  ("Random(-1,10)", Random (-1, 10), SignTop);
  ("Random(1,10)",  Random (1, 10), Pos);
  ("Random(-10,-1)",Random (-10, -1), Neg);
  ("Random(0,10)",  Random (0, 10), PosZero);
  ("Random(-10,0)", Random (-10, 0), NegZero);
  ("Random(0,0)",   Random (0, 0), Zero);
]

(* ------------------------------------------------------------------ *)
(* 3. Test sui comandi (eval_cmd / eval)                              *)
(* ------------------------------------------------------------------ *)

let assigntests = List.map make_prog_case [
  ("Assign semplice: x = 5",  Assign ("x", Const 5), [ ("x", Pos) ]);
  ("Assign semplice: x = -5", Assign ("x", Const (-5)), [ ("x", Neg) ]);
  ("Assign semplice: x = 0",  Assign ("x", Const 0), [ ("x", Zero) ]);
  ("Assign con variabile non definita: y = x (x non esiste -> Top)",
   Assign ("y", Var "x"), [ ("y", SignTop) ]);
  ("Assign con Random: x = Random(1,10)",
   Assign ("x", Random (1, 10)), [ ("x", Pos) ]);
]

let sequencetests = List.map make_prog_case [
  ("Sequence: x=5; y=-3",
   Sequence (Assign ("x", Const 5), Assign ("y", Const (-3))),
   [ ("x", Pos); ("y", Neg) ]);

  ("Sequence: usa il valore assegnato prima (y = x + x)",
   Sequence (
     Assign ("x", Const 5),
     Assign ("y", BinaryOperation (Var "x", Add, Var "x"))),
   [ ("x", Pos); ("y", Pos) ]);

  ("Sequence: catena di 3 assegnazioni con dipendenze",
   Sequence (
     Sequence (Assign ("x", Const 5), Assign ("y", Const (-5))),
     Assign ("z", BinaryOperation (Var "x", Add, Var "y"))),
   [ ("x", Pos); ("y", Neg); ("z", SignTop) ]);

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
   [ ("x", Zero); ("y", Neg); ("z", Neg) ]);
]

let overwritetests = List.map make_prog_case [
  ("Overwrite: x=5 poi x=-5",
   Sequence (Assign ("x", Const 5), Assign ("x", Const (-5))),
   [ ("x", Neg) ]);

  ("Overwrite: x=5, x=0, x=x-1 -> Neg",
   Sequence (
     Sequence (Assign ("x", Const 5), Assign ("x", Const 0)),
     Assign ("x", BinaryOperation (Var "x", Sub, Const 1))),
   [ ("x", Neg) ]);

  ("Overwrite tripla: y assegnata 3 volte, resta solo l'ultima",
   Sequence (
     Sequence (Assign ("y", Const 1), Assign ("y", Const 2)),
     Assign ("y", Const (-100))),
   [ ("y", Neg) ]);
]

let skiptests =
  [ ( "Skip da solo non modifica lo stato (stato vuoto)",
      `Quick,
      fun () ->
        let final_env = SignInterp.eval Skip in
        match final_env with
        | Env tbl ->
            Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl)
        | BottomEnv ->
            Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );

    ( "Skip in mezzo a una sequenza non altera i valori",
      `Quick,
      fun () ->
        let st = make_test_state () in
        let prog = Sequence (Assign ("x", Const 42), Skip) in
        let res = SignInterp.eval_cmd prog (Env st) in
        match res with
        | Env tbl ->
            Alcotest.(check sign_testable) "x resta Pos" Pos (Hashtbl.find tbl "x")
        | BottomEnv ->
            Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );
  ]

let envtests = List.map make_prog_case_with_env [
  ("Riassegna x usando y già presente (y=Neg): x = y + y -> Neg",
   Assign ("x", BinaryOperation (Var "y", Add, Var "y")),
   [ ("x", Neg) ]);

  ("z = w * k (PosZero*NegZero)",
   Assign ("z", BinaryOperation (Var "w", Mul, Var "k")),
   [ ("z", NegZero) ]);

  ("Programma multi-step su stato precompilato",
   Sequence (
     Assign ("x", BinaryOperation (Var "x", Add, Var "z")), (* Pos + Zero = Pos *)
     Assign ("y", UnaryOperation (Negation, Var "y"))),      (* -Neg = Pos *)
   [ ("x", Pos); ("y", Pos) ]);

  ("Divisione con rischio zero su stato precompilato: z = x / w",
   Assign ("z", BinaryOperation (Var "x", Div, Var "w")),
   [ ("z", SignTop) ]);
]

(* Test dedicato per il caso BottomEnv: qui non ha senso controllare
   variabili singole, perché l'intero stato collassa a BottomEnv. *)
let condtest =
  [ ( "Filter(false); x=10 -> stato finale BottomEnv",
      `Quick,
      fun () ->
        let st = make_test_state () in
        let prog =
          Sequence (Filter (Boolean false), Assign ("x", Const 10))
        in
        let res = SignInterp.eval_cmd prog (Env st) in
        match res with
        | BottomEnv -> ()
        | Env _ ->
            Alcotest.fail
              "Test Prog: atteso BottomEnv, ottenuto Env" )
  ]

(* ------------------------------------------------------------------ *)
(* 3bis. Test dedicati a Filter / eval_cond                           *)
(* ------------------------------------------------------------------ *)

(* Helper: si aspetta BottomEnv da un programma con stato precompilato *)
let expect_bottom_with_env desc prog =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      let res = SignInterp.eval_cmd prog (Env st) in
      match res with
      | BottomEnv -> ()
      | Env _ ->
          Alcotest.fail
            (Printf.sprintf "%s: atteso BottomEnv, ottenuto Env" desc) )

(* --- Casi certi: il confronto ha esito deciso senza ambiguità --- *)
let filter_certain_tests = List.map make_prog_case_with_env [
  ("Filter certo vero: x > y (Pos > Neg)",
   Filter (Comparison (Var "x", Bigger, Var "y")),
   [ ("x", Pos); ("y", Neg) ]);

  ("Filter certo vero: y < x (Neg < Pos)",
   Filter (Comparison (Var "y", Smaller, Var "x")),
   [ ("x", Pos); ("y", Neg) ]);

  ("Filter certo vero: z = z (Zero = Zero, stessa var)",
   Filter (Comparison (Var "z", Equals, Var "z")),
   [ ("z", Zero) ]);

  ("Filter certo vero: x != y (Pos disgiunto da Neg)",
   Filter (Comparison (Var "x", NotEquals, Var "y")),
   [ ("x", Pos); ("y", Neg) ]);
]

let filter_certain_bottom_tests = [
  expect_bottom_with_env
    "Filter certo falso: y > x (Neg > Pos, impossibile)"
    (Filter (Comparison (Var "y", Bigger, Var "x")));

  expect_bottom_with_env
    "Filter certo falso: x < y (Pos < Neg, impossibile)"
    (Filter (Comparison (Var "x", Smaller, Var "y")));

  expect_bottom_with_env
    "Filter certo falso: x = y (Pos disgiunto da Neg)"
    (Filter (Comparison (Var "x", Equals, Var "y")));

  expect_bottom_with_env
    "Filter certo falso: z != z (Zero != Zero, impossibile)"
    (Filter (Comparison (Var "z", NotEquals, Var "z")));
]

(* --- Casi ambigui: i segni si sovrappongono, Filter non deve tagliare --- *)
let filter_ambiguous_tests = List.map make_prog_case_with_env [
  ("Filter ambiguo: x > w (Pos vs PosZero si sovrappongono) -> passa, non restringe",
   Filter (Comparison (Var "x", Bigger, Var "w")),
   [ ("x", Pos); ("w", PosZero) ]);

  ("Filter ambiguo: x = w (Pos vs PosZero) -> passa",
   Filter (Comparison (Var "x", Equals, Var "w")),
   [ ("x", Pos); ("w", PosZero) ]);

  ("Filter ambiguo: x > n (Pos vs NonZero) -> passa",
   Filter (Comparison (Var "x", Bigger, Var "n")),
   [ ("x", Pos); ("n", NonZero) ]);

  ("Filter ambiguo: y < w (Neg vs PosZero, comunque si controlla) -> passa",
   Filter (Comparison (Var "y", Smaller, Var "w")),
   [ ("y", Neg); ("w", PosZero) ]);
]

(* --- Test di simmetria: stessa coppia ambigua, ordine invertito --- *)
let filter_symmetry_tests = List.map make_prog_case_with_env [
  ("Simmetria ambiguo A: x > w (Pos, PosZero)",
   Filter (Comparison (Var "x", Bigger, Var "w")),
   [ ("x", Pos); ("w", PosZero) ]);

  ("Simmetria ambiguo B: w > x (PosZero, Pos) - deve comportarsi come sopra",
   Filter (Comparison (Var "w", Bigger, Var "x")),
   [ ("x", Pos); ("w", PosZero) ]);

  ("Simmetria Equals A: x = w (Pos, PosZero)",
   Filter (Comparison (Var "x", Equals, Var "w")),
   [ ("x", Pos); ("w", PosZero) ]);

  ("Simmetria Equals B: w = x (PosZero, Pos)",
   Filter (Comparison (Var "w", Equals, Var "x")),
   [ ("x", Pos); ("w", PosZero) ]);
]

(* --- Operatori derivati: BiggerEquals / SmallerEquals --- *)
let filter_derived_ops_tests = List.map make_prog_case_with_env [
  ("BiggerEquals certo vero: x >= z (Pos >= Zero)",
   Filter (Comparison (Var "x", BiggerEquals, Var "z")),
   [ ("x", Pos); ("z", Zero) ]);

  ("SmallerEquals certo vero: z <= z (Zero <= Zero, caso limite)",
   Filter (Comparison (Var "z", SmallerEquals, Var "z")),
   [ ("z", Zero) ]);

  ("BiggerEquals ambiguo: w >= x (PosZero >= Pos) -> passa",
   Filter (Comparison (Var "w", BiggerEquals, Var "x")),
   [ ("w", PosZero); ("x", Pos) ]);
]

let filter_derived_ops_bottom_tests = [
  expect_bottom_with_env
    "SmallerEquals certo falso: x <= y (Pos <= Neg, impossibile)"
    (Filter (Comparison (Var "x", SmallerEquals, Var "y")));

  expect_bottom_with_env
    "BiggerEquals certo falso: y >= x (Neg >= Pos, impossibile)"
    (Filter (Comparison (Var "y", BiggerEquals, Var "x")));
]

(* --- Composizione: And, Or, Not --- *)
let filter_composition_tests = List.map make_prog_case_with_env [
  ("And di due certi veri: x>y And y<x",
   Filter (And (
     Comparison (Var "x", Bigger, Var "y"),
     Comparison (Var "y", Smaller, Var "x"))),
   [ ("x", Pos); ("y", Neg) ]);

  ("Or con un ramo impossibile e uno vero: passa comunque",
   Filter (Or (
     Comparison (Var "x", Smaller, Var "y"),   (* falso *)
     Comparison (Var "y", Smaller, Var "x"))), (* vero *)
   [ ("x", Pos); ("y", Neg) ]);

  ("Not su un confronto certo falso: diventa vero, passa",
   Filter (Not (Comparison (Var "x", Smaller, Var "y"))),
   [ ("x", Pos); ("y", Neg) ]);

  ("Or di due Boolean: false Or true -> passa",
   Filter (Or (Boolean false, Boolean true)),
   [ ("x", Pos) ]);
]

let filter_composition_bottom_tests = [
  expect_bottom_with_env
    "And con un ramo falso: x>y And x<y -> BottomEnv"
    (Filter (And (
       Comparison (Var "x", Bigger, Var "y"),
       Comparison (Var "x", Smaller, Var "y"))));

  expect_bottom_with_env
    "Not su un confronto certo vero: diventa falso -> BottomEnv"
    (Filter (Not (Comparison (Var "x", Bigger, Var "y"))));

  expect_bottom_with_env
    "Or di due Boolean false: false Or false -> BottomEnv"
    (Filter (Or (Boolean false, Boolean false)));
]

(* --- Caso limite: confronto che coinvolge SignBottom --- *)
let filter_bottom_value_tests = List.map make_prog_case_with_env [
  ("Confronto b = b (SignBottom = SignBottom, stessa var) -> certo uguale, passa",
   Filter (Comparison (Var "b", Equals, Var "b")),
   [ ("b", SignBottom) ]);
]

(* --- Filter incatenato con Assign, per verificare propagazione --- *)
let filter_chained_tests = List.map make_prog_case_with_env [
  ("Filter ambiguo poi Assign: lo stato prosegue e z viene ricalcolata",
   Sequence (
     Filter (Comparison (Var "x", Bigger, Var "w")),  (* ambiguo, passa *)
     Assign ("z", BinaryOperation (Var "x", Add, Var "y"))),
   [ ("x", Pos); ("z", SignTop) ]);
]

let filter_chained_bottom_tests = [
  expect_bottom_with_env
    "Filter certo falso poi Assign: BottomEnv si propaga, Assign non ha effetto"
    (Sequence (
       Filter (Comparison (Var "y", Bigger, Var "x")), (* certo falso *)
       Assign ("x", Const 999)));

  expect_bottom_with_env
    "Doppio Filter: prima passa (ambiguo), poi taglia (certo falso)"
    (Sequence (
       Filter (Comparison (Var "x", Bigger, Var "w")),  (* ambiguo, passa *)
       Filter (Comparison (Var "y", Bigger, Var "x")))); (* certo falso *)
]

(* ------------------------------------------------------------------ *)
(* 4. Esportazione unica di tutti i gruppi                            *)
(* ------------------------------------------------------------------ *)

let tests = [
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
  "Test Prog", condtest;
  "Filter - casi certi", filter_certain_tests;
   (* @ filter_certain_bottom_tests; *)
  "Filter - casi ambigui", filter_ambiguous_tests;
  "Filter - simmetria", filter_symmetry_tests;
  "Filter - operatori derivati", filter_derived_ops_tests @ filter_derived_ops_bottom_tests;
  "Filter - composizione And/Or/Not", filter_composition_tests @ filter_composition_bottom_tests;
  "Filter - valore Bottom", filter_bottom_value_tests;
  "Filter - incatenato", filter_chained_tests @ filter_chained_bottom_tests;
]