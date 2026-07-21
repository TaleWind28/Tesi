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
      let res = SignInterp.eval_exp expr st in
      Alcotest.(check sign_testable) desc expected res )

(* Verifica una o più variabili in uno stato finale *)
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
      check_vars desc (SignInterp.eval_cmd prog st) expected_vars )

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
        Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length final_env) );

    ( "Skip in mezzo a una sequenza non altera i valori",
      `Quick,
      fun () ->
        let final_env = make_test_state () in
        let prog = Sequence (Assign ("x", Const 42), Skip) in
        let res = SignInterp.eval_cmd prog final_env in
        Alcotest.(check sign_testable) "x resta Pos" Pos (Hashtbl.find res "x") );
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
]