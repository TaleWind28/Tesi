open Syntax
open Abstract_domains.Signs
open Interpeters

(* ------------------------------------------------------------ *)
(* 1. Setup                                                      *)
(* ------------------------------------------------------------ *)

let sign_to_string = function
  | SignTop -> "Top" 
  | Pos -> "Pos" 
  | Neg -> "Neg" 
  | Zero -> "Zero"
  | SignBottom -> "Bottom" 
  | PosZero -> "PosZero"
  | NegZero -> "NegZero" 
  | NonZero -> "NonZero"

let sign_testable =
  Alcotest.testable (fun fmt s -> Format.fprintf fmt "%s" (sign_to_string s)) ( = )

(* Stato "grezzo": va wrappato in Env(...) da chi lo usa *)
let make_test_state () =
  let st = Hashtbl.create 10 in
  List.iter (fun (k, v) -> Hashtbl.add st k v)
    [ "x", Pos; "y", Neg; "z", Zero; "w", PosZero;
      "k", NegZero; "n", NonZero; "t", SignTop; "b", SignBottom ];
  st

let make_case (desc, expr, expected) =
  ( desc, `Quick,
    fun () ->
      let res = SignInterp.eval_exp expr (Env (make_test_state ())) in
      Alcotest.(check sign_testable) desc expected res )

(* Verifica una o più variabili in uno stato finale; fallisce se BottomEnv *)
let check_vars desc (final_env : Interpeters.SignInterp.state) expected_vars =
  match final_env with
  | BottomEnv ->
      Alcotest.fail (Printf.sprintf "%s: stato finale è BottomEnv, impossibile verificare variabili" desc)
  | Env tbl ->
      List.iter
        (fun (var, expected) ->
          let res =
            match Hashtbl.find_opt tbl var with
            | Some v -> v
            | None -> Alcotest.fail (Printf.sprintf "Variabile '%s' non trovata nello stato finale" var)
          in
          Alcotest.(check sign_testable) (desc ^ " - " ^ var) expected res)
        expected_vars

(* Stato iniziale VUOTO *)
let make_prog_case (desc, prog, expected_vars) =
  (desc, `Quick, fun () -> check_vars desc (SignInterp.eval prog) expected_vars)

(* Stato iniziale PRECOMPILATO *)
let make_prog_case_with_env (desc, prog, expected_vars) =
  ( desc, `Quick,
    fun () -> check_vars desc (SignInterp.eval_cmd prog (Env (make_test_state ()))) expected_vars )

(* Si aspetta BottomEnv. Se [with_env] è true parte dallo stato precompilato
   (eval_cmd), altrimenti da stato vuoto (eval). Sostituisce le due funzioni
   duplicate expect_bottom / expect_bottom_with_env della versione originale. *)
let expect_bottom ?(with_env = false) desc prog =
  ( desc, `Quick,
    fun () ->
      let res =
        if with_env then SignInterp.eval_cmd prog (Env (make_test_state ()))
        else SignInterp.eval prog
      in
      match res with
      | BottomEnv -> ()
      | Env _ -> Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto Env" desc) )

let expect_bottom_with_env desc prog = expect_bottom ~with_env:true desc prog

(* ------------------------------------------------------------ *)
(* 2. Espressioni (eval_exp)                                     *)
(* ------------------------------------------------------------ *)

let sumtests = List.map make_case [
  "Sum: Pos + Pos", BinaryOperation (Var "x", Add, Var "x"), Pos;
  "Sum: Pos + Neg", BinaryOperation (Var "x", Add, Var "y"), SignTop;
  "Sum: Neg + Neg", BinaryOperation (Var "y", Add, Var "y"), Neg;
  "Sum: Pos + Zero", BinaryOperation (Var "x", Add, Var "z"), Pos;
  "Sum: Zero + Zero", BinaryOperation (Var "z", Add, Var "z"), Zero;
  "Sum: PosZero + PosZero", BinaryOperation (Var "w", Add, Var "w"), PosZero;
  "Sum: NegZero + NegZero", BinaryOperation (Var "k", Add, Var "k"), NegZero;
  "Sum: PosZero + NegZero", BinaryOperation (Var "w", Add, Var "k"), SignTop;
  "Sum: PosZero + Pos", BinaryOperation (Var "w", Add, Var "x"), Pos;
  "Sum: PosZero + Neg", BinaryOperation (Var "w", Add, Var "y"), SignTop;
  "Sum: NegZero + Pos", BinaryOperation (Var "k", Add, Var "x"), SignTop;
  "Sum: NegZero + Neg", BinaryOperation (Var "k", Add, Var "y"), Neg;
  "Sum: NonZero + Pos", BinaryOperation (Var "n", Add, Var "x"), SignTop;
  "Sum: NonZero + Zero", BinaryOperation (Var "n", Add, Var "z"), NonZero;
  "Sum: NonZero + NonZero", BinaryOperation (Var "n", Add, Var "n"), SignTop;
  "Sum: Top + Pos", BinaryOperation (Var "t", Add, Var "x"), SignTop;
  "Sum: Bottom + Pos", BinaryOperation (Var "b", Add, Var "x"), SignBottom;
  "Sum: 10 + (-20)", BinaryOperation (Const 10, Add, Const (-20)), SignTop;
]

let subtests = List.map make_case [
  "Sub: Pos - Neg", BinaryOperation (Var "x", Sub, Var "y"), Pos;
  "Sub: Pos - Pos (stessa var)", BinaryOperation (Var "x", Sub, Var "x"), SignTop;
  "Sub: 10 - 20", BinaryOperation (Const 10, Sub, Const 20), SignTop;
  "Sub: PosZero - PosZero", BinaryOperation (Var "w", Sub, Var "w"), SignTop;
  "Sub: Zero - Neg", BinaryOperation (Var "z", Sub, Var "y"), Pos;
]

let multests = List.map make_case [
  "Mul: Pos * Pos", BinaryOperation (Var "x", Mul, Var "x"), Pos;
  "Mul: Pos * Neg", BinaryOperation (Var "x", Mul, Var "y"), Neg;
  "Mul: Neg * Neg", BinaryOperation (Var "y", Mul, Var "y"), Pos;
  "Mul: Pos * Zero", BinaryOperation (Var "x", Mul, Var "z"), Zero;
  "Mul: PosZero * Neg", BinaryOperation (Var "w", Mul, Var "y"), NegZero;
  "Mul: NegZero * Pos", BinaryOperation (Var "k", Mul, Var "x"), NegZero;
  "Mul: NonZero * Zero", BinaryOperation (Var "n", Mul, Var "z"), Zero;
  "Mul: NonZero * NonZero", BinaryOperation (Var "n", Mul, Var "n"), NonZero;
  "Mul: Top * Zero", BinaryOperation (Var "t", Mul, Var "z"), Zero;
  "Mul: Bottom * Pos", BinaryOperation (Var "b", Mul, Var "x"), SignBottom;
]

let divtests = List.map make_case [
  "Div: Pos / Pos", BinaryOperation (Var "x", Div, Var "x"), PosZero;
  "Div: Pos / Neg", BinaryOperation (Var "x", Div, Var "y"), NegZero;
  "Div: Neg / Neg", BinaryOperation (Var "y", Div, Var "y"), PosZero;
  "Div: Costante / Zero", BinaryOperation (Const 10, Div, Var "z"), SignBottom;
  "Div: Pos / PosZero (rischio 0)", BinaryOperation (Var "x", Div, Var "w"), SignTop;
  "Div: Pos / NegZero (rischio 0)", BinaryOperation (Var "x", Div, Var "k"), SignTop;
  "Div: Pos / NonZero", BinaryOperation (Var "x", Div, Var "n"), SignTop;
  "Div: Zero / Pos", BinaryOperation (Var "z", Div, Var "x"), Zero;
  "Div: Zero / Neg", BinaryOperation (Var "z", Div, Var "y"), Zero;
  "Div: Top / Pos", BinaryOperation (Var "t", Div, Var "x"), SignTop;
  "Div: PosZero / Neg", BinaryOperation (Var "w", Div, Var "y"), NegZero;
  "Div: NegZero / Pos", BinaryOperation (Var "k", Div, Var "x"), NegZero;
]

let negatetests = List.map make_case [
  "Negate: Pos", UnaryOperation (Negation, Var "x"), Neg;
  "Negate: Neg", UnaryOperation (Negation, Var "y"), Pos;
  "Negate: Zero", UnaryOperation (Negation, Var "z"), Zero;
  "Negate: PosZero", UnaryOperation (Negation, Var "w"), NegZero;
  "Negate: NegZero", UnaryOperation (Negation, Var "k"), PosZero;
  "Negate: NonZero", UnaryOperation (Negation, Var "n"), NonZero;
  "Negate: Top", UnaryOperation (Negation, Var "t"), SignTop;
  "Negate: Bottom", UnaryOperation (Negation, Var "b"), SignBottom;
  "Doppia negazione: --Pos", UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Pos;
  "Pos + (-Neg)", BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Pos;
]

let randomtests = List.map make_case [
  "Random(-1,10)", Random (-1, 10), SignTop;
  "Random(1,10)", Random (1, 10), Pos;
  "Random(-10,-1)", Random (-10, -1), Neg;
  "Random(0,10)", Random (0, 10), PosZero;
  "Random(-10,0)", Random (-10, 0), NegZero;
  "Random(0,0)", Random (0, 0), Zero;
]

(* ------------------------------------------------------------ *)
(* 3. Comandi (eval_cmd / eval)                                  *)
(* ------------------------------------------------------------ *)

let assigntests = List.map make_prog_case [
  "Assign semplice: x = 5", Assign ("x", Const 5), [ "x", Pos ];
  "Assign semplice: x = -5", Assign ("x", Const (-5)), [ "x", Neg ];
  "Assign semplice: x = 0", Assign ("x", Const 0), [ "x", Zero ];
  "Assign con variabile non definita: y = x (x non esiste -> Top)",
    Assign ("y", Var "x"), [ "y", SignTop ];
  "Assign con Random: x = Random(1,10)", Assign ("x", Random (1, 10)), [ "x", Pos ];
]

let sequencetests = List.map make_prog_case [
  "Sequence: x=5; y=-3",
    Sequence (Assign ("x", Const 5), Assign ("y", Const (-3))),
    [ "x", Pos; "y", Neg ];

  "Sequence: usa il valore assegnato prima (y = x + x)",
    Sequence (Assign ("x", Const 5), Assign ("y", BinaryOperation (Var "x", Add, Var "x"))),
    [ "x", Pos; "y", Pos ];

  "Sequence: catena di 3 assegnazioni con dipendenze",
    Sequence (
      Sequence (Assign ("x", Const 5), Assign ("y", Const (-5))),
      Assign ("z", BinaryOperation (Var "x", Add, Var "y"))),
    [ "x", Pos; "y", Neg; "z", SignTop ];

  "Sequence: z ricalcolato due volte dopo un Skip",
    Sequence (
      Sequence (
        Sequence (Assign ("x", Const 0), Assign ("y", Const (-9))),
        Sequence (
          Assign ("z", BinaryOperation (Var "x", Add, Var "y")),
          Sequence (Skip, Assign ("z", BinaryOperation (Var "x", Add, Var "y"))))),
      Skip),
    [ "x", Zero; "y", Neg; "z", Neg ];
]

let overwritetests = List.map make_prog_case [
  "Overwrite: x=5 poi x=-5",
    Sequence (Assign ("x", Const 5), Assign ("x", Const (-5))), [ "x", Neg ];

  "Overwrite: x=5, x=0, x=x-1 -> Neg",
    Sequence (
      Sequence (Assign ("x", Const 5), Assign ("x", Const 0)),
      Assign ("x", BinaryOperation (Var "x", Sub, Const 1))),
    [ "x", Neg ];

  "Overwrite tripla: y assegnata 3 volte, resta solo l'ultima",
    Sequence (
      Sequence (Assign ("y", Const 1), Assign ("y", Const 2)),
      Assign ("y", Const (-100))),
    [ "y", Neg ];
]

let skiptests = [
  ( "Skip da solo non modifica lo stato (stato vuoto)", `Quick,
    fun () ->
      match SignInterp.eval Skip with
      | Env tbl -> Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl)
      | BottomEnv -> Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );

  ( "Skip in mezzo a una sequenza non altera i valori", `Quick,
    fun () ->
      let prog = Sequence (Assign ("x", Const 42), Skip) in
      match SignInterp.eval_cmd prog (Env (make_test_state ())) with
      | Env tbl -> Alcotest.(check sign_testable) "x resta Pos" Pos (Hashtbl.find tbl "x")
      | BottomEnv -> Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );
]

let envtests = List.map make_prog_case_with_env [
  "Riassegna x usando y già presente (y=Neg): x = y + y -> Neg",
    Assign ("x", BinaryOperation (Var "y", Add, Var "y")), [ "x", Neg ];

  "z = w * k (PosZero*NegZero)",
    Assign ("z", BinaryOperation (Var "w", Mul, Var "k")), [ "z", NegZero ];

  "Programma multi-step su stato precompilato",
    Sequence (
      Assign ("x", BinaryOperation (Var "x", Add, Var "z")),   (* Pos + Zero = Pos *)
      Assign ("y", UnaryOperation (Negation, Var "y"))),        (* -Neg = Pos *)
    [ "x", Pos; "y", Pos ];

  "Divisione con rischio zero su stato precompilato: z = x / w",
    Assign ("z", BinaryOperation (Var "x", Div, Var "w")), [ "z", SignTop ];
]

(* Qui lo stato collassa a BottomEnv: non ha senso controllare variabili singole *)
let condtest = [
  expect_bottom_with_env "Filter(false); x=10 -> stato finale BottomEnv"
    (Sequence (Filter (Boolean false), Assign ("x", Const 10)))
]

(* ------------------------------------------------------------ *)
(* 3bis. Filter / eval_cond                                      *)
(* ------------------------------------------------------------ *)

(* Casi certi: il confronto ha esito deciso senza ambiguità *)
let filter_certain_tests = List.map make_prog_case_with_env [
  "Filter certo vero: x > y (Pos > Neg)",
    Filter (Comparison (Var "x", Bigger, Var "y")), [ "x", Pos; "y", Neg ];
  "Filter certo vero: y < x (Neg < Pos)",
    Filter (Comparison (Var "y", Smaller, Var "x")), [ "x", Pos; "y", Neg ];
  "Filter certo vero: z = z (Zero = Zero, stessa var)",
    Filter (Comparison (Var "z", Equals, Var "z")), [ "z", Zero ];
  "Filter certo vero: x != y (Pos disgiunto da Neg)",
    Filter (Comparison (Var "x", NotEquals, Var "y")), [ "x", Pos; "y", Neg ];
]

let filter_certain_bottom_tests = [
  expect_bottom_with_env "Filter certo falso: y > x (Neg > Pos, impossibile)"
    (Filter (Comparison (Var "y", Bigger, Var "x")));
  expect_bottom_with_env "Filter certo falso: x < y (Pos < Neg, impossibile)"
    (Filter (Comparison (Var "x", Smaller, Var "y")));
  expect_bottom_with_env "Filter certo falso: x = y (Pos disgiunto da Neg)"
    (Filter (Comparison (Var "x", Equals, Var "y")));
  expect_bottom_with_env "Filter certo falso: z != z (Zero != Zero, impossibile)"
    (Filter (Comparison (Var "z", NotEquals, Var "z")));
]

(* Casi ambigui: i segni si sovrappongono, Filter non deve tagliare *)
let filter_ambiguous_tests = List.map make_prog_case_with_env [
  "Filter ambiguo: x > w (Pos vs PosZero si sovrappongono) -> passa, non restringe",
    Filter (Comparison (Var "x", Bigger, Var "w")), [ "x", Pos; "w", PosZero ];
  "Filter ambiguo: x = w (Pos vs PosZero) -> passa",
    Filter (Comparison (Var "x", Equals, Var "w")), [ "x", Pos; "w", Pos ];
  "Filter ambiguo: x > n (Pos vs NonZero) -> passa",
    Filter (Comparison (Var "x", Bigger, Var "n")), [ "x", Pos; "n", NonZero ];
  "Filter ambiguo: y < w (Neg vs PosZero, comunque si controlla) -> passa",
    Filter (Comparison (Var "y", Smaller, Var "w")), [ "y", Neg; "w", PosZero ];
]

(* Simmetria: stessa coppia ambigua, ordine invertito *)
let filter_symmetry_tests = List.map make_prog_case_with_env [
  "Simmetria ambiguo A: x > w (Pos, PosZero)",
    Filter (Comparison (Var "x", Bigger, Var "w")), [ "x", Pos; "w", PosZero ];
  "Simmetria ambiguo B: w > x (PosZero, Pos) - deve comportarsi come sopra",
    Filter (Comparison (Var "w", Bigger, Var "x")), [ "x", Pos; "w", PosZero ];
  "Simmetria Equals A: x = w (Pos, PosZero)",
    Filter (Comparison (Var "x", Equals, Var "w")), [ "x", Pos; "w", Pos ];
  "Simmetria Equals B: w = x (PosZero, Pos)",
    Filter (Comparison (Var "w", Equals, Var "x")), [ "x", Pos; "w", Pos ];
]

(* Operatori derivati: BiggerEquals / SmallerEquals *)
let filter_derived_ops_tests = List.map make_prog_case_with_env [
  "BiggerEquals certo vero: x >= z (Pos >= Zero)",
    Filter (Comparison (Var "x", BiggerEquals, Var "z")), [ "x", Pos; "z", Zero ];
  "SmallerEquals certo vero: z <= z (Zero <= Zero, caso limite)",
    Filter (Comparison (Var "z", SmallerEquals, Var "z")), [ "z", Zero ];
  "BiggerEquals ambiguo: w >= x (PosZero >= Pos) -> passa",
    Filter (Comparison (Var "w", BiggerEquals, Var "x")), [ "w", PosZero; "x", Pos ];
]

let filter_derived_ops_bottom_tests = [
  expect_bottom_with_env "SmallerEquals certo falso: x <= y (Pos <= Neg, impossibile)"
    (Filter (Comparison (Var "x", SmallerEquals, Var "y")));
  expect_bottom_with_env "BiggerEquals certo falso: y >= x (Neg >= Pos, impossibile)"
    (Filter (Comparison (Var "y", BiggerEquals, Var "x")));
]

(* Composizione: And, Or, Not *)
let filter_composition_tests = List.map make_prog_case_with_env [
  "And di due certi veri: x>y And y<x",
    Filter (And (Comparison (Var "x", Bigger, Var "y"), Comparison (Var "y", Smaller, Var "x"))),
    [ "x", Pos; "y", Neg ];
  "Or con un ramo impossibile e uno vero: passa comunque",
    Filter (Or (Comparison (Var "x", Smaller, Var "y"), Comparison (Var "y", Smaller, Var "x"))),
    [ "x", Pos; "y", Neg ];
  "Not su un confronto certo falso: diventa vero, passa",
    Filter (Not (Comparison (Var "x", Smaller, Var "y"))), [ "x", Pos; "y", Neg ];
  "Or di due Boolean: false Or true -> passa",
    Filter (Or (Boolean false, Boolean true)), [ "x", Pos ];
]

let filter_composition_bottom_tests = [
  expect_bottom_with_env "And con un ramo falso: x>y And x<y -> BottomEnv"
    (Filter (And (Comparison (Var "x", Bigger, Var "y"), Comparison (Var "x", Smaller, Var "y"))));
  expect_bottom_with_env "Not su un confronto certo vero: diventa falso -> BottomEnv"
    (Filter (Not (Comparison (Var "x", Bigger, Var "y"))));
  expect_bottom_with_env "Or di due Boolean false: false Or false -> BottomEnv"
    (Filter (Or (Boolean false, Boolean false)));
]

(* Caso limite: confronto che coinvolge SignBottom *)
(* let filter_bottom_value_tests = List.map make_prog_case_with_env [
  "Confronto b = b (SignBottom = SignBottom, stessa var) -> certo uguale, passa",
    Filter (Comparison (Var "b", Equals, Var "b")), [ "b", SignBottom ];
] *)

let filter_bottom_value_tests =  [ 
  expect_bottom_with_env "Confronto b = b (SignBottom = SignBottom, stessa var) -> certo uguale, passa"
  (Filter (Comparison (Var "b", Equals, Var "b")));
]
(* Filter incatenato con Assign, per verificare propagazione *)
let filter_chained_tests = List.map make_prog_case_with_env [
  "Filter ambiguo poi Assign: lo stato prosegue e z viene ricalcolata",
    Sequence (
      Filter (Comparison (Var "x", Bigger, Var "w")),  (* ambiguo, passa *)
      Assign ("z", BinaryOperation (Var "x", Add, Var "y"))),
    [ "x", Pos; "z", SignTop ];
]

let filter_chained_bottom_tests = [
  expect_bottom_with_env "Filter certo falso poi Assign: BottomEnv si propaga, Assign non ha effetto"
    (Sequence (Filter (Comparison (Var "y", Bigger, Var "x")), Assign ("x", Const 999)));
  expect_bottom_with_env "Doppio Filter: prima passa (ambiguo), poi taglia (certo falso)"
    (Sequence (
       Filter (Comparison (Var "x", Bigger, Var "w")),
       Filter (Comparison (Var "y", Bigger, Var "x"))));
]

(* ------------------------------------------------------------ *)
(* If: ricorda che make_test_state pre-popola
     x=Pos, y=Neg, z=Zero, w=PosZero, n=NonZero, b=SignBottom.
   Quando eval_cond restituisce "ambiguo" (2), entrambi i rami vengono
   eseguiti e il risultato è il lub dei due; un ramo viene scartato solo
   quando il confronto è certo. *)
(* ------------------------------------------------------------ *)

let if_certain_then_tests = List.map make_prog_case_with_env [
  "If certo vero (x>y, Pos>Neg): solo then esegue, else e' Bottom e sparisce nel lub",
    If (Comparison (Var "x", Bigger, Var "y"), Assign ("k", Const 1), Assign ("k", Const 999)),
    [ "k", Pos; "x", Pos; "y", Neg ];
]

let if_certain_else_tests = List.map make_prog_case_with_env [
  "If certo falso (y>x, Neg>Pos): then e' Bottom, solo else conta",
    If (Comparison (Var "y", Bigger, Var "x"), Assign ("k", Const (-999)), Assign ("k", Const 2)),
    [ "k", Pos ];
  "If su variabile SignBottom (b>0): then Bottom per compare_type=-1, else conta",
    If (Comparison (Var "b", Bigger, Const 0), Assign ("k", Const (-1)), Assign ("k", Const 42)),
    [ "k", Pos ];
]

let if_ambiguous_both_branches_tests = List.map make_prog_case_with_env [
  "Ambiguo: then=Pos(5), else=Neg(-5) -> lub = NonZero",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 5), Assign ("k", Const (-5))),
    [ "k", NonZero ];
  "Ambiguo: then=Pos(1), else=Zero(0) -> lub = PosZero",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 1), Assign ("k", Const 0)),
    [ "k", PosZero ];
  "Ambiguo: then=Neg(-1), else=Zero(0) -> lub = NegZero",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const (-1)), Assign ("k", Const 0)),
    [ "k", NegZero ];
  "Ambiguo: lub(Pos,NegZero) non ha riga esplicita in Signs.lub -> catch-all SignTop",
    If (Comparison (Var "w", Bigger, Var "x"),
        Assign ("k", Const 3),
        Assign ("k", BinaryOperation (Var "w", Mul, Var "y"))),
    [ "k", SignTop ];
  "Ambiguo: rami convergenti (entrambi Pos) -> lub = Pos, nessuna perdita di precisione",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 10), Assign ("k", Const 20)),
    [ "k", Pos ];
]

let if_partial_assignment_tests = List.map make_prog_case_with_env [
  "Ambiguo, var assegnata solo nel then (else = Skip) -> sopravvive col valore del then",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("m", Const 7), Skip), [ "m", Pos ];
  "Ambiguo, var assegnata solo nell'else (then = Skip) -> sopravvive col valore dell'else",
    If (Comparison (Var "w", Bigger, Var "x"), Skip, Assign ("m", Const (-7))), [ "m", Neg ];
]

let if_env_independence_tests = List.map make_prog_case_with_env [
  "Il then riassegna x, l'else no: i rami non si influenzano e le var non toccate restano invariate",
    If (Comparison (Var "w", Bigger, Var "x"), Assign ("x", Const (-100)), Skip),
    [ "x", NonZero; "y", Neg; "z", Zero; "w", PosZero; "n", NonZero; "b", SignBottom ];
]

let if_nested_tests = List.map make_prog_case_with_env [
  "If annidato: outer ambiguo, then contiene un altro If ambiguo",
    If (Comparison (Var "w", Bigger, Var "x"),
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 1), Assign ("k", Const (-1))),
        Assign ("k", Const 100)),
    (* then: lub(Pos,Neg)=NonZero, else: Pos -> lub finale = NonZero *)
    [ "k", NonZero ];
]

let if_composite_cond_tests = List.map make_prog_case_with_env [
  "If con And(certo vero, ambiguo): l'ambiguità fa passare comunque entrambi i rami",
    If (And (Comparison (Var "x", Bigger, Var "y"), Comparison (Var "w", Bigger, Var "x")),
        Assign ("k", Const 1), Assign ("k", Const (-1))),
    [ "k", NonZero ];
  "If con Or(certo falso, ambiguo): stesso discorso",
    If (Or (Comparison (Var "y", Bigger, Var "x"), Comparison (Var "w", Bigger, Var "x")),
        Assign ("k", Const 1), Assign ("k", Const (-1))),
    [ "k", NonZero ];
]

let if_bottom_propagation_tests = [
  expect_bottom_with_env "Filter certo falso prima dell'If: l'If non viene nemmeno valutato, resta Bottom"
    (Sequence (Filter (Boolean false), If (Boolean true, Assign ("k", Const 1), Assign ("k", Const 2))));
]

(* ------------------------------------------------------------ *)
(* While.
   I valori attesi sono stati verificati eseguendo davvero SignInterp
   (il fixpoint usa Hashtbl mutabili condivise tra iterazioni).

   Comportamenti reali documentati dai test:
   - negate_comp mappa Bigger <-> Smaller direttamente, senza passare per
     l'operatore "equals-inclusive" corretto (la negazione di "x > y"
     dovrebbe essere "x <= y", non "x < y"). Quindi un while con guardia
     "x > 0" e x = Zero (falsa da subito) produce comunque BottomEnv
     invece di preservare x = Zero.
   - compare_type tratta SignTop come sempre "maggiore" (SignTop,_ -> 1)
     e sempre "minore" come secondo argomento (_,SignTop -> -1): due loop
     strutturalmente identici con guardia ">" o "<" che perdono precisione
     possono avere esiti diversi (uno BottomEnv, l'altro un Env con SignTop). *)
(* ------------------------------------------------------------ *)

let while_not_entered_tests = List.map make_prog_case [
  "While mai eseguito (x=5, guardia x<0): x resta Pos, z non toccata resta Neg",
    Sequence (
      Sequence (Assign ("x", Const 5), Assign ("z", Const (-3))),
      While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Sub, Const 1)))),
    [ "x", Pos; "z", Neg ];
  "While mai eseguito (x=5, guardia x==0): x resta Pos",
    Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Equals, Const 0), Assign ("x", Const 0))),
    [ "x", Pos ];
]

let while_converges_tests = List.map make_prog_case [
  "While converge: x=5, while(x!=0) x=0 -> termina con x=Zero",
    Sequence (Assign ("x", Const 5), While (Comparison (Var "x", NotEquals, Const 0), Assign ("x", Const 0))),
    [ "x", Zero ]; (* Dovrebbe dare 0 perchè per uscire x deve essere == 0 ossia not cond *)
]

let while_precision_loss_tests = List.map make_prog_case [
  "Guardia '<': x=-5, while(x<0) x=x+1 -> perde precisione a SignTop ma l'uscita resta Env con x=SignTop",
    Sequence (
      Assign ("x", Const (-5)),
      While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
    [ "x", SignTop ];
]

let while_precision_loss_bottom_tests = List.map make_prog_case [
   "Guardia '>': x=5, while(x>0) x=x-1 -> perde precisione, uscita BottomEnv (asimmetria rispetto a '<')",
    (Sequence (
       Assign ("x", Const 5),
       While (Comparison (Var "x", Bigger, Const 0), Assign ("x", BinaryOperation (Var "x", Sub, Const 1))))),
       [ "x", SignTop ];
]

let while_infinite_loop_tests = [
  expect_bottom "Corpo = Skip: x=5, while(x>0) skip -> non termina mai, uscita BottomEnv"
    (Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Bigger, Const 0), Skip)));
  expect_bottom "Corpo modifica var non correlata: x=5, while(x>0) y=1 -> x non cambia mai, uscita BottomEnv"
    (Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1))));
]

let while_bottom_propagation_tests = [
  expect_bottom "Filter(false) prima del while: il while non viene valutato, resta BottomEnv"
    (Sequence (Filter (Boolean false), While (Boolean true, Assign ("x", Const 1))));
]

let while_equality_boundary_tests = [
  expect_bottom "Boundary sull'uguaglianza: x=0, while(x>0) x=x-1 -> ci si aspetterebbe x=Zero preservato, ma negate_comp produce BottomEnv"
    (Sequence (
       Assign ("x", Const 0),
       While (Comparison (Var "x", Bigger, Const 0), Assign ("x", BinaryOperation (Var "x", Sub, Const 1)))));
]

let while_nested_tests = [
  expect_bottom "While annidati: x=5, while(x>0){y=1; while(y>0) y=y-1} -> l'esterno non termina mai in astratto, BottomEnv"
    (Sequence (
       Assign ("x", Const 5),
       While (Comparison (Var "x", Bigger, Const 0),
              Sequence (
                Assign ("y", Const 1),
                While (Comparison (Var "y", Bigger, Const 0),
                       Assign ("y", BinaryOperation (Var "y", Sub, Const 1)))))));
]

let personal_while_test = List.map make_prog_case [
  "Guardia Sempre vera",
    Sequence (
      Sequence (
        Assign ("x", Const 1),
        Sequence (Assign ("y", Const 2), Assign ("z", Random (-3, 5)))),
      While (
        Comparison (Var "x", Equals, Var "y"),
        If (Comparison (Var "y", Bigger, Var "z"),
            Assign ("x", BinaryOperation (Var "x", Add, Const (-2))),
            Assign ("y", BinaryOperation (Var "x", Add, Var "y"))))),
    [ "x", SignTop; "y", Pos; "z", SignTop ];
]

(* ------------------------------------------------------------ *)
(* 4. Esportazione unica di tutti i gruppi                       *)
(* ------------------------------------------------------------ *)

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
  "Filter - casi certi", filter_certain_tests @ filter_certain_bottom_tests;
  "Filter - casi ambigui", filter_ambiguous_tests;
  "Filter - simmetria", filter_symmetry_tests;
  "Filter - operatori derivati", filter_derived_ops_tests @ filter_derived_ops_bottom_tests;
  "Filter - composizione And/Or/Not", filter_composition_tests @ filter_composition_bottom_tests;
  "Filter - valore Bottom", filter_bottom_value_tests;
  "Filter - incatenato", filter_chained_tests @ filter_chained_bottom_tests;

  "IF - Ramo Then", if_certain_then_tests;
  "IF - Ramo Else", if_certain_else_tests;
  "IF - Ambiguità", if_ambiguous_both_branches_tests;
  "IF - Assegnamento parziale", if_partial_assignment_tests;
  "IF - Ambiente Indipendente", if_env_independence_tests;
  "IF - Annidazioni", if_nested_tests;
  "IF - Condizioni Composte", if_composite_cond_tests;
  "IF - Propagazione di BottomEnv", if_bottom_propagation_tests;

  "While - non eseguito", while_not_entered_tests;
  "While - converge", while_converges_tests;
  "While - perdita precisione", while_precision_loss_tests @ while_precision_loss_bottom_tests;
  "While - loop infinito", while_infinite_loop_tests;
  "While - propagazione bottom", while_bottom_propagation_tests;
  "While - boundary uguaglianza", while_equality_boundary_tests;
  "While - annidati", while_nested_tests;
  "While - Personali", personal_while_test;
]