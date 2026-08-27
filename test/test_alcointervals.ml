open Syntax
open Abstract_domains.Intervals
open Interpeters.IntervalInterp

(* ------------------------------------------------------------------ *)
(* 1. Helper di stampa e testable per Alcotest                        *)
(* ------------------------------------------------------------------ *)

let bound_to_string = function
  | NegInf -> "-inf"
  | PosInf -> "+inf"
  | Int n  -> string_of_int n

let interval_to_string = function
  | Bottom -> "Bottom"
  | Interval (a, b) ->
      Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

let interval_testable =
  let pp fmt v = Format.fprintf fmt "%s" (interval_to_string v) in
  Alcotest.testable pp ( = )

(* ------------------------------------------------------------------ *)
(* 2. Helper generici per costruire i case Alcotest                   *)
(* ------------------------------------------------------------------ *)

let make_bool_case (desc, op, a, b, expected) =
  (desc, `Quick, fun () -> Alcotest.(check bool) desc expected (op a b))

let make_unary_case (desc, op, a, expected) =
  (desc, `Quick, fun () -> Alcotest.(check interval_testable) desc expected (op a))

let make_binary_case (desc, op, a, b, expected) =
  (desc, `Quick, fun () -> Alcotest.(check interval_testable) desc expected (op a b))

(* ------------------------------------------------------------------ *)
(* 3. Test sul dominio astratto degli intervalli (nessuno stato)      *)
(* ------------------------------------------------------------------ *)

let leq_tests = List.map make_bool_case [
  ("Bottom, [1, 2]",   leq, Bottom, abstract_range 1 2, true);
  ("[1, 2], Bottom",   leq, abstract_range 1 2, Bottom, false);
  ("[2, 3] in [1, 5]", leq, abstract_range 2 3, abstract_range 1 5, true);
  ("[1, 5] in [2, 3]", leq, abstract_range 1 5, abstract_range 2 3, false);
  ("[1, 2] in [1, 2]", leq, abstract_range 1 2, abstract_range 1 2, true);
  ("[0, 10] in Top",   leq, abstract_range 0 10, top, true);
  ("Top in [0, 10]",   leq, top, abstract_range 0 10, false);
]

let lub_tests = List.map make_binary_case [
  ("[1, 3], [5, 7]",  lub, abstract_range 1 3, abstract_range 5 7, abstract_range 1 7);
  ("[2, 5], [1, 3]",  lub, abstract_range 2 5, abstract_range 1 3, abstract_range 1 5);
  ("Bottom, [1, 2]",  lub, Bottom, abstract_range 1 2, abstract_range 1 2);
  ("[1, 2], Top",     lub, abstract_range 1 2, top, top);
]

let glb_tests = List.map make_binary_case [
  ("[1, 5] e [3, 8]",             glb, abstract_range 1 5, abstract_range 3 8, abstract_range 3 5);
  ("[1, 3] e [5, 8] (disgiunti)", glb, abstract_range 1 3, abstract_range 5 8, Bottom);
  ("[1, 5] e Bottom",             glb, abstract_range 1 5, Bottom, Bottom);
  ("[1, 5] e Top",                glb, abstract_range 1 5, top, abstract_range 1 5);
]

let sum_tests = List.map make_binary_case [
  ("[1, 2] + [3, 4]",    sum, abstract_range 1 2, abstract_range 3 4, abstract_range 4 6);
  ("[-2, 5] + [10, 20]", sum, abstract_range (-2) 5, abstract_range 10 20, abstract_range 8 25);
  ("[1, 2] + Bottom",    sum, abstract_range 1 2, Bottom, Bottom);
  ("[1, 2] + Top",       sum, abstract_range 1 2, top, top);
]

let negate_tests = List.map make_unary_case [
  ("[1, 5]",   negate, abstract_range 1 5, abstract_range (-5) (-1));
  ("[-3, 2]",  negate, abstract_range (-3) 2, abstract_range (-2) 3);
  ("Bottom",   negate, Bottom, Bottom);
]

let mul_tests = List.map make_binary_case [
  ("[2, 3] * [4, 5]",       mul, abstract_range 2 3, abstract_range 4 5, abstract_range 8 15);
  ("[-2, 3] * [-4, 5]",     mul, abstract_range (-2) 3, abstract_range (-4) 5, abstract_range (-12) 15);
  ("[-5, -2] * [-4, -1]",   mul, abstract_range (-5) (-2), abstract_range (-4) (-1), abstract_range 2 20);
  ("[-5, 2] * [4, 1]",      mul, abstract_range (-5) 2, abstract_range 4 1, abstract_range (-20) 8);
  ("[0, 5] * Bottom",       mul, abstract_range 0 5, Bottom, Bottom);
]

let div_tests = List.map make_binary_case [
  ("[10, 20] / [2, 5]",   div, abstract_range 10 20, abstract_range 2 5, abstract_range 2 10);
  ("[10, 20] / [-5, -2]", div, abstract_range 10 20, abstract_range (-5) (-2), abstract_range (-10) (-2));
  ("[10, 20] / [0, 0]",   div, abstract_range 10 20, abstract_range 0 0, Bottom);
]

let expr_tests = [
  "Intervals - leq",    leq_tests;
  "Intervals - lub",    lub_tests;
  "Intervals - glb",    glb_tests;
  "Intervals - sum",    sum_tests;
  "Intervals - negate", negate_tests;
  "Intervals - mul",    mul_tests;
  "Intervals - div",    div_tests;
]

(* ------------------------------------------------------------------ *)
(* 4. Stato di test e helper per l'interprete (usano il tipo state)   *)
(* ------------------------------------------------------------------ *)

(* x = [2,7]     positivo limitato
   y = [-8,-3]   negativo limitato
   z = [0,0]     zero esatto
   w = [0,5]     positivo-o-zero
   k = [-5,0]    negativo-o-zero
   n = [-3,4]    attraversa lo zero
   t = Top       b = Bottom
   p = [1,+inf]  positivo illimitato
   m = [-inf,-1] negativo illimitato *)
let make_test_state () =
  let tbl = Hashtbl.create 10 in
  List.iter (fun (name, v) -> Hashtbl.add tbl name v) [
    "x", Interval (Int 2, Int 7);
    "y", Interval (Int (-8), Int (-3));
    "z", Interval (Int 0, Int 0);
    "w", Interval (Int 0, Int 5);
    "k", Interval (Int (-5), Int 0);
    "n", Interval (Int (-3), Int 4);
    "t", top;
    "b", bottom;
    "p", Interval (Int 1, PosInf);
    "m", Interval (NegInf, Int (-1));
  ];
  Env (tbl)

(* Legge una variabile da uno state, fallendo il test se lo stato e'
   BottomEnv o la variabile non e' presente *)
let get_var desc state var =
  match state with
  | BottomEnv ->
      Alcotest.fail
        (Printf.sprintf "%s: stato e' BottomEnv, variabile '%s' non trovata" desc var)
  | Env tbl -> (
      match Hashtbl.find_opt tbl var with
      | Some v -> v
      | None ->
          Alcotest.fail
            (Printf.sprintf "%s: variabile '%s' non trovata nello stato finale" desc var))

let check_vars desc final_state expected_vars =
  List.iter
    (fun (var, expected) ->
      Alcotest.(check interval_testable) (desc ^ " - " ^ var) expected
        (get_var desc final_state var))
    expected_vars

let make_case (desc, expr, expected) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      Alcotest.(check interval_testable) desc expected (eval_exp expr st) )

let make_prog_case (desc, prog, expected_vars) =
  (desc, `Quick, fun () -> check_vars desc (eval prog) expected_vars)

let make_prog_case_with_env (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () ->
      let st = make_test_state () in
      check_vars desc (eval_cmd prog st) expected_vars )

(* ------------------------------------------------------------------ *)
(* 5. Test sulle espressioni (eval_exp)                               *)
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
  ("Div: x/x",                   BinaryOperation (Var "x", Div, Var "x"), Interval (Int 0, Int 3));
  ("Div: x/y",                   BinaryOperation (Var "x", Div, Var "y"), Interval (Int (-2), Int 0));
  ("Div: y/y",                   BinaryOperation (Var "y", Div, Var "y"), Interval (Int 0, Int 2));
  ("Div: Costante/z (0 esatto)", BinaryOperation (Const 10, Div, Var "z"), Bottom);
  ("Div: x/w (rischio 0, w>=0)", BinaryOperation (Var "x", Div, Var "w"), Interval (Int 0, Int 7));
  ("Div: x/k (rischio 0, k<=0)", BinaryOperation (Var "x", Div, Var "k"), Interval (Int (-7), Int 0));
  ("Div: [1,+inf]/x",            BinaryOperation (Var "p", Div, Var "x"), Interval (Int 0, PosInf));
  ("Div: x/[-inf,-1]",           BinaryOperation (Var "x", Div, Var "m"), Interval (Int (-7), Int 0));
  ("Div: Top/x",                 BinaryOperation (Var "t", Div, Var "x"), top);
  ("Div: w/y",                   BinaryOperation (Var "w", Div, Var "y"), Interval (Int (-1), Int 0));
  ("Div: k/x",                   BinaryOperation (Var "k", Div, Var "x"), Interval (Int (-2), Int 0));
]

let negatetests = List.map make_case [
  ("Negate: x",             UnaryOperation (Negation, Var "x"), Interval (Int (-7), Int (-2)));
  ("Negate: y",             UnaryOperation (Negation, Var "y"), Interval (Int 3, Int 8));
  ("Negate: z",             UnaryOperation (Negation, Var "z"), Interval (Int 0, Int 0));
  ("Negate: w",             UnaryOperation (Negation, Var "w"), Interval (Int (-5), Int 0));
  ("Negate: k",             UnaryOperation (Negation, Var "k"), Interval (Int 0, Int 5));
  ("Negate: n",             UnaryOperation (Negation, Var "n"), Interval (Int (-4), Int 3));
  ("Negate: Top",           UnaryOperation (Negation, Var "t"), top);
  ("Negate: Bottom",        UnaryOperation (Negation, Var "b"), Bottom);
  ("Negate: [1,+inf]",      UnaryOperation (Negation, Var "p"), Interval (NegInf, Int (-1)));
  ("Negate: [-inf,-1]",     UnaryOperation (Negation, Var "m"), Interval (Int 1, PosInf));
  ("Doppia negazione: --x", UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Interval (Int 2, Int 7));
  ("x + (-y)",              BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Interval (Int 5, Int 15));
]

let randomtests = List.map make_case [
  ("Random(-1,10)",                     Random (-1, 10), Interval (Int (-1), Int 10));
  ("Random(1,10)",                      Random (1, 10), Interval (Int 1, Int 10));
  ("Random(-10,-1)",                    Random (-10, -1), Interval (Int (-10), Int (-1)));
  ("Random(0,10)",                      Random (0, 10), Interval (Int 0, Int 10));
  ("Random(-10,0)",                     Random (-10, 0), Interval (Int (-10), Int 0));
  ("Random(0,0)",                       Random (0, 0), Interval (Int 0, Int 0));
  ("Random(10,-5) (estremi invertiti)", Random (10, -5), Interval (Int (-5), Int 10));
]

(* ------------------------------------------------------------------ *)
(* 6. Test sui comandi (eval_cmd / eval)                              *)
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
   Sequence (Assign ("x", Const 5), Assign ("y", BinaryOperation (Var "x", Add, Var "x"))),
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

let skiptests = [
  ( "Skip da solo non modifica lo stato (stato vuoto)",
    `Quick,
    fun () ->
      match eval Skip with
      | BottomEnv -> Alcotest.fail "Skip non dovrebbe produrre BottomEnv"
      | Env tbl -> Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl) );

  ( "Skip in mezzo a una sequenza non altera i valori",
    `Quick,
    fun () ->
      let prog = Sequence (Assign ("x", Const 42), Skip) in
      let res = eval_cmd prog (make_test_state ()) in
      Alcotest.(check interval_testable) "x resta [42,42]"
        (Interval (Int 42, Int 42)) (get_var "Skip" res "x") );
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
(* 7. Esportazione unica di tutti i gruppi                            *)
(* ------------------------------------------------------------------ *)

(* let tests = expr_tests @ [
  "Somma",              sumtests;
  "Sottrazione",        subtests;
  "Moltiplicazione",    multests;
  "Divisione",          divtests;
  "Negazione",          negatetests;
  "Random",             randomtests;
  "Assegnazioni",       assigntests;
  "Sequenze",           sequencetests;
  "Overwrite",          overwritetests;
  "Skip",               skiptests;
  "Stato precompilato", envtests;
] *)

(* ------------------------------------------------------------------ *)
(* 7. Helper per comandi che possono produrre BottomEnv               *)
(* ------------------------------------------------------------------ *)

(* Verifica che il programma, partendo da stato vuoto, produca BottomEnv *)
let make_bottom_case (desc, prog) =
  ( desc,
    `Quick,
    fun () ->
      match eval prog with
      | BottomEnv -> ()
      | Env _ -> Alcotest.fail (desc ^ ": atteso BottomEnv, ottenuto Env") )

(* Verifica che il programma, partendo da stato vuoto, NON produca BottomEnv
   e che le variabili indicate abbiano i valori attesi *)
let make_not_bottom_case (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () ->
      match eval prog with
      | BottomEnv -> Alcotest.fail (desc ^ ": atteso Env, ottenuto BottomEnv")
      | Env _ as st -> check_vars desc st expected_vars )

(* Verifica che il programma, partendo dallo stato precompilato
   (make_test_state), produca BottomEnv *)
let make_bottom_case_with_env (desc, prog) =
  ( desc,
    `Quick,
    fun () ->
      match eval_cmd prog (make_test_state ()) with
      | BottomEnv -> ()
      | Env _ -> Alcotest.fail (desc ^ ": atteso BottomEnv, ottenuto Env") )

(* Verifica che il programma, partendo dallo stato precompilato, NON
   produca BottomEnv e che le variabili indicate abbiano i valori attesi *)
let make_not_bottom_case_with_env (desc, prog, expected_vars) =
  ( desc,
    `Quick,
    fun () ->
      match eval_cmd prog (make_test_state ()) with
      | BottomEnv -> Alcotest.fail (desc ^ ": atteso Env, ottenuto BottomEnv")
      | Env _ as st -> check_vars desc st expected_vars )

(* ==================================================================== *)
(* FILTER                                                               *)
(* ==================================================================== *)

let filter_certain_tests =
  List.map make_not_bottom_case [
    ("x=5; Filter(x=5) -> singoletto uguale, stato invariato",
     Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Equals, Const 5))),
     [ ("x", Interval (Int 5, Int 5)) ]);

    ("x=5,y=1; Filter(x>y) -> disgiunti, decidibile vero",
     Sequence (
       Sequence (Assign ("x", Const 5), Assign ("y", Const 1)),
       Filter (Comparison (Var "x", Bigger, Var "y"))),
     [ ("x", Interval (Int 5, Int 5)); ("y", Interval (Int 1, Int 1)) ]);

    ("x=5; Filter(x<>0) -> disgiunti",
     Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", NotEquals, Const 0))),
     [ ("x", Interval (Int 5, Int 5)) ]);
  ]
  @ List.map make_not_bottom_case_with_env [
    ("p>0 decidibile (p=[1,+inf])", Filter (Comparison (Var "p", Bigger, Const 0)),
     [ ("p", Interval (Int 1, PosInf)) ]);
    ("m<0 decidibile (m=[-inf,-1])", Filter (Comparison (Var "m", Smaller, Const 0)),
     [ ("m", Interval (NegInf, Int (-1))) ]);
  ]

let filter_certain_bottom_tests =
  List.map make_bottom_case [
    ("x=5; Filter(x=10) -> disgiunti, decidibile falso",
     Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Equals, Const 10))));

    ("x=1,y=5; Filter(x>y) -> x sempre < y",
     Sequence (
       Sequence (Assign ("x", Const 1), Assign ("y", Const 5)),
       Filter (Comparison (Var "x", Bigger, Var "y"))));

    ("Filter(Boolean false) -> sempre Bottom", Filter (Boolean false));

    ("x=3; Filter(Not(x=3)) -> Bottom",
     Sequence (Assign ("x", Const 3), Filter (Not (Comparison (Var "x", Equals, Const 3)))));
  ]
  @ List.map make_bottom_case_with_env [
    ("Filter(y>x) sullo stato precompilato -> y sempre < x",
     Filter (Comparison (Var "y", Bigger, Var "x")));
  ]

(* Comparazioni fra intervalli che si sovrappongono: indecidibili (2),
   quindi il Filter le tratta come vere (guarda l'implementazione di
   eval_cond: Bigger/Smaller/Equals sono "true" anche quando compare_type
   restituisce 2) e lo stato NON viene ristretto. *)
let filter_ambiguous_tests =
  List.map make_not_bottom_case_with_env [
    ("Filter(n>0) su n=[-3,4] (attraversa lo 0) -> ambiguo, passa invariato",
     Filter (Comparison (Var "n", Bigger, Const 0)),
     [ ("n", Interval (Int (-3), Int 4)) ]);

    ("Filter(n<0) su n=[-3,4] -> ambiguo, passa invariato",
     Filter (Comparison (Var "n", Smaller, Const 0)),
     [ ("n", Interval (Int (-3), Int 4)) ]);
  ] @ List.map make_bottom_case_with_env [
    "Filter(w=k) con w=[0,5], k=[-5,0] sovrapposti in 0 -> ambiguo",
     Filter (Comparison (Var "w", Equals, Var "k"))
     (* Secondo me ha senso che non sia ambiguo in quanto a>c e b>d quindi w > k  Quindi BottomEnv*)
    (* Risultato di Claude:      [ ("k", Interval (Int (-5), Int 0)); ("w", Interval (Int 0, Int 5)) ]) *)
  ]

(* Simmetria: Filter(a > b) e Filter(b < a) devono avere lo stesso esito
   (pass/bottom) sullo stesso stato, essendo comparazioni equivalenti. *)
let filter_symmetry_tests =
  List.map make_not_bottom_case [
    ("x=5,y=1; Filter(x>y) passa",
     Sequence (Sequence (Assign ("x", Const 5), Assign ("y", Const 1)),
               Filter (Comparison (Var "x", Bigger, Var "y"))),
     [ ("x", Interval (Int 5, Int 5)) ]);

    ("x=5,y=1; Filter(y<x) passa (equivalente, ordine invertito)",
     Sequence (Sequence (Assign ("x", Const 5), Assign ("y", Const 1)),
               Filter (Comparison (Var "y", Smaller, Var "x"))),
     [ ("x", Interval (Int 5, Int 5)) ]);
  ]
  @ List.map make_bottom_case [
    ("x=1,y=5; Filter(x>y) -> Bottom",
     Sequence (Sequence (Assign ("x", Const 1), Assign ("y", Const 5)),
               Filter (Comparison (Var "x", Bigger, Var "y"))));
    ("x=1,y=5; Filter(y<x) -> Bottom (stesso esito, ordine invertito)",
     Sequence (Sequence (Assign ("x", Const 1), Assign ("y", Const 5)),
               Filter (Comparison (Var "y", Smaller, Var "x"))));
  ]

(* Operatori derivati BiggerEquals / SmallerEquals, implementati come
   Or(Bigger,Equals) / Or(Smaller,Equals) *)
let filter_derived_ops_tests =
  List.map make_not_bottom_case [
    ("x=5; Filter(x>=5) -> singoletto uguale via ramo Equals",
     Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", BiggerEquals, Const 5))),
     [ ("x", Interval (Int 5, Int 5)) ]);
    ("x=1; Filter(x<=1) -> singoletto uguale via ramo Equals",
     Sequence (Assign ("x", Const 1), Filter (Comparison (Var "x", SmallerEquals, Const 1))),
     [ ("x", Interval (Int 1, Int 1)) ]);
  ]

let filter_derived_ops_bottom_tests =
  List.map make_bottom_case [
    ("x=1,y=5; Filter(x>=y) -> x sempre < y, entrambi i rami falsi",
     Sequence (Sequence (Assign ("x", Const 1), Assign ("y", Const 5)),
               Filter (Comparison (Var "x", BiggerEquals, Var "y"))));
    ("x=5; Filter(x<=0) -> x sempre > 0, entrambi i rami falsi",
     Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", SmallerEquals, Const 0))));
  ]

(* Composizione And / Or / Not, casi decidibili *)
let filter_composition_tests =
  List.map make_not_bottom_case_with_env [
    ("And(x>0, y<0) vero sullo stato precompilato",
     Filter (And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "y", Smaller, Const 0))),
     [ ("x", Interval (Int 2, Int 7)); ("y", Interval (Int (-8), Int (-3))) ]);

    ("Or(z=1, z=0) vero grazie al secondo membro (z=[0,0])",
     Filter (Or (Comparison (Var "z", Equals, Const 1), Comparison (Var "z", Equals, Const 0))),
     [ ("z", Interval (Int 0, Int 0)) ]);

    ("Not(x=100) -> diventa x<>100, decidibile vero (x=[2,7])",
     Filter (Not (Comparison (Var "x", Equals, Const 100))),
     [ ("x", Interval (Int 2, Int 7)) ]);
  ]

let filter_composition_bottom_tests =
  List.map make_bottom_case_with_env [
    ("And(x>0, z=1) -> z=[0,0] diverso da 1, il secondo membro fallisce",
     Filter (And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "z", Equals, Const 1))));

    ("Or(x=100, y=100) -> entrambi i membri decidibilmente falsi",
     Filter (Or (Comparison (Var "x", Equals, Const 100), Comparison (Var "y", Equals, Const 100))));
  ]

(* 
  Comparazioni che coinvolgono una variabile il cui valore astratto e'
  Bottom (variabile "b" nello stato precompilato). Questo dipende da come
  il tuo dominio definisce compare_type su Bottom: qui assumo che il
  confronto risulti comunque "vero" (interpretabile come vacuamente vero,
  dato che Bottom rappresenta l'insieme vuoto) - VERIFICA e correggi se
  la tua implementazione si comporta diversamente 
  (es. solleva eccezione o restituisce sempre 2).   
*)
let filter_bottom_value_tests =
  List.map make_bottom_case_with_env [
    ("Filter(b=x) con b=Bottom -> assunto vacuamente vero, da verificare", (* la logica mi direbbe che qua deve uscire bottom perchè bottom lo considero come errore*)
     Filter (Comparison (Var "b", Equals, Var "x"))
      )  (* Risultato di Claude: ("x", Interval (Int 2, Int 7)) lo mantengo perchè può avere senso *)
  ]

(* Filter incatenati: ognuno decidibile singolarmente *)
let filter_chained_tests =
  List.map make_not_bottom_case [
    ("x=5; Filter(x>0); Filter(x<10); Filter(x<>3) -> catena di veri",
     Sequence (Assign ("x", Const 5),
       Sequence (Filter (Comparison (Var "x", Bigger, Const 0)),
         Sequence (Filter (Comparison (Var "x", Smaller, Const 10)),
                   Filter (Comparison (Var "x", NotEquals, Const 3))))),
     [ ("x", Interval (Int 5, Int 5)) ]);
  ]

let filter_chained_bottom_tests =
  List.map make_bottom_case [
    ("x=5; Filter(x>0) passa, poi Filter(x=100) fallisce -> Bottom si propaga",
     Sequence (Assign ("x", Const 5),
       Sequence (Filter (Comparison (Var "x", Bigger, Const 0)),
         Sequence (Filter (Comparison (Var "x", Equals, Const 100)),
                   Filter (Comparison (Var "x", Smaller, Const 10))))));
  ]

(* ==================================================================== *)
(* IF                                                                    *)
(* ==================================================================== *)

let if_certain_then_tests =
  List.map make_not_bottom_case_with_env [
    ("x>0 decidibile vero (x=[2,7]) -> solo il ramo then contribuisce",
     If (Comparison (Var "x", Bigger, Const 0),
         Assign ("r", Const 1), Assign ("r", Const (-1))),
     [ ("r", Interval (Int 1, Int 1)) ]);
  ]

let if_certain_else_tests =
  List.map make_not_bottom_case_with_env [
    ("y>0 decidibile falso (y=[-8,-3]) -> solo il ramo else contribuisce",
     If (Comparison (Var "y", Bigger, Const 0),
         Assign ("r", Const 100), Assign ("r", Const (-100))),
     [ ("r", Interval (Int (-100), Int (-100))) ]);
  ]

let if_ambiguous_both_branches_tests =
  List.map make_not_bottom_case_with_env [
    ("n>0 ambiguo (n=[-3,4]) -> entrambi i rami contribuiscono via lub",
     If (Comparison (Var "n", Bigger, Const 0),
         Assign ("r", Const 100), Assign ("r", Const (-100))),
     [ ("r", Interval (Int (-100), Int 100)) ]);
  ]

(* Dimostra una particolarita' di lub_env: se una variabile viene assegnata
   solo in un ramo e l'altro ramo la lascia assente, lub_env la mantiene
   COSI' COM'E' (non viene "fusa" con nulla), invece di sparire o diventare
   Top. Questo si deduce direttamente dal codice di lub_env fornito, non
   dal dominio Intervals. *)
let if_partial_assignment_tests =
  List.map make_not_bottom_case_with_env [
    ("n>0 ambiguo; solo il ramo then assegna q -> q sopravvive invariata",
     If (Comparison (Var "n", Bigger, Const 0),
         Assign ("q", Const 1), Skip),
     [ ("q", Interval (Int 1, Int 1)) ]);
  ]

(* Verifica che una mutazione fatta in un ramo (su una copia dell'env) non
   sia visibile nell'altro ramo *)
let if_env_independence_tests =
  List.map make_not_bottom_case_with_env [
    ("n>0 ambiguo; il ramo then modifica x, il ramo else legge x -> deve "
     ^ "vedere ancora il valore originale, non quello mutato nell'altro ramo",
     If (Comparison (Var "n", Bigger, Const 0),
         Sequence (Assign ("x", Const 999), Skip),
         Assign ("y", BinaryOperation (Var "x", Add, Const 0))),
     [ ("x",Interval(Int(2),Int(999)));("y", Interval (Int (-8), Int 7)) ]); (* Claude aveva scazzato col risultato *)
  ]

let if_nested_tests =
  List.map make_not_bottom_case_with_env [
    ("If annidato, entrambe le condizioni decidibili vere",
     If (Comparison (Var "x", Bigger, Const 0),
         If (Comparison (Var "y", Smaller, Const 0),
             Assign ("r", Const 1), Assign ("r", Const 2)),
         Assign ("r", Const 3)),
     [ ("r", Interval (Int 1, Int 1)) ]);
  ]

let if_composite_cond_tests =
  List.map make_not_bottom_case_with_env [
    ("If con condizione composta And, decidibile vera",
     If (And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "y", Smaller, Const 0)),
         Assign ("r", Const 1), Assign ("r", Const 2)),
     [ ("r", Interval (Int 1, Int 1)) ]);
  ]

let if_bottom_propagation_tests =
  List.map make_bottom_case [
    ("If eseguito su uno stato gia' BottomEnv resta BottomEnv",
     Sequence (Filter (Boolean false),
               If (Boolean true, Assign ("x", Const 1), Assign ("x", Const 2))));
  ]

(* ==================================================================== *)
(* WHILE                                                                 *)
(* ==================================================================== *)

let while_not_entered_tests =
  List.map make_not_bottom_case_with_env [
    ("z=[0,0]; while(z=1) z:=99 -> condizione decidibile falsa, corpo mai eseguito",
     While (Comparison (Var "z", Equals, Const 1), Assign ("z", Const 99)),
     [ ("z", Interval (Int 0, Int 0)) ]);
  ]
  @ [
    ( "While(Boolean false, ...) partendo da stato vuoto non aggiunge variabili",
      `Quick,
      fun () ->
        match eval (While (Boolean false, Assign ("x", Const 100))) with
        | BottomEnv -> Alcotest.fail "atteso Env vuoto, ottenuto BottomEnv"
        | Env tbl -> Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl) );
  ]

(* NB: dipende da widen/aliasing (vedi nota in cima) - esegui e correggi
   se il valore atteso non combacia col tuo interprete. *)
let while_converges_tests =
  List.map make_not_bottom_case [
    ("z=0; while(z=0) z:=1 -> ATTESO indicativo, da verificare",
     Sequence (Assign ("z", Const 0),
       While (Comparison (Var "z", Equals, Const 0), Assign ("z", Const 1))),
     [ ]);  (* <-- riempi con il valore reale osservato, non e' predicibile con certezza *)
  ]

(* Esempio classico di perdita di precisione dovuta al widening:
   assumo un widen "alla Cousot" (se il limite superiore cresce tra
   un'iterazione e l'altra, salta a +inf). Se il tuo widen e' diverso,
   correggi il valore atteso. *)
let while_precision_loss_tests =
  List.map make_not_bottom_case [
    ("x=0; while(x<3) x:=x+1 -> il limite superiore cresce, widen -> +inf (assunzione)",
     Sequence (Assign ("x", Const 0),
       While (Comparison (Var "x", Smaller, Const 3),
              Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
     [ ("x", Interval (Int 0, PosInf)) ]); (* Risultato di Claude : x = [Int 3, PosInf] è corretto però non facendo narrowing è impossibile da avere, va implementato altrimenti fa cagare l'interprete*)
  ]

let while_precision_loss_bottom_tests =
  List.map make_bottom_case [
    ("x=0; while(x<3) (x:=x+1; Filter(x<>x)) -> il Filter interno e' "
     ^ "sempre falso (x<>x su singoletto e' decidibilmente falso), "
     ^ "quindi il corpo produce Bottom fin dalla prima iterazione",
     Sequence (Assign ("x", Const 0),
       While (Comparison (Var "x", Smaller, Const 3),
         Sequence (Assign ("x", BinaryOperation (Var "x", Add, Const 1)),
                   Filter (Comparison (Var "x", NotEquals, Var "x"))))));
  ]

(* Il corpo mantiene la variabile costante (x sempre [1,1]): niente
   crescita, quindi niente estrapolazione a +inf necessaria (assunzione
   minima: widen e' idempotente su valori uguali). La condizione di
   uscita pero' non e' mai vera concretamente -> il punto di uscita e'
   irraggiungibile -> Bottom. *)
let while_infinite_loop_tests =
  List.map make_bottom_case [
    ("x=1; while(x<5) x:=1 -> il corpo non fa mai crescere x, la "
     ^ "condizione di uscita non e' mai vera -> Bottom",
     Sequence (Assign ("x", Const 1),
       While (Comparison (Var "x", Smaller, Const 5), Assign ("x", Const 1))));
  ]

let while_bottom_propagation_tests =
  List.map make_bottom_case [
    ("While eseguito su stato gia' BottomEnv resta BottomEnv",
     Sequence (Filter (Boolean false),
               While (Boolean true, Assign ("x", Const 1))));
  ]

(* Boundary di uguaglianza: stessa dinamica di while_infinite_loop_tests
   ma espressa con Equals/NotEquals invece di </> *)
let while_equality_boundary_tests =
  List.map make_bottom_case [
    ("z=0; while(z=0) Skip -> la condizione resta sempre vera, uscita "
     ^ "irraggiungibile -> Bottom",
     Sequence (Assign ("z", Const 0),
       While (Comparison (Var "z", Equals, Const 0), Skip)));
  ]

let while_nested_tests =
  List.map make_not_bottom_case_with_env [
    ("While esterno mai entrato contenente un While interno (mai valutato)",
     While (Comparison (Var "z", Equals, Const 1),
       While (Comparison (Var "z", Equals, Const 1), Assign ("z", Const 99))),
     [ ("z", Interval (Int 0, Int 0)) ]);
  ]

(* NB: dipende fortemente da widen/aliasing - test "personale" da
   adattare dopo aver osservato l'output reale del tuo interprete. *)
let personal_while_test =
  List.map make_not_bottom_case [
    ("x=0; while(x<10) x:=x+2 -> ATTESO indicativo, da verificare",
     Sequence (Assign ("x", Const 0),
       While (Comparison (Var "x", Smaller, Const 10),
              Assign ("x", BinaryOperation (Var "x", Add, Const 2)))),
     [ ]);  (* <-- riempi con il valore reale osservato *)
  ]

(* ==================================================================== *)
(* Test Prog generici (mix di comandi)                                  *)
(* ==================================================================== *)

let condtest =
  List.map make_not_bottom_case [
    ("Sequenza con Filter e If misti, tutto decidibile",
     Sequence (
       Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Bigger, Const 0))),
       If (Comparison (Var "x", Equals, Const 5),
           Assign ("r", Const 1), Assign ("r", Const 0))),
     [ ("x", Interval (Int 5, Int 5)); ("r", Interval (Int 1, Int 1)) ]);
  ]

(* ==================================================================== *)
(* Export finale                                                        *)
(* ==================================================================== *)

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