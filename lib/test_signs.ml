module TestSigns = struct
  open Syntax
  open Abstract_domains.Signs
  open Interpeters

  (* Stato di test: associa nomi di variabili a valori nel dominio Signs *)
  let test_st : (string, t) Hashtbl.t = Hashtbl.create 10

  let init_state () =
    Hashtbl.clear test_st;
    Hashtbl.add test_st "x" Pos;        (* > 0 *)
    Hashtbl.add test_st "y" Neg;        (* < 0 *)
    Hashtbl.add test_st "z" Zero;       (* = 0 *)
    Hashtbl.add test_st "w" PosZero;    (* >= 0 *)
    Hashtbl.add test_st "k" NegZero;    (* <= 0 *)
    Hashtbl.add test_st "n" NonZero;    (* != 0 *)
    Hashtbl.add test_st "t" SignTop;    (* Top *)
    Hashtbl.add test_st "b" SignBottom (* Bottom *)

  let sign_to_string = function
    | SignTop    -> "Top"
    | Pos        -> "Pos"
    | Neg        -> "Neg"
    | Zero       -> "Zero"
    | SignBottom -> "Bottom"
    | PosZero    -> "PosZero"
    | NegZero    -> "NegZero"
    | NonZero    -> "NonZero"

  (* ------------------------------------------------------------------ *)
  (*  Lista di Test: (Descrizione, Espressione, Risultato Atteso)       *)
  (* ------------------------------------------------------------------ *)

  let sumtests = [
    ("Sum: Pos + Pos",             BinaryOperation (Var "x", Add, Var "x"), Pos);
    ("Sum: Pos + Neg",             BinaryOperation (Var "x", Add, Var "y"), SignTop);
    ("Sum: Neg + Neg",             BinaryOperation (Var "y", Add, Var "y"), Neg);
    ("Sum: Pos + Zero",            BinaryOperation (Var "x", Add, Var "z"), Pos);
    ("Sum: Zero + Zero",           BinaryOperation (Var "z", Add, Var "z"), Zero);
    ("Sum: PosZero + PosZero",     BinaryOperation (Var "w", Add, Var "w"), PosZero);
    ("Sum: NegZero + NegZero",     BinaryOperation (Var "k", Add, Var "k"), NegZero);
    ("Sum: PosZero + NegZero",     BinaryOperation (Var "w", Add, Var "k"), SignTop);
    ("Sum: PosZero + Pos",         BinaryOperation (Var "w", Add, Var "x"), Pos);
    ("Sum: PosZero + Neg",         BinaryOperation (Var "w", Add, Var "y"), SignTop);
    ("Sum: NegZero + Pos",         BinaryOperation (Var "k", Add, Var "x"), SignTop);
    ("Sum: NegZero + Neg",         BinaryOperation (Var "k", Add, Var "y"), Neg);
    ("Sum: NonZero + Pos",         BinaryOperation (Var "n", Add, Var "x"), SignTop);
    ("Sum: NonZero + Zero",        BinaryOperation (Var "n", Add, Var "z"), NonZero);
    ("Sum: NonZero + NonZero",     BinaryOperation (Var "n", Add, Var "n"), SignTop);
    ("Sum: Top + Pos",             BinaryOperation (Var "t", Add, Var "x"), SignTop);
    ("Sum: Bottom + Pos",          BinaryOperation (Var "b", Add, Var "x"), SignBottom);
    ("Sum: 10 + (-20)",            BinaryOperation (Const 10, Add, Const (-20)), SignTop);
  ]

  let subtests = [
    ("Sub: Pos - Neg",             BinaryOperation (Var "x", Sub, Var "y"), Pos);
    ("Sub: Pos - Pos (stessa var)",BinaryOperation (Var "x", Sub, Var "x"), SignTop);
    ("Sub: 10 - 20",               BinaryOperation (Const 10, Sub, Const 20), SignTop);
    ("Sub: PosZero - PosZero",     BinaryOperation (Var "w", Sub, Var "w"), SignTop);
    ("Sub: Zero - Neg",            BinaryOperation (Var "z", Sub, Var "y"), Pos);
  ]

  let multests = [
    ("Mul: Pos * Pos",             BinaryOperation (Var "x", Mul, Var "x"), Pos);
    ("Mul: Pos * Neg",             BinaryOperation (Var "x", Mul, Var "y"), Neg);
    ("Mul: Neg * Neg",             BinaryOperation (Var "y", Mul, Var "y"), Pos);
    ("Mul: Pos * Zero",            BinaryOperation (Var "x", Mul, Var "z"), Zero);
    ("Mul: PosZero * Neg",         BinaryOperation (Var "w", Mul, Var "y"), NegZero);
    ("Mul: NegZero * Pos",         BinaryOperation (Var "k", Mul, Var "x"), NegZero);
    ("Mul: NonZero * Zero",        BinaryOperation (Var "n", Mul, Var "z"), Zero);
    ("Mul: NonZero * NonZero",     BinaryOperation (Var "n", Mul, Var "n"), NonZero);
    ("Mul: Top * Zero",            BinaryOperation (Var "t", Mul, Var "z"), Zero);
    ("Mul: Bottom * Pos",          BinaryOperation (Var "b", Mul, Var "x"), SignBottom);
  ]

  let divtests = [
    ("Div: Pos / Pos",             BinaryOperation (Var "x", Div, Var "x"), PosZero);
    ("Div: Pos / Neg",             BinaryOperation (Var "x", Div, Var "y"), NegZero);
    ("Div: Neg / Neg",             BinaryOperation (Var "y", Div, Var "y"), PosZero);
    ("Div: Costante / Zero",       BinaryOperation (Const 10, Div, Var "z"), SignBottom);
    ("Div: Pos / PosZero (rischio 0)", BinaryOperation (Var "x", Div, Var "w"), SignTop);
    ("Div: Pos / NegZero (rischio 0)", BinaryOperation (Var "x", Div, Var "k"), SignTop);
    ("Div: Pos / NonZero",         BinaryOperation (Var "x", Div, Var "n"), SignTop);
    ("Div: Zero / Pos",            BinaryOperation (Var "z", Div, Var "x"), Zero);
    ("Div: Zero / Neg",            BinaryOperation (Var "z", Div, Var "y"), Zero);
    ("Div: Top / Pos",             BinaryOperation (Var "t", Div, Var "x"), SignTop);
    ("Div: PosZero / Neg",             BinaryOperation (Var "w", Div, Var "y"), NegZero);
    ("Div: NegZero / Pos",             BinaryOperation (Var "k", Div, Var "x"), NegZero);
  ]

  let negatetests = [
    ("Negate: Pos",                UnaryOperation (Negation, Var "x"), Neg);
    ("Negate: Neg",                UnaryOperation (Negation, Var "y"), Pos);
    ("Negate: Zero",               UnaryOperation (Negation, Var "z"), Zero);
    ("Negate: PosZero",            UnaryOperation (Negation, Var "w"), NegZero);
    ("Negate: NegZero",            UnaryOperation (Negation, Var "k"), PosZero);
    ("Negate: NonZero",            UnaryOperation (Negation, Var "n"), NonZero);
    ("Negate: Top",                UnaryOperation (Negation, Var "t"), SignTop);
    ("Negate: Bottom",             UnaryOperation (Negation, Var "b"), SignBottom);
    ("Doppia negazione: --Pos",    UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), Pos);
    ("Pos + (-Neg)",               BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), Pos);
  ]

  let randomtests = [
    ("Random(-1,10)",              Random (-1, 10), SignTop);
    ("Random(1,10)",               Random (1, 10), Pos);
    ("Random(-10,-1)",             Random (-10, -1), Neg);
    ("Random(0,10)",               Random (0, 10), PosZero);
    ("Random(-10,0)",              Random (-10, 0), NegZero);
    ("Random(0,0)",                Random (0, 0), Zero);
  ]

  (* ------------------------------------------------------------------ *)
  (*  Runner dei Test con output automatizzato                          *)
  (* ------------------------------------------------------------------ *)

  let run_group name tests =
    Printf.printf "===== TEST %s =====\n" name;
    List.iter
      (fun (test_name, expr, expected) ->
        try
          let result = SignInterp.eval expr test_st in
          let ok = result = expected in
          Printf.printf "[%s] %-35s -> %-10s (atteso %s)\n"
            (if ok then " OK " else "FAIL")
            test_name
            (sign_to_string result)
            (sign_to_string expected)
        with e ->
          Printf.printf "[EXC ] %-35s -> ECCEZIONE: %s\n"
            test_name
            (Printexc.to_string e))
      tests;
    print_newline ()

  let run_all_tests () =
    init_state ();
    Printf.printf "=========================================\n";
    Printf.printf "        TEST INTERPRETE SEGNI            \n";
    Printf.printf "=========================================\n\n";
    run_group "SOMMA" sumtests;
    run_group "SOTTRAZIONE" subtests;
    run_group "MOLTIPLICAZIONE" multests;
    run_group "DIVISIONE" divtests;
    run_group "NEGAZIONE" negatetests;
    run_group "RANDOM" randomtests
end