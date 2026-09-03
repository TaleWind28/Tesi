open Abstract_domains
open Interpeters
open Oracles
open Syntax

module Make_Sign_Tests (D : DOMAIN) (E : EXPECTED_VALUES with type t = D.t) = struct
  module Interp = AbsInterp (D)

  let sign_testable =
    Alcotest.testable
      (fun fmt s -> Format.fprintf fmt "%s" (D.to_string s))
      (fun a b -> a = b)

  (* Stato precompilato generico per il dominio D *)
  let make_test_state () =
    let st = Hashtbl.create 10 in
    let mappings = [
      ("x", D.abstract_range 1 10);        (* Positivo *)
      ("y", D.abstract_range (-10) (-1));  (* Negativo *)
      ("z", D.abstract_int 0);             (* Zero *)
      ("w", D.abstract_range 0 10);        (* PosZero *)
      ("k", D.abstract_range (-10) 0);     (* NegZero *)
      ("n", D.lub (D.abstract_range 1 10) (D.abstract_range (-10) (-1))); (* NonZero *)
      ("t", D.top);
      ("b", D.bottom)
    ] in
    List.iter (fun (k, v) -> Hashtbl.add st k v) mappings;
    st

  let make_case (desc, expr, expected) =
    ( desc,
      `Quick,
      fun () ->
        let res = Interp.eval_exp expr (Interp.Env (make_test_state ())) in
        Alcotest.(check sign_testable) desc expected res )

  let check_vars desc (final_env : Interp.state) expected_vars =
    match final_env with
    | Interp.BottomEnv ->
        Alcotest.fail (Printf.sprintf "%s: lo stato finale è BottomEnv" desc)
    | Interp.Env tbl ->
        List.iter
          (fun (var, expected) ->
            match Hashtbl.find_opt tbl var with
            | Some v -> Alcotest.(check sign_testable) (desc ^ " - " ^ var) expected v
            | None -> Alcotest.fail (Printf.sprintf "Variabile '%s' non trovata" var))
          expected_vars

  let make_prog_case (desc, prog, expected_vars) =
    (desc, `Quick, fun () -> check_vars desc (Interp.eval prog) expected_vars)

  let make_prog_case_with_env (desc, prog, expected_vars) =
    ( desc,
      `Quick,
      fun () ->
        let final_st = Interp.eval_cmd prog (Interp.Env (make_test_state ())) in
        check_vars desc final_st expected_vars )

  let expect_bottom ?(with_env = false) desc prog =
    ( desc,
      `Quick,
      fun () ->
        let res =
          if with_env then Interp.eval_cmd prog (Interp.Env (make_test_state ()))
          else Interp.eval prog
        in
        match res with
        | Interp.BottomEnv -> ()
        | Interp.Env _ -> Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto Env" desc) )

  let expect_bottom_with_env desc prog = expect_bottom ~with_env:true desc prog

  (* ------------------------------------------------------------ *)
  (* TEST ESPRESSIONI                                             *)
  (* ------------------------------------------------------------ *)

  let sumtests = List.map make_case [
    "Sum: Pos + Pos", BinaryOperation (Var "x", Add, Var "x"), E.sum_1;
    "Sum: Pos + Neg", BinaryOperation (Var "x", Add, Var "y"), E.sum_2;
    "Sum: Neg + Neg", BinaryOperation (Var "y", Add, Var "y"), E.sum_3;
    "Sum: Pos + Zero", BinaryOperation (Var "x", Add, Var "z"), E.sum_4;
    "Sum: Zero + Zero", BinaryOperation (Var "z", Add, Var "z"), E.sum_5;
    "Sum: PosZero + PosZero", BinaryOperation (Var "w", Add, Var "w"), E.sum_6;
    "Sum: NegZero + NegZero", BinaryOperation (Var "k", Add, Var "k"), E.sum_7;
    "Sum: PosZero + NegZero", BinaryOperation (Var "w", Add, Var "k"), E.sum_8;
    "Sum: PosZero + Pos", BinaryOperation (Var "w", Add, Var "x"), E.sum_9;
    "Sum: PosZero + Neg", BinaryOperation (Var "w", Add, Var "y"), E.sum_10;
    "Sum: NegZero + Pos", BinaryOperation (Var "k", Add, Var "x"), E.sum_11;
    "Sum: NegZero + Neg", BinaryOperation (Var "k", Add, Var "y"), E.sum_12;
    "Sum: NonZero + Pos", BinaryOperation (Var "n", Add, Var "x"), E.sum_13;
    "Sum: NonZero + Zero", BinaryOperation (Var "n", Add, Var "z"), E.sum_14;
    "Sum: NonZero + NonZero", BinaryOperation (Var "n", Add, Var "n"), E.sum_15;
    "Sum: Top + Pos", BinaryOperation (Var "t", Add, Var "x"), E.sum_16;
    "Sum: Bottom + Pos", BinaryOperation (Var "b", Add, Var "x"), E.sum_17;
    "Sum: 10 + (-20)", BinaryOperation (Const 10, Add, Const (-20)), E.sum_18;
  ]

  let subtests = List.map make_case [
    "Sub: Pos - Neg", BinaryOperation (Var "x", Sub, Var "y"), E.sub_1;
    "Sub: Pos - Pos (stessa var)", BinaryOperation (Var "x", Sub, Var "x"), E.sub_2;
    "Sub: 10 - 20", BinaryOperation (Const 10, Sub, Const 20), E.sub_3;
    "Sub: PosZero - PosZero", BinaryOperation (Var "w", Sub, Var "w"), E.sub_4;
    "Sub: Zero - Neg", BinaryOperation (Var "z", Sub, Var "y"), E.sub_5;
  ]

  let multests = List.map make_case [
    "Mul: Pos * Pos", BinaryOperation (Var "x", Mul, Var "x"), E.mul_1;
    "Mul: Pos * Neg", BinaryOperation (Var "x", Mul, Var "y"), E.mul_2;
    "Mul: Neg * Neg", BinaryOperation (Var "y", Mul, Var "y"), E.mul_3;
    "Mul: Pos * Zero", BinaryOperation (Var "x", Mul, Var "z"), E.mul_4;
    "Mul: PosZero * Neg", BinaryOperation (Var "w", Mul, Var "y"), E.mul_5;
    "Mul: NegZero * Pos", BinaryOperation (Var "k", Mul, Var "x"), E.mul_6;
    "Mul: NonZero * Zero", BinaryOperation (Var "n", Mul, Var "z"), E.mul_7;
    "Mul: NonZero * NonZero", BinaryOperation (Var "n", Mul, Var "n"), E.mul_8;
    "Mul: Top * Zero", BinaryOperation (Var "t", Mul, Var "z"), E.mul_9;
    "Mul: Bottom * Pos", BinaryOperation (Var "b", Mul, Var "x"), E.mul_10;
  ]

  let divtests = List.map make_case [
    "Div: Pos / Pos", BinaryOperation (Var "x", Div, Var "x"), E.div_1;
    "Div: Pos / Neg", BinaryOperation (Var "x", Div, Var "y"), E.div_2;
    "Div: Neg / Neg", BinaryOperation (Var "y", Div, Var "y"), E.div_3;
    "Div: Costante / Zero", BinaryOperation (Const 10, Div, Var "z"), E.div_4;
    "Div: Pos / PosZero", BinaryOperation (Var "x", Div, Var "w"), E.div_5;
    "Div: Pos / NegZero", BinaryOperation (Var "x", Div, Var "k"), E.div_6;
    "Div: Pos / NonZero", BinaryOperation (Var "x", Div, Var "n"), E.div_7;
    "Div: Zero / Pos", BinaryOperation (Var "z", Div, Var "x"), E.div_8;
    "Div: Zero / Neg", BinaryOperation (Var "z", Div, Var "y"), E.div_9;
    "Div: Top / Pos", BinaryOperation (Var "t", Div, Var "x"), E.div_10;
    "Div: PosZero / Neg", BinaryOperation (Var "w", Div, Var "y"), E.div_11;
    "Div: NegZero / Pos", BinaryOperation (Var "k", Div, Var "x"), E.div_12;
  ]

  let negatetests = List.map make_case [
    "Negate: Pos", UnaryOperation (Negation, Var "x"), E.neg_1;
    "Negate: Neg", UnaryOperation (Negation, Var "y"), E.neg_2;
    "Negate: Zero", UnaryOperation (Negation, Var "z"), E.neg_3;
    "Negate: PosZero", UnaryOperation (Negation, Var "w"), E.neg_4;
    "Negate: NegZero", UnaryOperation (Negation, Var "k"), E.neg_5;
    "Negate: NonZero", UnaryOperation (Negation, Var "n"), E.neg_6;
    "Negate: Top", UnaryOperation (Negation, Var "t"), E.neg_7;
    "Negate: Bottom", UnaryOperation (Negation, Var "b"), E.neg_8;
    "Doppia negazione: --Pos", UnaryOperation (Negation, UnaryOperation (Negation, Var "x")), E.neg_9;
    "Pos + (-Neg)", BinaryOperation (Var "x", Add, UnaryOperation (Negation, Var "y")), E.neg_10;
  ]

  let randomtests = List.map make_case [
    "Random(-1,10)", Random (-1, 10), E.rand_1;
    "Random(1,10)", Random (1, 10), E.rand_2;
    "Random(-10,-1)", Random (-10, -1), E.rand_3;
    "Random(0,10)", Random (0, 10), E.rand_4;
    "Random(-10,0)", Random (-10, 0), E.rand_5;
    "Random(0,0)", Random (0, 0), E.rand_6;
  ]

  (* ------------------------------------------------------------ *)
  (* TEST COMANDI                                                 *)
  (* ------------------------------------------------------------ *)

  let assigntests = List.map make_prog_case [
    "Assign semplice: x = 5", Assign ("x", Const 5), [ "x", E.assign_1 ];
    "Assign semplice: x = -5", Assign ("x", Const (-5)), [ "x", E.assign_2 ];
    "Assign semplice: x = 0", Assign ("x", Const 0), [ "x", E.assign_3 ];
    "Assign con variabile non definita: y = x", Assign ("y", Var "x"), [ "y", E.assign_4 ];
    "Assign con Random: x = Random(1,10)", Assign ("x", Random (1, 10)), [ "x", E.assign_5 ];
  ]

  let sequencetests = List.map make_prog_case [
    "Sequence: x=5; y=-3",
      Sequence (Assign ("x", Const 5), Assign ("y", Const (-3))),
      [ "x", E.sequence_1_1; "y", E.sequence_1_2 ];
  ]

  let skiptests = [
    ( "Skip da solo non modifica lo stato (stato vuoto)", `Quick,
      fun () ->
        match Interp.eval Skip with
        | Interp.Env tbl -> Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl)
        | Interp.BottomEnv -> Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );
    ( "Skip in mezzo a una sequenza non altera i valori", `Quick,
      fun () ->
        let prog = Sequence (Assign ("x", Const 42), Skip) in
        match Interp.eval_cmd prog (Interp.Env (make_test_state ())) with
        | Interp.Env tbl -> Alcotest.(check sign_testable) "x resta Pos" E.skip_1 (Hashtbl.find tbl "x")
        | Interp.BottomEnv -> Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );
  ]

  (* ------------------------------------------------------------ *)
  (* TEST CONTROLLO DI FLUSSO                                     *)
  (* ------------------------------------------------------------ *)

  let iftests = [
    make_prog_case_with_env
      ( "If Pos: if (x > 0) then y = 1 else y = -1",
        If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1), Assign ("y", Const (-1))),
        ["y", E.if_1] );
    make_prog_case_with_env
      ( "If certo vero (x>y)",
        If (Comparison (Var "x", Bigger, Var "y"), Assign ("k", Const 1), Assign ("k", Const 999)),
        [ "k", E.if_2 ] );
    make_prog_case_with_env
      ( "If certo falso (y>x)",
        If (Comparison (Var "y", Bigger, Var "x"), Assign ("k", Const (-999)), Assign ("k", Const 2)),
        [ "k", E.if_3 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Pos(5), else=Neg(-5)",
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 5), Assign ("k", Const (-5))),
        [ "k", E.if_4 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Pos(1), else=Zero(0)",
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 1), Assign ("k", Const 0)),
        [ "k", E.if_5 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Neg(-1), else=Zero(0)",
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const (-1)), Assign ("k", Const 0)),
        [ "k", E.if_6 ] );
    make_prog_case_with_env
      ( "Ambiguo: catch-all",
        If (Comparison (Var "w", Bigger, Var "x"),
            Assign ("k", Const 3),
            Assign ("k", BinaryOperation (Var "w", Mul, Var "y"))),
        [ "k", E.if_7 ] );
    make_prog_case_with_env
      ( "Ambiguo: rami convergenti (entrambi Pos)",
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 10), Assign ("k", Const 20)),
        [ "k", E.if_8 ] );
    make_prog_case_with_env
      ( "Ambiguo, var assegnata solo nel then",
        If (Comparison (Var "w", Bigger, Var "x"), Assign ("m", Const 7), Skip), [ "m", E.if_9 ]; );
    make_prog_case_with_env
      ( "Ambiguo, var assegnata solo nell'else",
        If (Comparison (Var "w", Bigger, Var "x"), Skip, Assign ("m", Const (-7))), [ "m", E.if_10 ]; );
    make_prog_case_with_env
      ( "If annidato",
        If (Comparison (Var "w", Bigger, Var "x"),
            If (Comparison (Var "w", Bigger, Var "x"), Assign ("k", Const 1), Assign ("k", Const (-1))),
            Assign ("k", Const 100)),
        [ "k", E.if_11 ] );
    make_prog_case_with_env
      ( "If con And",
        If (And (Comparison (Var "x", Bigger, Var "y"), Comparison (Var "w", Bigger, Var "x")),
            Assign ("k", Const 1), Assign ("k", Const (-1))),
        [ "k", E.if_12 ] );
    expect_bottom_with_env "Filter certo falso prima dell'If"
      (Sequence (Filter (Boolean false), If (Boolean true, Assign ("k", Const 1), Assign ("k", Const 2))))
  ]

  let whiletests = [
    make_prog_case
      ( "While mai eseguito",
        Sequence (Assign ("x", Const 5),
          While (
            Comparison (Var "x", Smaller, Const 0),
             Assign ("x", BinaryOperation (Var "x", Sub, Const 1))
            )),
        [ "x", E.while_1_1 ] );
    (* make_prog_case
      ( "While converge",
        Sequence (Assign ("x", Const 5), While (Comparison (Var "x", NotEquals, Const 0), Assign ("x", Const 0))),
        [ "x", E.while_2_1 ] ); *)
    make_prog_case
      ( "While perdita di precisione",
        Sequence (
          Assign ("x", Const (-5)),
          While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
        [ "x", E.while_3_1 ] );
    expect_bottom "Corpo = Skip (loop infinito)"
      (Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Bigger, Const 0), Skip)));
    expect_bottom "Filter(false) prima del while"
      (Sequence (Filter (Boolean false), While (Boolean true, Assign ("x", Const 1))));
    make_prog_case
      ( "Personal While Test",
        Sequence (
          Sequence (
            Assign ("x", Const 1),
            Sequence (Assign ("y", Const 2), Assign ("z", Random (-3, 5)))),
          While (
            Comparison (Var "x", Equals, Var "y"),
            If (Comparison (Var "y", Bigger, Var "z"),
                Assign ("x", BinaryOperation (Var "x", Add, Const (-2))),
                Assign ("y", BinaryOperation (Var "x", Add, Var "y"))))),
        [ "x", E.while_4_1; "y", E.while_4_2; "z", E.while_4_3 ] );
  ]

  let tests = [
    "Somma", sumtests;
    "Sottrazione", subtests;
    "Moltiplicazione", multests;
    "Divisione", divtests;
    "Negazione Unaria", negatetests;
    "Random", randomtests;
    "Assegnamenti", assigntests;
    "Sequenze", sequencetests;
    "Skip", skiptests;
    "Istruzioni Condizionali", iftests;
    "Cicli While", whiletests;
  ]
end

module TestSuite_Signs = Make_Sign_Tests (Abstract_domains.Signs) (Expected_Signs)
module TestSuite_SimpleSigns = Make_Sign_Tests (Abstract_domains.SimpleSigns) (Expected_SimpleSigns)
(* module TestSuite_ReducedSigns = Make_Sign_Tests (Abstract_domains.ReducedSigns) *)
module TestSuite_SimplifiedSigns = Make_Sign_Tests (Abstract_domains.SimplifiedSigns) (Expected_SimplifiedSigns)
(* module TestSuite_StrangeSigns = Make_Sign_Tests (Abstract_domains.StrangeSigns) *)
(* module TestSuite_Intervals = Make_Sign_Tests (Abstract_domains.Intervals) *)

(* 2. Esecuzione tramite Alcotest *)
(*let () =
   Alcotest.run "Suite di Test per Interprete Astratto" [
    "Domain: Signs", TestSuite_Signs.tests;
    "Domain: SimpleSigns", TestSuite_SimpleSigns.tests;
    "Domain: ReducedSigns", TestSuite_ReducedSigns.tests;
    "Domain: SimplifiedSigns", TestSuite_SimplifiedSigns.tests;
    "Domain: StrangeSigns", TestSuite_StrangeSigns.tests;
    "Domain: Intervals", TestSuite_Intervals.tests;
  ] *)
let () =
  Alcotest.run "Abstract Interpreter Tests" (
    List.map (fun (name, test_list) -> ("Signs: " ^ name, test_list)) TestSuite_Signs.tests
    @ List.map (fun (name, test_list) -> ("SimplifiedSigns: " ^ name, test_list)) TestSuite_SimplifiedSigns.tests
    @ List.map (fun (name,test_list) -> ("SimpleSigns: " ^ name, test_list)) TestSuite_SimpleSigns.tests
  )