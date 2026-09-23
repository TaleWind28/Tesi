open Abstract_domains
open Interpeters
open Oracles
open Syntax

module Make_Sign_Tests (D : NonRelationalDomain) (E : EXPECTED_VALUES with type t = D.t) = struct
  module Interp = NonRelationalAbsInterp (D)

  let sign_testable =
    Alcotest.testable
      (fun fmt s -> Format.fprintf fmt "%s" (D.to_string s))
      (fun a b -> a = b)

  (* Stato precompilato generico per il dominio D *)
  let make_test_state () =
    let st = Hashtbl.create 10 in
    Hashtbl.add st "x" (D.abstract_range 1 10);        (* Positivo *)
    Hashtbl.add st "y" (D.abstract_range (-10) (-1));  (* Negativo *)
    Hashtbl.add st "z" (D.abstract_int 0);             (* Zero *)
    Hashtbl.add st "w" (D.abstract_range 0 10);        (* PosZero *)
    Hashtbl.add st "k"(D.abstract_range (-10) 0);     (* NegZero *)
    Hashtbl.add st "n"(D.lub (D.abstract_range 1 10) (D.abstract_range (-10) (-1))); (* NonZero *)
    Hashtbl.add st "t"(D.top);
    Hashtbl.add st "b"(D.bottom);
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

  let check_filter_case ?(with_env = false) (desc, prog, outcome) =
    ( desc,
      `Quick,
      fun () ->
        let res =
          if with_env then Interp.eval_cmd prog (Interp.Env (make_test_state ()))
          else Interp.eval prog
        in
        match outcome with
        | ExpectBottom ->
            (match res with
             | Interp.BottomEnv -> ()
             | Interp.Env _ ->
                 Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto Env" desc))
        | ExpectEnv expected_vars ->
            (match res with
             | Interp.BottomEnv ->
                 Alcotest.fail (Printf.sprintf "%s: atteso Env, ottenuto BottomEnv" desc)
             | Interp.Env _ ->
                 check_vars desc res expected_vars) )

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
    make_prog_case
      ( "If Pos: if (x > 0) then y = 1 else y = -1",
        Sequence(Assign("x",Const(5)),If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const (1)), Assign ("y", Const (-1)))),
        ["y", E.if_1] );
    make_prog_case_with_env
      ( "If certo vero (x>y)",
        If (Comparison (Var "x", Bigger, Var "y"), Assign ("k", Const 1), Assign ("k", Const 999)),
        [ "k", E.if_2 ] );
    make_prog_case
      ( "If certo falso (y>x)",
        Sequence(
          Sequence(Assign("x",Const(5)),Assign("y",Const(-3))),
          If (
            Comparison (Var "y", Bigger, Var "x"), 
            Assign ("k", Const (-999)), 
            Assign ("k", Const 2))),
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
        Sequence (
          Assign ("x", Const 5),
          While (
            Comparison (Var "x", Smaller, Const 0),
            Assign ("x", BinaryOperation (Var "x", Sub, Const 1))
          )
        ),
        [ "x", E.while_1_1 ] );
    make_prog_case
      ( "While converge: x = 5; while x!=0 x = 0; -> risultato x = 0",
        Sequence (
          Assign ("x", Const 5),
          While (
            Comparison (Var "x", NotEquals, Const 0),
            Assign ("x", Const 0))
        ),
        [ "x", E.while_2_1 ] );
    make_prog_case
      ( "While perdita di precisione",
        Sequence (
          Assign ("x", Const (-5)),
          While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
        [ "x", E.while_3_1 ] );
      ( "Corpo = Skip (loop infinito)",
      `Quick,
      fun () ->
        let prog = Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Bigger, Const 0), Skip)) in
        match Interp.eval prog with
        | Interp.BottomEnv -> ()
        | Interp.Env tbl ->
            (* Per domini senza Pos stretto (come SimpleSigns), il ciclo esce soundly con x = 0 *)
            (match Hashtbl.find_opt tbl "x" with
             | Some v -> Alcotest.(check sign_testable) "x deve valere 0 (Zero o PosZero)" (E.while_3_2) v
             | None -> Alcotest.fail "Variabile 'x' non trovata nello stato finale") );
    (* expect_bottom "Corpo = Skip (loop infinito)"
      (Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Bigger, Const 0), Skip))); *)
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

  let contradiction_tests = [
    check_filter_case
      ("Filtro false booleano", Filter (Boolean false), E.filter_1);
    check_filter_case
      ("Filtro costante: 5 < 2", Filter (Comparison (Const 5, Smaller, Const 2)), E.filter_2);
    check_filter_case
      ("Filtro costante: 5 == 2", Filter (Comparison (Const 5, Equals, Const 2)), E.filter_3);
    check_filter_case
      ("Filtro costante contraddittorio: 5 <= -1", Filter (Comparison (Const 5, SmallerEquals, Const (-1))), E.filter_4);
    check_filter_case
      ("Filtro costante contraddittorio: -3 > 0", Filter (Comparison (Const (-3), Bigger, Const 0)), E.filter_5);
    check_filter_case
      ("Filtro costante contraddittorio: 0 != 0", Filter (Comparison (Const 0, NotEquals, Const 0)), E.filter_6);
    check_filter_case ~with_env:true
      ("Filtro x < 0 con x Pos", Filter (Comparison (Var "x", Smaller, Const 0)), E.filter_7);
    check_filter_case ~with_env:true
      ("Filtro y > 0 con y Neg", Filter (Comparison (Var "y", Bigger, Const 0)), E.filter_8);
    check_filter_case ~with_env:true
      ("Filtro z != 0 con z Zero", Filter (Comparison (Var "z", NotEquals, Const 0)), E.filter_9);
    check_filter_case ~with_env:true
      ("Filtro x < y con x Pos e y Neg", Filter (Comparison (Var "x", Smaller, Var "y")), E.filter_10);
    check_filter_case ~with_env:true
      ("Filtro y > x con x Pos e y Neg", Filter (Comparison (Var "y", Bigger, Var "x")), E.filter_11);
    check_filter_case ~with_env:true
      ("Filtro x == y con x Pos e y Neg", Filter (Comparison (Var "x", Equals, Var "y")), E.filter_12);
    check_filter_case ~with_env:true
      ("Filtro And (true, false)", Filter (And (Boolean true, Boolean false)), E.filter_13);
    check_filter_case ~with_env:true
      ("Filtro Not (x > y) con x Pos e y Neg", Filter (Not (Comparison (Var "x", Bigger, Var "y"))), E.filter_14);
    check_filter_case
      ("Filtro sequenza: x = 10; Filter(x <= 0)", Sequence (Assign ("x", Const 10), Filter (Comparison (Var "x", SmallerEquals, Const 0))), E.filter_15);
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
    "Filtri Contraddittori Base", contradiction_tests;
  ]
end

module TestSuite_ExtendedSigns = Make_Sign_Tests (Abstract_domains.ExtendedSigns) (Expected_ExtendedSigns)
module TestSuite_SimpleSigns = Make_Sign_Tests (Abstract_domains.SimpleSigns) (Expected_SimpleSigns)
module TestSuite_Signs = Make_Sign_Tests (Abstract_domains.Signs) (Expected_Signs)
module TestSuite_SimplifiedSigns = Make_Sign_Tests (Abstract_domains.SimplifiedSigns) (Expected_SimplifiedSigns)
module TestSuite_StrangeSigns = Make_Sign_Tests (Abstract_domains.StrangeSigns) (Expected_StrangeSigns)
module TestSuite_Intervals = Make_Sign_Tests (Abstract_domains.Intervals) (Expected_Intervals)

module Make_WeakRelational_Tests
    (D : Abstract_domains.WeakRelationalDomain)
    (Interp : sig val eval : cmd -> D.t end)
    (E : EXPECTED_ZONES with type value = D.value) = struct
  let val_testable =
    Alcotest.testable
      (fun fmt v -> Format.fprintf fmt "%s" (D.string_of_value v))
      (fun a b -> a = b)

  let check_vars desc final_env expected_vars =
    if D.is_bottom final_env then
      Alcotest.fail (Printf.sprintf "%s: lo stato finale è Bottom inaspettatamente" desc)
    else
      List.iter
        (fun (var, expected) ->
          let v = D.retrieve_variable var final_env in
          Alcotest.(check val_testable) (desc ^ " - " ^ var) expected v)
        expected_vars

  let make_prog_case (desc, prog, expected_vars) =
    (desc, `Quick, fun () -> check_vars desc (Interp.eval prog) expected_vars)

  let expect_bottom desc prog =
    ( desc,
      `Quick,
      fun () ->
        let res = Interp.eval prog in
        if not (D.is_bottom res) then
          Alcotest.fail (Printf.sprintf "%s: atteso Bottom, ottenuto stato valido" desc) )

  (* ------------------------------------------------------------ *)
  (* TEST ASSEGNAMENTI                                            *)
  (* ------------------------------------------------------------ *)
  let assigntests = List.map make_prog_case [
    "Assign costante positiva: x = 5", Assign ("x", Const 5), [ "x", E.assign_const_1 ];
    "Assign costante negativa: x = -5", Assign ("x", Const (-5)), [ "x", E.assign_const_2 ];
    "Assign zero: x = 0", Assign ("x", Const 0), [ "x", E.assign_const_3 ];
    "Assign Random positivo: x = Random(1,10)", Assign ("x", Random (1, 10)), [ "x", E.assign_rand_1 ];
    "Assign Random misto: x = Random(-5,5)", Assign ("x", Random (-5, 5)), [ "x", E.assign_rand_2 ];
    "Assign tra variabili: x = 5; y = x",
      Sequence (Assign ("x", Const 5), Assign ("y", Var "x")),
      [ "x", E.assign_var_1_x; "y", E.assign_var_1_y ];
    "Assign tra variabili con Random: x = Random(1,5); y = x",
      Sequence (Assign ("x", Random (1, 5)), Assign ("y", Var "x")),
      [ "x", E.assign_var_rand_x; "y", E.assign_var_rand_y ];
  ]

  (* ------------------------------------------------------------ *)
  (* TEST SHIFT E OFFSET                                          *)
  (* ------------------------------------------------------------ *)
  let shifttests = List.map make_prog_case [
    "Shift positivo: x = 5; x = x + 3",
      Sequence (Assign ("x", Const 5), Assign ("x", BinaryOperation (Var "x", Add, Const 3))),
      [ "x", E.shift_pos ];
    "Shift negativo: x = 5; x = x + (-10)",
      Sequence (Assign ("x", Const 5), Assign ("x", BinaryOperation (Var "x", Add, Const (-10)))),
      [ "x", E.shift_neg ];
    "Assign con offset tra due variabili: x = 5; y = x + 2",
      Sequence (Assign ("x", Const 5), Assign ("y", BinaryOperation (Var "x", Add, Const 2))),
      [ "x", E.assign_var_offset_x; "y", E.assign_var_offset_y ];
  ]

  let binoptests = List.map make_prog_case [
    "BinOp Add: x=3; y=4; z=x+y",
      Sequence (Assign ("x", Const 3), Sequence (Assign ("y", Const 4), Assign ("z", BinaryOperation (Var "x", Add, Var "y")))),
      [ "z", E.binop_add ];
    "BinOp Sub: x=10; y=2; z=x-y",
      Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 2), Assign ("z", BinaryOperation (Var "x", Sub, Var "y")))),
      [ "z", E.binop_sub ];
    "BinOp Mul: x=10; y=2; z=x*y",
      Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 2), Assign ("z", BinaryOperation (Var "x", Mul, Var "y")))),
      [ "z", E.binop_mul ];
    "BinOp Div: x=10; y=2; z=x/y",
      Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 2), Assign ("z", BinaryOperation (Var "x", Div, Var "y")))),
      [ "z", E.binop_div ];
    "UnOp Negation: x=7; y=-x",
      Sequence (Assign ("x", Const 7), Assign ("y", UnaryOperation (Negation, Var "x"))),
      [ "y", E.unop_neg ];
  ]

  (* ------------------------------------------------------------ *)
  (* TEST SEQUENZE E SKIP                                         *)
  (* ------------------------------------------------------------ *)
  let sequencetests = List.map make_prog_case [
    "Sequence: x = 1; y = 2",
      Sequence (Assign ("x", Const 1), Assign ("y", Const 2)),
      [ "x", E.seq_x; "y", E.seq_y ];
    "Skip in sequenza: x = 42; Skip",
      Sequence (Assign ("x", Const 42), Skip),
      [ "x", E.skip_val ];
  ]

  let skiptests = [
    ( "Skip da solo", `Quick, fun () ->
        let res = Interp.eval Skip in
        if D.is_bottom res then
          Alcotest.fail "Skip: stato inaspettatamente Bottom" );
  ]

  (* ------------------------------------------------------------ *)
  (* TEST FILTRI E RAFFINAMENTO                                   *)
  (* ------------------------------------------------------------ *)
  let filtertests = List.map make_prog_case [
    "Filter raffina limite superiore: x=Random(1,10); Filter(x <= 5)",
      Sequence (Assign ("x", Random (1, 10)), Filter (Comparison (Var "x", SmallerEquals, Const 5))),
      [ "x", E.filter_refine_ub ];
    "Filter raffina limite inferiore: x=Random(1,10); Filter(x >= 6)",
      Sequence (Assign ("x", Random (1, 10)), Filter (Comparison (Var "x", BiggerEquals, Const 6))),
      [ "x", E.filter_refine_lb ];
    "Filter raffina strettamente minore: x=Random(1,10); Filter(x < 5)",
      Sequence (Assign ("x", Random (1, 10)), Filter (Comparison (Var "x", Smaller, Const 5))),
      [ "x", E.filter_refine_lt ];
    "Filter raffina strettamente maggiore: x=Random(1,10); Filter(x > 5)",
      Sequence (Assign ("x", Random (1, 10)), Filter (Comparison (Var "x", Bigger, Const 5))),
      [ "x", E.filter_refine_gt ];
    "Filter tra variabili: x=5; y=3; Filter(x > y)",
      Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 3), Filter (Comparison (Var "x", Bigger, Var "y")))),
      [ "x", E.filter_rel_x; "y", E.filter_rel_y ];
    "Filter tra variabile ed espressione: x=4; y=-3; Filter(x > x+y)",
      Sequence (Assign ("x", Const 4), Sequence (Assign ("y", Const (-3)), Filter (Comparison (Var "x", Bigger, BinaryOperation (Var "x", Add, Var "y"))))),
      [ "x", E.filter_expr_x; "y", E.filter_expr_y ];
    "Filter uguaglianza: x=5; y=5; Filter(x == y)",
      Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (Var "x", Equals, Var "y")))),
      [ "x", E.filter_eq_x; "y", E.filter_eq_y ];
    "Filter booleano true: x=5; Filter(true)",
      Sequence (Assign ("x", Const 5), Filter (Boolean true)),
      [ "x", E.filter_true_x ];
    "Filter Not: x=5; Filter(not (x < 0))",
      Sequence (Assign ("x", Const 5), Filter (Not (Comparison (Var "x", Smaller, Const 0)))),
      [ "x", E.filter_not_x ];
    "Filter And: x=5; Filter(x > 0 and x < 10)",
      Sequence (Assign ("x", Const 5), Filter (And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "x", Smaller, Const 10)))),
      [ "x", E.filter_and_x ];
  ]

  (* ------------------------------------------------------------ *)
  (* ------------------------------------------------------------ *)
  (* TEST FILTRI CONTRADDITTORI (ATTESO BOTTOM)                   *)
  (* ------------------------------------------------------------ *)
  let bottomtests = [
    expect_bottom "Filtro impossibile su costante: x=5; Filter(x < 0)"
      (Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Smaller, Const 0))));
    expect_bottom "Filtro impossibile su costante: x=5; Filter(x > 10)"
      (Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Bigger, Const 10))));
    expect_bottom "Filtro uguaglianza incompatibile: x=5; Filter(x == 6)"
      (Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Equals, Const 6))));
    expect_bottom "Filtro tra variabili incompatibile: x=5; y=10; Filter(x > y)"
      (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 10), Filter (Comparison (Var "x", Bigger, Var "y")))));
    expect_bottom "Filtro booleano false: x=5; Filter(false)"
      (Sequence (Assign ("x", Const 5), Filter (Boolean false)));
    expect_bottom "Filtro congiunzione incompatibile: x=5; Filter(x > 10 and x < 2)"
      (Sequence (Assign ("x", Const 5), Filter (And (Comparison (Var "x", Bigger, Const 10), Comparison (Var "x", Smaller, Const 2)))));
  ]

  (* ------------------------------------------------------------ *)
  (* TEST ISTRUZIONI CONDIZIONALI (IF)                            *)
  (* ------------------------------------------------------------ *)
  let iftests = [
    make_prog_case
      ( "If certo vero: if (x > 0) y = 1 else y = -1",
        Sequence (Assign ("x", Const 5), If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1), Assign ("y", Const (-1)))),
        [ "x", E.if_true_x; "y", E.if_true_y ] );

    make_prog_case
      ( "If certo falso: if (x < 0) y = 1 else y = -1",
        Sequence (Assign ("x", Const 5), If (Comparison (Var "x", Smaller, Const 0), Assign ("y", Const 1), Assign ("y", Const (-1)))),
        [ "x", E.if_false_x; "y", E.if_false_y ] );

    make_prog_case
      ( "If ambiguo: x=Random(1,10); if (x <= 5) y = 1 else y = 2",
        Sequence (Assign ("x", Random (1, 10)), If (Comparison (Var "x", SmallerEquals, Const 5), Assign ("y", Const 1), Assign ("y", Const 2))),
        [ "y", E.if_ambig_y ] );

    make_prog_case
      ( "If ambiguo con range: x=Random(1,10); if (x <= 5) y = 10 else y = 20",
        Sequence (Assign ("x", Random (1, 10)), If (Comparison (Var "x", SmallerEquals, Const 5), Assign ("y", Const 10), Assign ("y", Const 20))),
        [ "y", E.if_ambig_range_y ] );

    make_prog_case
      ( "If con raffinamento variabile: x=Random(1,10); if (x <= 5) x = x+10 else x = x-5",
        Sequence (
          Assign ("x", Random (1, 10)),
          If (
            Comparison (Var "x", SmallerEquals, Const 5),
            Assign ("x", BinaryOperation (Var "x", Add, Const 10)),
            Assign ("x", BinaryOperation (Var "x", Add, Const (-5))))),
        [ "x", E.if_refine_x ] );

    make_prog_case
      ( "If relazionale certo: x=10; y=20; if (x < y) z = 1 else z = 2",
        Sequence (
          Assign ("x", Const 10),
          Sequence (
            Assign ("y", Const 20),
            If (Comparison (Var "x", Smaller, Var "y"), Assign ("z", Const 1), Assign ("z", Const 2)))),
        [ "z", E.if_rel_true_z ] );

    make_prog_case
      ( "If relazionale ambiguo: x=Random(1,10); y=Random(1,10); if (x < y) k = 1 else k = 2",
        Sequence (
          Assign ("x", Random (1, 10)),
          Sequence (
            Assign ("y", Random (1, 10)),
            If (Comparison (Var "x", Smaller, Var "y"), Assign ("k", Const 1), Assign ("k", Const 2)))),
        [ "k", E.if_rel_ambig_k ] );

    make_prog_case
      ( "If annidato: x=5; if (x > 0) then (if (x < 10) y = 1 else y = 2) else y = 3",
        Sequence (
          Assign ("x", Const 5),
          If (
            Comparison (Var "x", Bigger, Const 0),
            If (Comparison (Var "x", Smaller, Const 10), Assign ("y", Const 1), Assign ("y", Const 2)),
            Assign ("y", Const 3))),
        [ "y", E.if_nested_y ] );

    make_prog_case
      ( "If con assegnamento parziale: y=0; x=Random(1,5); if (x > 3) y = 7 else Skip",
        Sequence (
          Assign ("y", Const 0),
          Sequence (
            Assign ("x", Random (1, 5)),
            If (Comparison (Var "x", Bigger, Const 3), Assign ("y", Const 7), Skip))),
        [ "y", E.if_partial_assign_y ] );

    expect_bottom "If con filtro falso prima dell'If"
      (Sequence (Assign ("x", Const 5), Sequence (Filter (Boolean false), If (Boolean true, Assign ("y", Const 1), Assign ("y", Const 2)))));
  ]

  (* ------------------------------------------------------------ *)
  (* TEST CICLI WHILE                                             *)
  (* ------------------------------------------------------------ *)
  let whiletests = [
    make_prog_case
      ( "While mai eseguito: x = 5; while (x < 0) x = x + 1",
        Sequence (Assign ("x", Const 5), While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
        [ "x", E.while_not_executed_x ] );

    make_prog_case
      ( "While incremento da negativo a zero: x = -5; while (x < 0) x = x + 1",
        Sequence (Assign ("x", Const (-5)), While (Comparison (Var "x", Smaller, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
        [ "x", E.while_inc_from_neg_x ] );

    make_prog_case
      ( "While incremento da zero a dieci: x = 0; while (x < 10) x = x + 1",
        Sequence (Assign ("x", Const 0), While (Comparison (Var "x", Smaller, Const 10), Assign ("x", BinaryOperation (Var "x", Add, Const 1)))),
        [ "x", E.while_inc_from_zero_x ] );

    make_prog_case
      ( "While decremento a zero: x = 10; while (x > 0) x = x - 1",
        Sequence (Assign ("x", Const 10), While (Comparison (Var "x", Bigger, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const (-1))))),
        [ "x", E.while_dec_to_zero_x ] );

    make_prog_case
      ( "While step 2: x = 0; while (x < 10) x = x + 2",
        Sequence (Assign ("x", Const 0), While (Comparison (Var "x", Smaller, Const 10), Assign ("x", BinaryOperation (Var "x", Add, Const 2)))),
        [ "x", E.while_step_two_x ] );

    make_prog_case
      ( "While relazionale tra due variabili: x = 0; y = 10; while (x < y) x = x + 1",
        Sequence (
          Assign ("x", Const 0),
          Sequence (
            Assign ("y", Const 10),
            While (Comparison (Var "x", Smaller, Var "y"), Assign ("x", BinaryOperation (Var "x", Add, Const 1))))),
        [ "x", E.while_relational_x; "y", E.while_relational_y ] );

    make_prog_case
      ( "While con invariante preservata: x = 0; y = 42; while (x < 5) x = x + 1",
        Sequence (
          Assign ("x", Const 0),
          Sequence (
            Assign ("y", Const 42),
            While (Comparison (Var "x", Smaller, Const 5), Assign ("x", BinaryOperation (Var "x", Add, Const 1))))),
        [ "x", E.while_invariant_x; "y", E.while_invariant_y ] );

    make_prog_case
      ( "While convergenza rapida: x = 5; while (x != 0) x = 0",
        Sequence (Assign ("x", Const 5), While (Comparison (Var "x", NotEquals, Const 0), Assign ("x", Const 0))),
        [ "x", E.while_fast_converge_x ] );

    expect_bottom "While con filtro falso prima del ciclo"
      (Sequence (Assign ("x", Const 5), Sequence (Filter (Boolean false), While (Comparison (Var "x", Smaller, Const 10), Assign ("x", Const 1)))));
  ]

  let relational_inconsistency_tests = [
    expect_bottom "Transitivita negativa a 3 nodi: x-y<=2; y-z<=3; Filter(x-z >= 10)"
      (Sequence (
        Assign ("x", Random (0, 100)),
        Sequence (
          Assign ("y", Random (0, 100)),
          Sequence (
            Assign ("z", Random (0, 100)),
            Sequence (
              Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), SmallerEquals, Const 2)),
              Sequence (
                Filter (Comparison (BinaryOperation (Var "y", Sub, Var "z"), SmallerEquals, Const 3)),
                Filter (Comparison (BinaryOperation (Var "x", Sub, Var "z"), BiggerEquals, Const 10))
              )
            )
          )
        )
      ));
    expect_bottom "Transitivita a 4 nodi: x-y<=1; y-z<=1; z-w<=1; Filter(x-w >= 5)"
      (Sequence (
        Assign ("x", Random (0, 50)),
        Sequence (
          Assign ("y", Random (0, 50)),
          Sequence (
            Assign ("z", Random (0, 50)),
            Sequence (
              Assign ("w", Random (0, 50)),
              Sequence (
                Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), SmallerEquals, Const 1)),
                Sequence (
                  Filter (Comparison (BinaryOperation (Var "y", Sub, Var "z"), SmallerEquals, Const 1)),
                  Sequence (
                    Filter (Comparison (BinaryOperation (Var "z", Sub, Var "w"), SmallerEquals, Const 1)),
                    Filter (Comparison (BinaryOperation (Var "x", Sub, Var "w"), BiggerEquals, Const 5))
                  )
                )
              )
            )
          )
        )
      ));
    expect_bottom "Auto-contraddizione: Filter(x < x)"
      (Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", Smaller, Var "x"))));
    expect_bottom "Auto-contraddizione NotEquals: Filter(x != x)"
      (Sequence (Assign ("x", Const 5), Filter (Comparison (Var "x", NotEquals, Var "x"))));
    expect_bottom "Contraddizione diretta tra costanti: x=5; y=10; Filter(x >= y)"
      (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 10), Filter (Comparison (Var "x", BiggerEquals, Var "y")))));
    expect_bottom "Differenza incompatibile con costanti: x=10; y=2; Filter(x-y < 5)"
      (Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 2), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), Smaller, Const 5)))));
    expect_bottom "Random range superiore violato: x in [1,5]; Filter(x > 10)"
      (Sequence (Assign ("x", Random (1, 5)), Filter (Comparison (Var "x", Bigger, Const 10))));
    expect_bottom "Random range inferiore violato: x in [1,5]; Filter(x < 0)"
      (Sequence (Assign ("x", Random (1, 5)), Filter (Comparison (Var "x", Smaller, Const 0))));
    expect_bottom "Random intervalli disgiunti: x in [1,5]; y in [10,20]; Filter(x >= y)"
      (Sequence (Assign ("x", Random (1, 5)), Sequence (Assign ("y", Random (10, 20)), Filter (Comparison (Var "x", BiggerEquals, Var "y")))));
    expect_bottom "Random intervalli disgiunti uguaglianza: x in [1,5]; y in [10,20]; Filter(x == y)"
      (Sequence (Assign ("x", Random (1, 5)), Sequence (Assign ("y", Random (10, 20)), Filter (Comparison (Var "x", Equals, Var "y")))));
    expect_bottom "Random differenza incompatibile: x in [0,5]; y in [20,30]; Filter(x-y > 0)"
      (Sequence (Assign ("x", Random (0, 5)), Sequence (Assign ("y", Random (20, 30)), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), Bigger, Const 0)))));
    expect_bottom "Random differenza incompatibile inversa: x in [20,30]; y in [0,5]; Filter(x-y < 0)"
      (Sequence (Assign ("x", Random (20, 30)), Sequence (Assign ("y", Random (0, 5)), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), Smaller, Const 0)))));
    expect_bottom "Shift var e contraddizione: x=5; y=x+3; Filter(y <= x)"
      (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", BinaryOperation (Var "x", Add, Const 3)), Filter (Comparison (Var "y", SmallerEquals, Var "x")))));
    expect_bottom "Shift var e contraddizione inversa: x=5; y=x-4; Filter(x <= y)"
      (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", BinaryOperation (Var "x", Sub, Const 4)), Filter (Comparison (Var "x", SmallerEquals, Var "y")))));
    expect_bottom "Shift e bound incompatibile: x=10; y=x+2; Filter(y < 12)"
      (Sequence (Assign ("x", Const 10), Sequence (Assign ("y", BinaryOperation (Var "x", Add, Const 2)), Filter (Comparison (Var "y", Smaller, Const 12)))));
    expect_bottom "Shift e bound incompatibile negativo: x=10; y=x-2; Filter(y > 8)"
      (Sequence (Assign ("x", Const 10), Sequence (Assign ("y", BinaryOperation (Var "x", Sub, Const 2)), Filter (Comparison (Var "y", Bigger, Const 8)))));
    expect_bottom "Doppio filtro incompatibile: x in [1,10]; Filter(x < 3); Filter(x > 7)"
      (Sequence (Assign ("x", Random (1, 10)), Sequence (Filter (Comparison (Var "x", Smaller, Const 3)), Filter (Comparison (Var "x", Bigger, Const 7)))));
    expect_bottom "Ciclo chiuso di differenze impossibile: x-y<=0, y-x<=-1"
      (Sequence (
        Assign ("x", Random (0, 10)),
        Sequence (
          Assign ("y", Random (0, 10)),
          Sequence (
            Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), SmallerEquals, Const 0)),
            Filter (Comparison (BinaryOperation (Var "y", Sub, Var "x"), SmallerEquals, Const (-1)))
          )
        )
      ));
    expect_bottom "If con entrambi i rami contraddittori"
      (Sequence (
        Assign ("x", Random (1, 5)),
        If (Comparison (Var "x", Bigger, Const 10), Assign ("y", Const 1), Filter (Boolean false))
      ));
    expect_bottom "While con condizione impossibile e stato iniziale vuoto"
      (Sequence (
        Assign ("x", Const 5),
        Sequence (
          Filter (Comparison (Var "x", Equals, Const 0)),
          While (Comparison (Var "x", Smaller, Const 10), Assign ("x", Const 1))
        )
      ));
  ]

  let tests = [
    "Assegnamenti", assigntests;
    "Shift e Offset", shifttests;
    "Operazioni Aritmetiche", binoptests;
    "Sequenze", sequencetests;
    "Skip", skiptests;
    "Filtri", filtertests;
    "Filtri Contraddittori (Bottom)", bottomtests;
    "Inconsistenze Relazionali Aggiuntive", relational_inconsistency_tests;
    "Istruzioni Condizionali", iftests;
    "Cicli While", whiletests;
  ]
end

module TestSuite_Zones = Make_WeakRelational_Tests (Abstract_domains.Zones) (ZoneInterp) (Expected_Zones)
module TestSuite_Octagons = Make_WeakRelational_Tests (Abstract_domains.Octagons) (OctagonInterp) (Expected_Octagons)

module OctagonSpecificTests = struct
  open Abstract_domains.Octagons
  let oct_val_testable =
    Alcotest.testable
      (fun fmt v -> Format.fprintf fmt "%s" (string_of_value v))
      (fun a b -> a = b)

  let check_vars desc final_env expected_vars =
    if is_bottom final_env then
      Alcotest.fail (Printf.sprintf "%s: lo stato finale è Bottom inaspettatamente" desc)
    else
      List.iter
        (fun (var, expected) ->
          let v = retrieve_variable var final_env in
          Alcotest.(check oct_val_testable) (desc ^ " - " ^ var) expected v)
        expected_vars

  let make_case (desc, prog, expected_vars) =
    (desc, `Quick, fun () -> check_vars desc (OctagonInterp.eval prog) expected_vars)

  let expect_bottom desc prog =
    ( desc, `Quick, fun () ->
        let res = OctagonInterp.eval prog in
        if not (is_bottom res) then
          Alcotest.fail (Printf.sprintf "%s: atteso Bottom, ottenuto stato valido" desc) )

  let tests = [
    "Ottagoni: Funzionalità Specifiche", [
      make_case (
        "Assegnamento variabile negativa esatto: x=5; y=-x",
        Sequence (Assign ("x", Const 5), Assign ("y", UnaryOperation (Negation, Var "x"))),
        [ "x", abstract_int 5; "y", abstract_int (-5) ]
      );
      make_case (
        "Assegnamento affine variabile negativa: x=5; y=-x+2",
        Sequence (Assign ("x", Const 5), Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 2))),
        [ "x", abstract_int 5; "y", abstract_int (-3) ]
      );
      make_case (
        "Filtro somma concorde positiva: x=Random(1,10); y=Random(1,10); Filter(x+y <= 12)",
        Sequence (
          Assign ("x", Random (1, 10)),
          Sequence (
            Assign ("y", Random (1, 10)),
            Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 12))
          )
        ),
        [ "x", abstract_range 1 10; "y", abstract_range 1 10 ]
      );
      expect_bottom
        "Filtro somma concorde contraddittorio: x=10; y=10; Filter(x+y <= 15)"
        (Sequence (
          Assign ("x", Const 10),
          Sequence (
            Assign ("y", Const 10),
            Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 15))
          )
        ));
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-10; y=-10; Filter(-x-y <= 15)"
        (Sequence (
          Assign ("x", Const (-10)),
          Sequence (
            Assign ("y", Const (-10)),
            Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const 15))
          )
        ));

      (* --- 35 TEST AGGIUNTIVI SPECIFICI SUGLI OTTAGONI --- *)
      make_case (
        "Assegnamento negativo con zero: x=0; y=-x",
        Sequence (Assign ("x", Const 0), Assign ("y", UnaryOperation (Negation, Var "x"))),
        [ "x", abstract_int 0; "y", abstract_int 0 ]
      );
      make_case (
        "Assegnamento negativo da valore negativo: x=-7; y=-x",
        Sequence (Assign ("x", Const (-7)), Assign ("y", UnaryOperation (Negation, Var "x"))),
        [ "x", abstract_int (-7); "y", abstract_int 7 ]
      );
      make_case (
        "Assegnamento affine negativo sottrazione: x=4; y=-x-2",
        Sequence (Assign ("x", Const 4), Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Const 2))),
        [ "x", abstract_int 4; "y", abstract_int (-6) ]
      );
      make_case (
        "Assegnamento affine negativo su negativo: x=-5; y=-x+10",
        Sequence (Assign ("x", Const (-5)), Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10))),
        [ "x", abstract_int (-5); "y", abstract_int 15 ]
      );
      make_case (
        "Assegnamento affine negativo con offset zero: x=8; y=-x+0",
        Sequence (Assign ("x", Const 8), Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 0))),
        [ "x", abstract_int 8; "y", abstract_int (-8) ]
      );
      make_case (
        "Doppia negazione di variabile: x=6; y=-x; z=-y",
        Sequence (Assign ("x", Const 6), Sequence (Assign ("y", UnaryOperation (Negation, Var "x")), Assign ("z", UnaryOperation (Negation, Var "y")))),
        [ "x", abstract_int 6; "y", abstract_int (-6); "z", abstract_int 6 ]
      );
      make_case (
        "Catena di negazioni: x=-3; y=-x; z=-y; w=-z",
        Sequence (Assign ("x", Const (-3)), Sequence (Assign ("y", UnaryOperation (Negation, Var "x")), Sequence (Assign ("z", UnaryOperation (Negation, Var "y")), Assign ("w", UnaryOperation (Negation, Var "z"))))),
        [ "x", abstract_int (-3); "y", abstract_int 3; "z", abstract_int (-3); "w", abstract_int 3 ]
      );
      make_case (
        "Somma di opposti: x=5; y=-x; s=x+y",
        Sequence (Assign ("x", Const 5), Sequence (Assign ("y", UnaryOperation (Negation, Var "x")), Assign ("s", BinaryOperation (Var "x", Add, Var "y")))),
        [ "x", abstract_int 5; "y", abstract_int (-5); "s", abstract_int 0 ]
      );
      make_case (
        "Filtro somma concorde upper bound: x=4; y=6; Filter(x+y <= 10)",
        Sequence (Assign ("x", Const 4), Sequence (Assign ("y", Const 6), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 10)))),
        [ "x", abstract_int 4; "y", abstract_int 6 ]
      );
      make_case (
        "Filtro somma concorde lower bound: x=4; y=6; Filter(x+y >= 10)",
        Sequence (Assign ("x", Const 4), Sequence (Assign ("y", Const 6), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), BiggerEquals, Const 10)))),
        [ "x", abstract_int 4; "y", abstract_int 6 ]
      );
      make_case (
        "Filtro somma concorde uguaglianza: x=3; y=7; Filter(x+y == 10)",
        Sequence (Assign ("x", Const 3), Sequence (Assign ("y", Const 7), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), Equals, Const 10)))),
        [ "x", abstract_int 3; "y", abstract_int 7 ]
      );
      make_case (
        "Filtro -x-y <= -10 valido: x=5; y=5; Filter(-x-y <= -10)",
        Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const (-10))))),
        [ "x", abstract_int 5; "y", abstract_int 5 ]
      );
      make_case (
        "Filtro -x+y <= 6 valido: x=2; y=8; Filter(-x+y <= 6)",
        Sequence (Assign ("x", Const 2), Sequence (Assign ("y", Const 8), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Var "y"), SmallerEquals, Const 6)))),
        [ "x", abstract_int 2; "y", abstract_int 8 ]
      );
      make_case (
        "Filtro x-y <= 10 valido: x=8; y=2; Filter(x-y <= 10)",
        Sequence (Assign ("x", Const 8), Sequence (Assign ("y", Const 2), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), SmallerEquals, Const 10)))),
        [ "x", abstract_int 8; "y", abstract_int 2 ]
      );
      make_case (
        "Filtro affine positivo: x=3; y=x+5; Filter(x-y <= -5)",
        Sequence (Assign ("x", Const 3), Sequence (Assign ("y", BinaryOperation (Var "x", Add, Const 5)), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), SmallerEquals, Const (-5))))),
        [ "x", abstract_int 3; "y", abstract_int 8 ]
      );
      make_case (
        "If con filtro somma vero: x=3; y=4; if (x+y <= 10) z=1 else z=2",
        Sequence (Assign ("x", Const 3), Sequence (Assign ("y", Const 4), If (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 10), Assign ("z", Const 1), Assign ("z", Const 2)))),
        [ "x", abstract_int 3; "y", abstract_int 4; "z", abstract_int 1 ]
      );
      make_case (
        "If con filtro somma falso: x=3; y=4; if (x+y <= 5) z=1 else z=2",
        Sequence (Assign ("x", Const 3), Sequence (Assign ("y", Const 4), If (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 5), Assign ("z", Const 1), Assign ("z", Const 2)))),
        [ "x", abstract_int 3; "y", abstract_int 4; "z", abstract_int 2 ]
      );
      make_case (
        "If con filtro somma negativa vero: x=-3; y=-4; if (-x-y <= 10) z=1 else z=2",
        Sequence (Assign ("x", Const (-3)), Sequence (Assign ("y", Const (-4)), If (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const 10), Assign ("z", Const 1), Assign ("z", Const 2)))),
        [ "x", abstract_int (-3); "y", abstract_int (-4); "z", abstract_int 1 ]
      );
      make_case (
        "If con filtro somma negativa falso: x=-3; y=-4; if (-x-y <= 5) z=1 else z=2",
        Sequence (Assign ("x", Const (-3)), Sequence (Assign ("y", Const (-4)), If (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const 5), Assign ("z", Const 1), Assign ("z", Const 2)))),
        [ "x", abstract_int (-3); "y", abstract_int (-4); "z", abstract_int 2 ]
      );
      make_case (
        "While mai eseguito su somma: x=10; y=10; while (x+y < 15) x=0",
        Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 10), While (Comparison (BinaryOperation (Var "x", Add, Var "y"), Smaller, Const 15), Assign ("x", Const 0)))),
        [ "x", abstract_int 10; "y", abstract_int 10 ]
      );
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y <= 9)"
        (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 9)))));
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y < 10)"
        (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), Smaller, Const 10)))));
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y > 10)"
        (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), Bigger, Const 10)))));
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y >= 11)"
        (Sequence (Assign ("x", Const 5), Sequence (Assign ("y", Const 5), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), BiggerEquals, Const 11)))));
      expect_bottom
        "Filtro somma negativa contraddittorio: x=2; y=3; Filter(-x-y >= 0)"
        (Sequence (Assign ("x", Const 2), Sequence (Assign ("y", Const 3), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), BiggerEquals, Const 0)))));
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-5; y=-5; Filter(-x-y <= 9)"
        (Sequence (Assign ("x", Const (-5)), Sequence (Assign ("y", Const (-5)), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const 9)))));
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-5; y=-5; Filter(-x-y < 10)"
        (Sequence (Assign ("x", Const (-5)), Sequence (Assign ("y", Const (-5)), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), Smaller, Const 10)))));
      expect_bottom
        "Filtro -x+y contraddittorio: x=10; y=2; Filter(-x+y >= 0)"
        (Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 2), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Var "y"), BiggerEquals, Const 0)))));
      expect_bottom
        "Filtro x-y contraddittorio: x=2; y=10; Filter(x-y >= 0)"
        (Sequence (Assign ("x", Const 2), Sequence (Assign ("y", Const 10), Filter (Comparison (BinaryOperation (Var "x", Sub, Var "y"), BiggerEquals, Const 0)))));
      expect_bottom
        "Filtro Random somma contraddittoria: x in [5,10]; y in [5,10]; Filter(x+y <= 8)"
        (Sequence (Assign ("x", Random (5, 10)), Sequence (Assign ("y", Random (5, 10)), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 8)))));
      expect_bottom
        "Filtro Random somma contraddittoria: x in [1,2]; y in [1,2]; Filter(x+y >= 10)"
        (Sequence (Assign ("x", Random (1, 2)), Sequence (Assign ("y", Random (1, 2)), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), BiggerEquals, Const 10)))));
      expect_bottom
        "Filtro Random somma negativa contraddittoria: x in [5,10]; y in [5,10]; Filter(-x-y >= 0)"
        (Sequence (Assign ("x", Random (5, 10)), Sequence (Assign ("y", Random (5, 10)), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), BiggerEquals, Const 0)))));
      expect_bottom
        "Filtro Random -x-y contraddittoria: x in [-2, -1]; y in [-2, -1]; Filter(-x-y <= 1)"
        (Sequence (Assign ("x", Random (-2, -1)), Sequence (Assign ("y", Random (-2, -1)), Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "x"), Sub, Var "y"), SmallerEquals, Const 1)))));
      expect_bottom
        "While con filtro somma contraddittorio all'ingresso"
        (Sequence (Assign ("x", Const 10), Sequence (Assign ("y", Const 10), Sequence (Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), Smaller, Const 0)), While (Boolean true, Skip)))));
      expect_bottom
        "Filtro somma concorde uguaglianza contraddittoria: x=3; y=4; Filter(x+y == 10)"
        (Sequence (Assign ("x", Const 3), Sequence (Assign ("y", Const 4), Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), Equals, Const 10)))));
    ]
  ]
end

module AdvancedInterpreterTests = struct
  open Syntax

  let expect_bottom is_bottom eval desc prog =
    ( desc,
      `Quick,
      fun () ->
        if not (is_bottom (eval prog)) then
          Alcotest.fail (Printf.sprintf "%s: atteso Bottom, ottenuto stato valido" desc) )

  (* 1. TEST RELAZIONALI CONDIVISI (Zone e Ottagoni) *)
  let make_shared_relational_tests is_bottom eval = [
    expect_bottom is_bottom eval
      "Lockstep While: x=0; y=0; while(x<10) {x++; y++}; Filter(x != y)"
      (Sequence (
        Assign ("x", Const 0),
        Sequence (
          Assign ("y", Const 0),
          Sequence (
            While (Comparison (Var "x", Smaller, Const 10),
              Sequence (
                Assign ("x", BinaryOperation (Var "x", Add, Const 1)),
                Assign ("y", BinaryOperation (Var "y", Add, Const 1))
              )
            ),
            Filter (Comparison (Var "x", NotEquals, Var "y"))
          )
        )
      ));

    expect_bottom is_bottom eval
      "Swap di variabili: x=10; y=20; t=x; x=y; y=t; Filter(x <= y)"
      (Sequence (
        Assign ("x", Const 10),
        Sequence (
          Assign ("y", Const 20),
          Sequence (
            Assign ("t", Var "x"),
            Sequence (
              Assign ("x", Var "y"),
              Sequence (
                Assign ("y", Var "t"),
                Filter (Comparison (Var "x", SmallerEquals, Var "y"))
              )
            )
          )
        )
      ));

    expect_bottom is_bottom eval
      "Transitivita a 5 nodi: a-b<=-2; b-c<=-3; c-d<=-1; d-e<=-2; Filter(a-e >= -5)"
      (Sequence (
        Assign ("a", Random (0, 100)),
        Sequence (
          Assign ("b", Random (0, 100)),
          Sequence (
            Assign ("c", Random (0, 100)),
            Sequence (
              Assign ("d", Random (0, 100)),
              Sequence (
                Assign ("e", Random (0, 100)),
                Sequence (
                  Filter (Comparison (BinaryOperation (Var "a", Sub, Var "b"), SmallerEquals, Const (-2))),
                  Sequence (
                    Filter (Comparison (BinaryOperation (Var "b", Sub, Var "c"), SmallerEquals, Const (-3))),
                    Sequence (
                      Filter (Comparison (BinaryOperation (Var "c", Sub, Var "d"), SmallerEquals, Const (-1))),
                      Sequence (
                        Filter (Comparison (BinaryOperation (Var "d", Sub, Var "e"), SmallerEquals, Const (-2))),
                        Filter (Comparison (BinaryOperation (Var "a", Sub, Var "e"), BiggerEquals, Const (-5)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ));

    expect_bottom is_bottom eval
      "Assegnamenti affini concatenati: x=Random(0,50); y=x-4; z=y+2; Filter(x-z != 2)"
      (Sequence (
        Assign ("x", Random (0, 50)),
        Sequence (
          Assign ("y", BinaryOperation (Var "x", Sub, Const 4)),
          Sequence (
            Assign ("z", BinaryOperation (Var "y", Add, Const 2)),
            Filter (Comparison (BinaryOperation (Var "x", Sub, Var "z"), NotEquals, Const 2))
          )
        )
      ));

    expect_bottom is_bottom eval
      "If con branch merging e bound relazionale: y=x+2 o y=x+5; Filter(y-x < 2)"
      (Sequence (
        Assign ("x", Random (0, 20)),
        Sequence (
          If (Comparison (Random (0, 1), Equals, Const 0),
              Assign ("y", BinaryOperation (Var "x", Add, Const 2)),
              Assign ("y", BinaryOperation (Var "x", Add, Const 5))),
          Filter (Comparison (BinaryOperation (Var "y", Sub, Var "x"), Smaller, Const 2))
        )
      ));
  ]

  (* 2. TEST SPECIFICI PER GLI OTTAGONI (relazioni con somme e variabili negate) *)
  let octagon_advanced_tests = [
    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Assegnamento affine negativo y=-x+10; Filter(x+y != 10)"
      (Sequence (
        Assign ("x", Random (0, 20)),
        Sequence (
          Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10)),
          Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), NotEquals, Const 10))
        )
      ));

    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Chiusura forte mista x+y<=4; -y+z<=2; Filter(x+z >= 10)"
      (Sequence (
        Assign ("x", Random (0, 50)),
        Sequence (
          Assign ("y", Random (0, 50)),
          Sequence (
            Assign ("z", Random (0, 50)),
            Sequence (
              Filter (Comparison (BinaryOperation (Var "x", Add, Var "y"), SmallerEquals, Const 4)),
              Sequence (
                Filter (Comparison (BinaryOperation (UnaryOperation (Negation, Var "y"), Add, Var "z"), SmallerEquals, Const 2)),
                Filter (Comparison (BinaryOperation (Var "x", Add, Var "z"), BiggerEquals, Const 10))
              )
            )
          )
        )
      ));

    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Doppia negazione affine y=-x; z=-y; Filter(x-z != 0)"
      (Sequence (
        Assign ("x", Random (1, 10)),
        Sequence (
          Assign ("y", UnaryOperation (Negation, Var "x")),
          Sequence (
            Assign ("z", UnaryOperation (Negation, Var "y")),
            Filter (Comparison (BinaryOperation (Var "x", Sub, Var "z"), NotEquals, Const 0))
          )
        )
      ));
  ]

  (* 3. TEST AVANZATI PER GLI INTERVALLI (Cicli, Narrowing e Aritmetica) *)
  let is_interval_bottom = function
    | IntervalInterp.BottomEnv -> true
    | IntervalInterp.Env _ -> false

  let intervals_advanced_tests = [
    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While narrowing con incremento a passo 3: x=0; while(x<10) x=x+3; Filter(x > 15)"
      (Sequence (
        Assign ("x", Const 0),
        Sequence (
          While (Comparison (Var "x", Smaller, Const 10),
            Assign ("x", BinaryOperation (Var "x", Add, Const 3))),
          Filter (Comparison (Var "x", Bigger, Const 15))
        )
      ));

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While narrowing decremento a 0: x=10; while(x>0) x=x-2; Filter(x > 0)"
      (Sequence (
        Assign ("x", Const 10),
        Sequence (
          While (Comparison (Var "x", Bigger, Const 0),
            Assign ("x", BinaryOperation (Var "x", Sub, Const 2))),
          Filter (Comparison (Var "x", Bigger, Const 0))
        )
      ));

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: Prodotto di intervalli discordi: x in [2,5]; y in [-4,-2]; z=x*y; Filter(z >= 0)"
      (Sequence (
        Assign ("x", Random (2, 5)),
        Sequence (
          Assign ("y", Random (-4, -2)),
          Sequence (
            Assign ("z", BinaryOperation (Var "x", Mul, Var "y")),
            Filter (Comparison (Var "z", BiggerEquals, Const 0))
          )
        )
      ));

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: Divisione sicura: x in [20,40]; y in [2,4]; z=x/y; Filter(z < 4)"
      (Sequence (
        Assign ("x", Random (20, 40)),
        Sequence (
          Assign ("y", Random (2, 4)),
          Sequence (
            Assign ("z", BinaryOperation (Var "x", Div, Var "y")),
            Filter (Comparison (Var "z", Smaller, Const 4))
          )
        )
      ));
  ]

  (* 4. TEST AVANZATI PER I DOMINI DEI SEGNI *)
  let make_sign_advanced_tests is_bottom eval = [
    expect_bottom is_bottom eval
      "Signs: Negazione di positivo: x in [1,10]; y=-x; Filter(y > 0)"
      (Sequence (
        Assign ("x", Random (1, 10)),
        Sequence (
          Assign ("y", UnaryOperation (Negation, Var "x")),
          Filter (Comparison (Var "y", Bigger, Const 0))
        )
      ));

    expect_bottom is_bottom eval
      "Signs: Prodotto tra opposti: x in [1,10]; y in [-10,-1]; z=x*y; Filter(z > 0)"
      (Sequence (
        Assign ("x", Random (1, 10)),
        Sequence (
          Assign ("y", Random (-10, -1)),
          Sequence (
            Assign ("z", BinaryOperation (Var "x", Mul, Var "y")),
            Filter (Comparison (Var "z", Bigger, Const 0))
          )
        )
      ));

    expect_bottom is_bottom eval
      "Signs: If con entrambi rami positivi: if (b) x=5 else x=10; Filter(x < 0)"
      (Sequence (
        If (Boolean true, Assign ("x", Const 5), Assign ("x", Const 10)),
        Filter (Comparison (Var "x", Smaller, Const 0))
      ));

    expect_bottom is_bottom eval
      "Signs: While mai eseguito: x=-5; while(x>0) x=x+1; Filter(x > 0)"
      (Sequence (
        Assign ("x", Const (-5)),
        Sequence (
          While (Comparison (Var "x", Bigger, Const 0), Assign ("x", BinaryOperation (Var "x", Add, Const 1))),
          Filter (Comparison (Var "x", Bigger, Const 0))
        )
      ));
  ]
end

(* 2. Esecuzione tramite Alcotest *)
let () =
  Alcotest.run "Abstract Interpreter Tests" (
    List.map (fun (name, test_list) -> ("ExtendedSigns: " ^ name, test_list)) TestSuite_ExtendedSigns.tests @
    List.map (fun (name, test_list) -> ("SimplifiedSigns: " ^ name, test_list)) TestSuite_SimplifiedSigns.tests @
    List.map (fun (name, test_list) -> ("SimpleSigns: " ^ name, test_list)) TestSuite_SimpleSigns.tests @
    List.map (fun (name, test_list) -> ("StrangeSigns: " ^ name, test_list)) TestSuite_StrangeSigns.tests @
    List.map (fun (name, test_list) -> ("Signs: " ^ name, test_list)) TestSuite_Signs.tests @
    List.map (fun (name, test_list) -> ("Intervals: "^ name, test_list)) TestSuite_Intervals.tests @
    List.map (fun (name, test_list) -> ("Zones: " ^ name, test_list)) TestSuite_Zones.tests @
    List.map (fun (name, test_list) -> ("Octagons: " ^ name, test_list)) TestSuite_Octagons.tests @
    OctagonSpecificTests.tests @
    [
      "Zones: Sfide Relazionali Avanzate",
        AdvancedInterpreterTests.make_shared_relational_tests Abstract_domains.Zones.is_bottom ZoneInterp.eval;
      "Octagons: Sfide Relazionali Avanzate",
        AdvancedInterpreterTests.make_shared_relational_tests Abstract_domains.Octagons.is_bottom OctagonInterp.eval;
      "Octagons: Sfide Specifiche Ottagonali",
        AdvancedInterpreterTests.octagon_advanced_tests;
      "Intervals: Sfide Avanzate Narrowing e Aritmetica",
        AdvancedInterpreterTests.intervals_advanced_tests;
      "ExtendedSigns: Sfide Avanzate Segni",
        AdvancedInterpreterTests.make_sign_advanced_tests (function ExtendedSignInterp.BottomEnv -> true | _ -> false) ExtendedSignInterp.eval;
      "SimplifiedSigns: Sfide Avanzate Segni",
        AdvancedInterpreterTests.make_sign_advanced_tests (function SimplifiedSignInterp.BottomEnv -> true | _ -> false) SimplifiedSignInterp.eval;
    ]
  )
