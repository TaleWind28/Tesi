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

  let tests = [
    "Assegnamenti", assigntests;
    "Shift e Offset", shifttests;
    "Operazioni Aritmetiche", binoptests;
    "Sequenze", sequencetests;
    "Skip", skiptests;
    "Filtri", filtertests;
    "Filtri Contraddittori (Bottom)", bottomtests;
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
    ]
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
    OctagonSpecificTests.tests
  )
