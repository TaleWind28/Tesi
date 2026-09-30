open Syntax
open Parser

let check_cmd name expected actual =
  Alcotest.(check bool) name true (expected = actual)

let check_exp name expected actual =
  Alcotest.(check bool) name true (expected = actual)

let check_cond name expected actual =
  Alcotest.(check bool) name true (expected = actual)

let tests = [
  ("Parser: Costanti e Identificatori", `Quick, fun () ->
    check_exp "int 42" (Const 42) (parse_exp "42");
    check_exp "int negativo -7" (Const (-7)) (parse_exp "-7");
    check_exp "identificatore x" (Var "x") (parse_exp "x");
    check_exp "identificatore con underscore" (Var "var_1") (parse_exp "var_1")
  );

  ("Parser: Operazioni Aritmetiche e Precedenza", `Quick, fun () ->
    (* 1 + 2 * 3 deve essere 1 + (2 * 3) *)
    let expected = BinaryOperation (Const 1, Add, BinaryOperation (Const 2, Mul, Const 3)) in
    check_exp "precedenza mul su add" expected (parse_exp "1 + 2 * 3");

    (* (1 + 2) * 3 con parentesi *)
    let expected2 = BinaryOperation (BinaryOperation (Const 1, Add, Const 2), Mul, Const 3) in
    check_exp "parentesi su add" expected2 (parse_exp "(1 + 2) * 3");

    (* 10 - 4 - 2 associativo a sinistra: (10 - 4) - 2 *)
    let expected3 = BinaryOperation (BinaryOperation (Const 10, Sub, Const 4), Sub, Const 2) in
    check_exp "associativita sinistra sub" expected3 (parse_exp "10 - 4 - 2");

    (* 20 / 4 / 2 *)
    let expected4 = BinaryOperation (BinaryOperation (Const 20, Div, Const 4), Div, Const 2) in
    check_exp "associativita sinistra div" expected4 (parse_exp "20 / 4 / 2")
  );

  ("Parser: Operatore Unario -x e Doppia Negazione", `Quick, fun () ->
    check_exp "unario -x" (UnaryOperation (Negation, Var "x")) (parse_exp "-x");
    check_exp "doppia negazione --x" (UnaryOperation (Negation, UnaryOperation (Negation, Var "x"))) (parse_exp "--x");
    let exp_complex = BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10) in
    check_exp "-x + 10" exp_complex (parse_exp "-x + 10")
  );

  ("Parser: nondet e Random", `Quick, fun () ->
    check_exp "nondet(1, 5)" (Random (1, 5)) (parse_exp "nondet(1, 5)");
    check_exp "Random(-10, 10)" (Random (-10, 10)) (parse_exp "Random(-10, 10)");
    check_exp "nondet(0, 2 + 3)" (Random (0, 5)) (parse_exp "nondet(0, 2 + 3)")
  );

  ("Parser: inc e dec", `Quick, fun () ->
    check_exp "inc(x)" (Inc (Var "x")) (parse_exp "inc(x)");
    check_exp "dec(Const 5)" (Dec (Const 5)) (parse_exp "dec(5)");
    check_exp "x + 1 come inc(x)" (Inc (Var "x")) (parse_exp "x + 1");
    check_exp "x - 1 come dec(x)" (Dec (Var "x")) (parse_exp "x - 1");
    check_exp "x++ come inc(x)" (Inc (Var "x")) (parse_exp "x++");
    check_exp "x-- come dec(x)" (Dec (Var "x")) (parse_exp "x--");
    check_exp "++x come inc(x)" (Inc (Var "x")) (parse_exp "++x")
  );

  ("Parser: Condizioni Relazionali comp", `Quick, fun () ->
    check_cond "x > 0" (Comparison (Var "x", Bigger, Const 0)) (parse_cond "x > 0");
    check_cond "x >= 5" (Comparison (Var "x", BiggerEquals, Const 5)) (parse_cond "x >= 5");
    check_cond "x < 10" (Comparison (Var "x", Smaller, Const 10)) (parse_cond "x < 10");
    check_cond "x <= y" (Comparison (Var "x", SmallerEquals, Var "y")) (parse_cond "x <= y");
    check_cond "x == y" (Comparison (Var "x", Equals, Var "y")) (parse_cond "x == y");
    check_cond "x = y" (Comparison (Var "x", Equals, Var "y")) (parse_cond "x = y");
    check_cond "x != 0" (Comparison (Var "x", NotEquals, Const 0)) (parse_cond "x != 0");
    check_cond "x <> 0" (Comparison (Var "x", NotEquals, Const 0)) (parse_cond "x <> 0")
  );

  ("Parser: Condizioni Booleane not, and, or", `Quick, fun () ->
    check_cond "true" (Boolean true) (parse_cond "true");
    check_cond "false" (Boolean false) (parse_cond "false");
    check_cond "not (x > 0)" (Not (Comparison (Var "x", Bigger, Const 0))) (parse_cond "not (x > 0)");
    
    (* Precedenza and > or: (x > 0 and y < 5) or z == 1 *)
    let expected_and_or = Or (
      And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "y", Smaller, Const 5)),
      Comparison (Var "z", Equals, Const 1)
    ) in
    check_cond "and ha precedenza su or" expected_and_or (parse_cond "x > 0 and y < 5 or z == 1")
  );

  ("Parser: Comandi Skip, Assign, Sequence", `Quick, fun () ->
    check_cmd "Skip" Skip (parse_cmd "Skip");
    check_cmd "skip minuscolo" Skip (parse_cmd "skip");
    check_cmd "x = 5" (Assign ("x", Const 5)) (parse_cmd "x = 5");
    check_cmd "x++ comando" (Assign ("x", Inc (Var "x"))) (parse_cmd "x++");
    check_cmd "x-- comando" (Assign ("x", Dec (Var "x"))) (parse_cmd "x--");
    check_cmd "++x comando" (Assign ("x", Inc (Var "x"))) (parse_cmd "++x");
    check_cmd "--x comando" (Assign ("x", Dec (Var "x"))) (parse_cmd "--x");
    check_cmd "x = 1; y = 2" (Sequence (Assign ("x", Const 1), Assign ("y", Const 2))) (parse_cmd "x = 1; y = 2");
    check_cmd "x = 1; x--" (Sequence (Assign ("x", Const 1), Assign ("x", Dec (Var "x")))) (parse_cmd "x = 1; x--;");
    check_cmd "trailing semicolon" (Sequence (Assign ("x", Const 1), Assign ("y", Const 2))) (parse_cmd "x = 1; y = 2;")
  );

  ("Parser: Filtri cond ?", `Quick, fun () ->
    check_cmd "x > 0 ?" (Filter (Comparison (Var "x", Bigger, Const 0))) (parse_cmd "x > 0 ?");
    check_cmd "x <= 5 ?" (Filter (Comparison (Var "x", SmallerEquals, Const 5))) (parse_cmd "x <= 5 ?");
    check_cmd "(x > 0 and y < 10) ?" (Filter (And (Comparison (Var "x", Bigger, Const 0), Comparison (Var "y", Smaller, Const 10)))) (parse_cmd "(x > 0 and y < 10) ?");
    check_cmd "filter(x == 5)" (Filter (Comparison (Var "x", Equals, Const 5))) (parse_cmd "filter(x == 5)")
  );

  ("Parser: If-Then-Else e While-Do", `Quick, fun () ->
    let expected_if = If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1), Assign ("y", Const 2)) in
    check_cmd "if then else" expected_if (parse_cmd "if x > 0 then y = 1 else y = 2");

    let expected_while = While (Comparison (Var "x", Bigger, Const 0), Assign ("x", Dec (Var "x"))) in
    check_cmd "while do" expected_while (parse_cmd "while x > 0 do x = x - 1")
  );

  ("Parser: Programma Completo (README)", `Quick, fun () ->
    let prog_str = "
      // Esempio da README.md
      x = Random(1, 5);
      y = -x + 10;
      z = x + 2
    " in
    let expected =
      Sequence (
        Assign ("x", Random (1, 5)),
        Sequence (
          Assign ("y", BinaryOperation (UnaryOperation (Negation, Var "x"), Add, Const 10)),
          Assign ("z", BinaryOperation (Var "x", Add, Const 2))
        )
      )
    in
    check_cmd "programma da README" expected (parse_cmd prog_str)
  );

  ("Parser: Sequenza in If e While con parentesi", `Quick, fun () ->
    let prog_str = "
      if x > 0 then {
        y = 1;
        z = 2
      } else {
        y = 0;
        z = 0
      };
      w = y + z
    " in
    let expected =
      Sequence (
        If (
          Comparison (Var "x", Bigger, Const 0),
          Sequence (Assign ("y", Const 1), Assign ("z", Const 2)),
          Sequence (Assign ("y", Const 0), Assign ("z", Const 0))
        ),
        Assign ("w", BinaryOperation (Var "y", Add, Var "z"))
      )
    in
    check_cmd "blocchi e sequenze" expected (parse_cmd prog_str)
  );

  ("Parser: Valutazione End-to-End con Interprete Astratto", `Quick, fun () ->
    let prog = parse_cmd "x = nondet(1, 5); y = -x + 10; z = x + 2" in
    let res = Interpeters.IntervalInterp.eval prog in
    match res with
    | Interpeters.IntervalInterp.BottomEnv ->
        Alcotest.fail "Lo stato dell'interprete non dovrebbe essere Bottom"
    | Interpeters.IntervalInterp.Env env ->
        let x_val = Hashtbl.find env "x" in
        let y_val = Hashtbl.find env "y" in
        let z_val = Hashtbl.find env "z" in
        Alcotest.(check bool) "x in [1, 5]" true (x_val = Abstract_domains.Intervals.abstract_range 1 5);
        Alcotest.(check bool) "y in [5, 9]" true (y_val = Abstract_domains.Intervals.abstract_range 5 9);
        Alcotest.(check bool) "z in [3, 7]" true (z_val = Abstract_domains.Intervals.abstract_range 3 7)
  )
]
