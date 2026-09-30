open Abstract_domains
open Interpreters
open Oracles
open Syntax
open Parsers

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
    "Sum: Pos + Pos", parse_exp "x + x", E.sum_1;
    "Sum: Pos + Neg", parse_exp "x + y", E.sum_2;
    "Sum: Neg + Neg", parse_exp "y + y", E.sum_3;
    "Sum: Pos + Zero", parse_exp "x + z", E.sum_4;
    "Sum: Zero + Zero", parse_exp "z + z", E.sum_5;
    "Sum: PosZero + PosZero", parse_exp "w + w", E.sum_6;
    "Sum: NegZero + NegZero", parse_exp "k + k", E.sum_7;
    "Sum: PosZero + NegZero", parse_exp "w + k", E.sum_8;
    "Sum: PosZero + Pos", parse_exp "w + x", E.sum_9;
    "Sum: PosZero + Neg", parse_exp "w + y", E.sum_10;
    "Sum: NegZero + Pos", parse_exp "k + x", E.sum_11;
    "Sum: NegZero + Neg", parse_exp "k + y", E.sum_12;
    "Sum: NonZero + Pos", parse_exp "n + x", E.sum_13;
    "Sum: NonZero + Zero", parse_exp "n + z", E.sum_14;
    "Sum: NonZero + NonZero", parse_exp "n + n", E.sum_15;
    "Sum: Top + Pos", parse_exp "t + x", E.sum_16;
    "Sum: Bottom + Pos", parse_exp "b + x", E.sum_17;
    "Sum: 10 + (-20)", parse_exp "10 + (-20)", E.sum_18;
  ]

  let subtests = List.map make_case [
    "Sub: Pos - Neg", parse_exp "x - y", E.sub_1;
    "Sub: Pos - Pos (stessa var)", parse_exp "x - x", E.sub_2;
    "Sub: 10 - 20", parse_exp "10 - 20", E.sub_3;
    "Sub: PosZero - PosZero", parse_exp "w - w", E.sub_4;
    "Sub: Zero - Neg", parse_exp "z - y", E.sub_5;
  ]

  let multests = List.map make_case [
    "Mul: Pos * Pos", parse_exp "x * x", E.mul_1;
    "Mul: Pos * Neg", parse_exp "x * y", E.mul_2;
    "Mul: Neg * Neg", parse_exp "y * y", E.mul_3;
    "Mul: Pos * Zero", parse_exp "x * z", E.mul_4;
    "Mul: PosZero * Neg", parse_exp "w * y", E.mul_5;
    "Mul: NegZero * Pos", parse_exp "k * x", E.mul_6;
    "Mul: NonZero * Zero", parse_exp "n * z", E.mul_7;
    "Mul: NonZero * NonZero", parse_exp "n * n", E.mul_8;
    "Mul: Top * Zero", parse_exp "t * z", E.mul_9;
    "Mul: Bottom * Pos", parse_exp "b * x", E.mul_10;
  ]

  let divtests = List.map make_case [
    "Div: Pos / Pos", parse_exp "x / x", E.div_1;
    "Div: Pos / Neg", parse_exp "x / y", E.div_2;
    "Div: Neg / Neg", parse_exp "y / y", E.div_3;
    "Div: Costante / Zero", parse_exp "10 / z", E.div_4;
    "Div: Pos / PosZero", parse_exp "x / w", E.div_5;
    "Div: Pos / NegZero", parse_exp "x / k", E.div_6;
    "Div: Pos / NonZero", parse_exp "x / n", E.div_7;
    "Div: Zero / Pos", parse_exp "z / x", E.div_8;
    "Div: Zero / Neg", parse_exp "z / y", E.div_9;
    "Div: Top / Pos", parse_exp "t / x", E.div_10;
    "Div: PosZero / Neg", parse_exp "w / y", E.div_11;
    "Div: NegZero / Pos", parse_exp "k / x", E.div_12;
  ]

  let negatetests = List.map make_case [
    "Negate: Pos", parse_exp "-x", E.neg_1;
    "Negate: Neg", parse_exp "-y", E.neg_2;
    "Negate: Zero", parse_exp "-z", E.neg_3;
    "Negate: PosZero", parse_exp "-w", E.neg_4;
    "Negate: NegZero", parse_exp "-k", E.neg_5;
    "Negate: NonZero", parse_exp "-n", E.neg_6;
    "Negate: Top", parse_exp "-t", E.neg_7;
    "Negate: Bottom", parse_exp "-b", E.neg_8;
    "Doppia negazione: --Pos", parse_exp "--x", E.neg_9;
    "Pos + (-Neg)", parse_exp "x + (-y)", E.neg_10;
  ]

  let randomtests = List.map make_case [
    "Random(-1,10)", parse_exp "Random(-1, 10)", E.rand_1;
    "Random(1,10)", parse_exp "Random(1, 10)", E.rand_2;
    "Random(-10,-1)", parse_exp "Random(-10, -1)", E.rand_3;
    "Random(0,10)", parse_exp "Random(0, 10)", E.rand_4;
    "Random(-10,0)", parse_exp "Random(-10, 0)", E.rand_5;
    "Random(0,0)", parse_exp "Random(0, 0)", E.rand_6;
  ]

  let inctests = List.map make_case [
    "Inc: Pos", parse_exp "inc(x)", E.inc_1;
    "Inc: Neg", parse_exp "inc(y)", E.inc_2;
    "Inc: Zero", parse_exp "inc(z)", E.inc_3;
    "Inc: PosZero", parse_exp "inc(w)", E.inc_4;
    "Inc: NegZero", parse_exp "inc(k)", E.inc_5;
    "Inc: NonZero", parse_exp "inc(n)", E.inc_6;
    "Inc: Top", parse_exp "inc(t)", E.inc_7;
    "Inc: Bottom", parse_exp "inc(b)", E.inc_8;
    "Inc: Annidato inc(inc(z))", parse_exp "inc(inc(z))", E.inc_9;
    "Inc/Dec: dec(inc(x))", parse_exp "dec(inc(x))", E.inc_10;
  ]

  let dectests = List.map make_case [
    "Dec: Pos", parse_exp "dec(x)", E.dec_1;
    "Dec: Neg", parse_exp "dec(y)", E.dec_2;
    "Dec: Zero", parse_exp "dec(z)", E.dec_3;
    "Dec: PosZero", parse_exp "dec(w)", E.dec_4;
    "Dec: NegZero", parse_exp "dec(k)", E.dec_5;
    "Dec: NonZero", parse_exp "dec(n)", E.dec_6;
    "Dec: Top", parse_exp "dec(t)", E.dec_7;
    "Dec: Bottom", parse_exp "dec(b)", E.dec_8;
    "Dec: Annidato dec(dec(z))", parse_exp "dec(dec(z))", E.dec_9;
    "Dec/Inc: inc(dec(x))", parse_exp "inc(dec(x))", E.dec_10;
  ]

  let inc_dec_assigntests = [
    make_prog_case
      ( "Assign con Inc: x = 5; x = inc(x)",
        parse_cmd "x = 5; x = inc(x)",
        [ "x", E.assign_inc_5 ] );
    make_prog_case
      ( "Assign con Dec: x = 5; x = dec(x)",
        parse_cmd "x = 5; x = dec(x)",
        [ "x", E.assign_dec_5 ] );
    make_prog_case
      ( "Assign con Inc e Dec a nuove variabili",
        parse_cmd "x = 0; y = inc(x); z = dec(x)",
        [ "y", E.assign_inc_0_y; "z", E.assign_dec_0_z ] );
    make_prog_case
      ( "Sequenza di 3 Inc: x = 0; x = inc(x); x = inc(x); x = inc(x)",
        parse_cmd "x = 0; x = inc(x); x = inc(x); x = inc(x)",
        [ "x", E.assign_seq_3_inc ] );
    make_prog_case
      ( "Sequenza di 3 Dec: x = 0; x = dec(x); x = dec(x); x = dec(x)",
        parse_cmd "x = 0; x = dec(x); x = dec(x); x = dec(x)",
        [ "x", E.assign_seq_3_dec ] );
    make_prog_case
      ( "Alternanza Inc e Dec: x = 10; x = inc(x); x = dec(x)",
        parse_cmd "x = 10; x = inc(x); x = dec(x)",
        [ "x", E.assign_inc_dec_cancel ] );
    make_prog_case
      ( "Inc con Random: x = Random(1, 5); y = inc(x)",
        parse_cmd "x = Random(1, 5); y = inc(x)",
        [ "y", E.assign_inc_rand_y ] );
    make_prog_case
      ( "Dec con Random: x = Random(1, 5); y = dec(x)",
        parse_cmd "x = Random(1, 5); y = dec(x)",
        [ "y", E.assign_dec_rand_y ] );
  ]

  let inc_dec_filtertests = [
    check_filter_case
      ( "Filtro contraddittorio su Inc: x=5; y=inc(x); Filter(y <= 5)",
        parse_cmd "x = 5; y = inc(x); y <= 5 ?",
        E.filter_inc_contra_1 );
    check_filter_case
      ( "Filtro contraddittorio su Dec: x=5; y=dec(x); Filter(y >= 5)",
        parse_cmd "x = 5; y = dec(x); y >= 5 ?",
        E.filter_dec_contra_1 );
    check_filter_case
      ( "Filtro contraddittorio con Random e Inc: x in [1,5]; y=inc(x); Filter(y < 2)",
        parse_cmd "x = Random(1, 5); y = inc(x); y < 2 ?",
        E.filter_inc_rand_contra );
    check_filter_case
      ( "Filtro contraddittorio con Random e Dec: x in [1,5]; y=dec(x); Filter(y > 4)",
        parse_cmd "x = Random(1, 5); y = dec(x); y > 4 ?",
        E.filter_dec_rand_contra );
    check_filter_case
      ( "Filtro diretto su Inc: x=10; Filter(inc(x) <= 10)",
        parse_cmd "x = 10; inc(x) <= 10 ?",
        E.filter_direct_inc_contra );
    check_filter_case
      ( "Filtro diretto su Dec: x=10; Filter(dec(x) >= 10)",
        parse_cmd "x = 10; dec(x) >= 10 ?",
        E.filter_direct_dec_contra );
    check_filter_case
      ( "Catena di 3 Inc e filtro != 3",
        parse_cmd "x = 0; x = inc(inc(inc(x))); x != 3 ?",
        E.filter_chain_3_inc_contra );
    check_filter_case
      ( "Catena di 3 Dec e filtro != 0",
        parse_cmd "x = 3; x = dec(dec(dec(x))); x != 0 ?",
        E.filter_chain_3_dec_contra );
    make_prog_case
      ( "Filtro valido su Inc preserva lo stato",
        parse_cmd "x = 5; y = inc(x); y == 6 ?",
        [ "x", E.filter_inc_valid_x; "y", E.filter_inc_valid_y ] );
    make_prog_case
      ( "Filtro valido su Dec preserva lo stato",
        parse_cmd "x = 5; y = dec(x); y == 4 ?",
        [ "x", E.filter_dec_valid_x; "y", E.filter_dec_valid_y ] );
    check_filter_case
      ( "If con guardia Inc contraddittoria",
        parse_cmd "x = 0; if inc(x) > 0 then y = 1 else y = 2; y == 2 ?",
        E.filter_if_inc_contra );
    check_filter_case
      ( "If con guardia Dec contraddittoria",
        parse_cmd "x = 0; if dec(x) < 0 then y = 1 else y = 2; y == 2 ?",
        E.filter_if_dec_contra );
    check_filter_case
      ( "While decrementale mai eseguito contraddittorio",
        parse_cmd "x = -5; while x > 0 do x = dec(x); x > 0 ?",
        E.filter_while_dec_contra );
  ]

  (* ------------------------------------------------------------ *)
  (* TEST COMANDI                                                 *)
  (* ------------------------------------------------------------ *)

  let assigntests = List.map make_prog_case [
    "Assign semplice: x = 5", parse_cmd "x = 5", [ "x", E.assign_1 ];
    "Assign semplice: x = -5", parse_cmd "x = -5", [ "x", E.assign_2 ];
    "Assign semplice: x = 0", parse_cmd "x = 0", [ "x", E.assign_3 ];
    "Assign con variabile non definita: y = x", parse_cmd "y = x", [ "y", E.assign_4 ];
    "Assign con Random: x = Random(1,10)", parse_cmd "x = Random(1, 10)", [ "x", E.assign_5 ];
  ]

  let sequencetests = List.map make_prog_case [
    "Sequence: x=5; y=-3",
      parse_cmd "x = 5; y = -3",
      [ "x", E.sequence_1_1; "y", E.sequence_1_2 ];
  ]

  let skiptests = [
    ( "Skip da solo non modifica lo stato (stato vuoto)", `Quick,
      fun () ->
        match Interp.eval (parse_cmd "skip") with
        | Interp.Env tbl -> Alcotest.(check int) "stato vuoto" 0 (Hashtbl.length tbl)
        | Interp.BottomEnv -> Alcotest.fail "Skip: stato inaspettatamente BottomEnv" );
    ( "Skip in mezzo a una sequenza non altera i valori", `Quick,
      fun () ->
        let prog = parse_cmd "x = 42; skip" in
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
        parse_cmd "x = 5; if x > 0 then y = 1 else y = -1",
        ["y", E.if_1] );
    make_prog_case_with_env
      ( "If certo vero (x>y)",
        parse_cmd "if x > y then k = 1 else k = 999",
        [ "k", E.if_2 ] );
    make_prog_case
      ( "If certo falso (y>x)",
        parse_cmd "x = 5; y = -3; if y > x then k = -999 else k = 2",
        [ "k", E.if_3 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Pos(5), else=Neg(-5)",
        parse_cmd "if w > x then k = 5 else k = -5",
        [ "k", E.if_4 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Pos(1), else=Zero(0)",
        parse_cmd "if w > x then k = 1 else k = 0",
        [ "k", E.if_5 ] );
    make_prog_case_with_env
      ( "Ambiguo: then=Neg(-1), else=Zero(0)",
        parse_cmd "if w > x then k = -1 else k = 0",
        [ "k", E.if_6 ] );
    make_prog_case_with_env
      ( "Ambiguo: catch-all",
        parse_cmd "if w > x then k = 3 else k = w * y",
        [ "k", E.if_7 ] );
    make_prog_case_with_env
      ( "Ambiguo: rami convergenti (entrambi Pos)",
        parse_cmd "if w > x then k = 10 else k = 20",
        [ "k", E.if_8 ] );
    make_prog_case_with_env
      ( "Ambiguo, var assegnata solo nel then",
        parse_cmd "if w > x then m = 7 else skip", [ "m", E.if_9 ]; );
    make_prog_case_with_env
      ( "Ambiguo, var assegnata solo nell'else",
        parse_cmd "if w > x then skip else m = -7", [ "m", E.if_10 ]; );
    make_prog_case_with_env
      ( "If annidato",
        parse_cmd "if w > x then (if w > x then k = 1 else k = -1) else k = 100",
        [ "k", E.if_11 ] );
    make_prog_case_with_env
      ( "If con And",
        parse_cmd "if x > y and w > x then k = 1 else k = -1",
        [ "k", E.if_12 ] );
    expect_bottom_with_env "Filter certo falso prima dell'If"
      (parse_cmd "filter(false); if true then k = 1 else k = 2")
  ]

  let whiletests = [
    make_prog_case
      ( "While mai eseguito",
        parse_cmd "x = 5; while x < 0 do x = x - 1",
        [ "x", E.while_1_1 ] );
    make_prog_case
      ( "While converge: x = 5; while x!=0 x = 0; -> risultato x = 0",
        parse_cmd "x = 5; while x != 0 do x = 0",
        [ "x", E.while_2_1 ] );
    make_prog_case
      ( "While perdita di precisione",
        parse_cmd "x = -5; while x < 0 do x = 1 + x",
        [ "x", E.while_3_1 ] );
      ( "Corpo = Skip (loop infinito)",
      `Quick,
      fun () ->
        let prog = parse_cmd "x = 5; while x > 0 do skip" in
        match Interp.eval prog with
        | Interp.BottomEnv -> ()
        | Interp.Env tbl ->
            (* Per domini senza Pos stretto (come SimpleSigns), il ciclo esce soundly con x = 0 *)
            (match Hashtbl.find_opt tbl "x" with
             | Some v -> Alcotest.(check sign_testable) "x deve valere 0 (Zero o PosZero)" (E.while_3_2) v
             | None -> Alcotest.fail "Variabile 'x' non trovata nello stato finale") );
    expect_bottom "Filter(false) prima del while"
      (parse_cmd "filter(false); while true do x = 1");
    make_prog_case
      ( "Personal While Test",
        parse_cmd "x = 1; y = 2; z = Random(-3, 5); while x == y do if y > z then x = x + (-2) else y = x + y",
        [ "x", E.while_4_1; "y", E.while_4_2; "z", E.while_4_3 ] );
  ]

  let contradiction_tests = [
    check_filter_case
      ("Filtro false booleano", parse_cmd "filter(false)", E.filter_1);
    check_filter_case
      ("Filtro costante: 5 < 2", parse_cmd "5 < 2 ?", E.filter_2);
    check_filter_case
      ("Filtro costante: 5 == 2", parse_cmd "5 == 2 ?", E.filter_3);
    check_filter_case
      ("Filtro costante contraddittorio: 5 <= -1", parse_cmd "5 <= -1 ?", E.filter_4);
    check_filter_case
      ("Filtro costante contraddittorio: -3 > 0", parse_cmd "-3 > 0 ?", E.filter_5);
    check_filter_case
      ("Filtro costante contraddittorio: 0 != 0", parse_cmd "0 != 0 ?", E.filter_6);
    check_filter_case ~with_env:true
      ("Filtro x < 0 con x Pos", parse_cmd "x < 0 ?", E.filter_7);
    check_filter_case ~with_env:true
      ("Filtro y > 0 con y Neg", parse_cmd "y > 0 ?", E.filter_8);
    check_filter_case ~with_env:true
      ("Filtro z != 0 con z Zero", parse_cmd "z != 0 ?", E.filter_9);
    check_filter_case ~with_env:true
      ("Filtro x < y con x Pos e y Neg", parse_cmd "x < y ?", E.filter_10);
    check_filter_case ~with_env:true
      ("Filtro y > x con x Pos e y Neg", parse_cmd "y > x ?", E.filter_11);
    check_filter_case ~with_env:true
      ("Filtro x == y con x Pos e y Neg", parse_cmd "x == y ?", E.filter_12);
    check_filter_case ~with_env:true
      ("Filtro And (true, false)", parse_cmd "filter(true and false)", E.filter_13);
    check_filter_case ~with_env:true
      ("Filtro Not (x > y) con x Pos e y Neg", parse_cmd "filter(not (x > y))", E.filter_14);
    check_filter_case
      ("Filtro sequenza: x = 10; Filter(x <= 0)", parse_cmd "x = 10; x <= 0 ?", E.filter_15);
  ]

  let tests = [
    "Somma", sumtests;
    "Sottrazione", subtests;
    "Moltiplicazione", multests;
    "Divisione", divtests;
    "Negazione Unaria", negatetests;
    "Random", randomtests;
    "Incremento", inctests;
    "Decremento", dectests;
    "Assegnamenti", assigntests;
    "Inc/Dec Assegnamenti", inc_dec_assigntests;
    "Sequenze", sequencetests;
    "Skip", skiptests;
    "Istruzioni Condizionali", iftests;
    "Cicli While", whiletests;
    "Filtri Contraddittori Base", contradiction_tests;
    "Inc/Dec Filtri e Contraddizioni", inc_dec_filtertests;
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
    "Assign costante positiva: x = 5", parse_cmd "x = 5", [ "x", E.assign_const_1 ];
    "Assign costante negativa: x = -5", parse_cmd "x = -5", [ "x", E.assign_const_2 ];
    "Assign zero: x = 0", parse_cmd "x = 0", [ "x", E.assign_const_3 ];
    "Assign Random positivo: x = Random(1,10)", parse_cmd "x = Random(1, 10)", [ "x", E.assign_rand_1 ];
    "Assign Random misto: x = Random(-5,5)", parse_cmd "x = Random(-5, 5)", [ "x", E.assign_rand_2 ];
    "Assign tra variabili: x = 5; y = x",
      parse_cmd "x = 5; y = x",
      [ "x", E.assign_var_1_x; "y", E.assign_var_1_y ];
    "Assign tra variabili con Random: x = Random(1,5); y = x",
      parse_cmd "x = Random(1, 5); y = x",
      [ "x", E.assign_var_rand_x; "y", E.assign_var_rand_y ];
  ]

  (* ------------------------------------------------------------ *)
  (* TEST SHIFT E OFFSET                                          *)
  (* ------------------------------------------------------------ *)
  let shifttests = List.map make_prog_case [
    "Shift positivo: x = 5; x = x + 3",
      parse_cmd "x = 5; x = x + 3",
      [ "x", E.shift_pos ];
    "Shift negativo: x = 5; x = x + (-10)",
      parse_cmd "x = 5; x = x + (-10)",
      [ "x", E.shift_neg ];
    "Assign con offset tra due variabili: x = 5; y = x + 2",
      parse_cmd "x = 5; y = x + 2",
      [ "x", E.assign_var_offset_x; "y", E.assign_var_offset_y ];
  ]

  let binoptests = List.map make_prog_case [
    "BinOp Add: x=3; y=4; z=x+y",
      parse_cmd "x = 3; y = 4; z = x + y",
      [ "z", E.binop_add ];
    "BinOp Sub: x=10; y=2; z=x-y",
      parse_cmd "x = 10; y = 2; z = x - y",
      [ "z", E.binop_sub ];
    "BinOp Mul: x=10; y=2; z=x*y",
      parse_cmd "x = 10; y = 2; z = x * y",
      [ "z", E.binop_mul ];
    "BinOp Div: x=10; y=2; z=x/y",
      parse_cmd "x = 10; y = 2; z = x / y",
      [ "z", E.binop_div ];
    "UnOp Negation: x=7; y=-x",
      parse_cmd "x = 7; y = -x",
      [ "y", E.unop_neg ];
  ]

  (* ------------------------------------------------------------ *)
  (* TEST SEQUENZE E SKIP                                         *)
  (* ------------------------------------------------------------ *)
  let sequencetests = List.map make_prog_case [
    "Sequence: x = 1; y = 2",
      parse_cmd "x = 1; y = 2",
      [ "x", E.seq_x; "y", E.seq_y ];
    "Skip in sequenza: x = 42; Skip",
      parse_cmd "x = 42; skip",
      [ "x", E.skip_val ];
  ]

  let skiptests = [
    ( "Skip da solo", `Quick, fun () ->
        let res = Interp.eval (parse_cmd "skip") in
        if D.is_bottom res then
          Alcotest.fail "Skip: stato inaspettatamente Bottom" );
  ]

  (* ------------------------------------------------------------ *)
  (* TEST FILTRI E RAFFINAMENTO                                   *)
  (* ------------------------------------------------------------ *)
  let filtertests = List.map make_prog_case [
    "Filter raffina limite superiore: x=Random(1,10); Filter(x <= 5)",
      parse_cmd "x = Random(1, 10); x <= 5 ?",
      [ "x", E.filter_refine_ub ];
    "Filter raffina limite inferiore: x=Random(1,10); Filter(x >= 6)",
      parse_cmd "x = Random(1, 10); x >= 6 ?",
      [ "x", E.filter_refine_lb ];
    "Filter raffina strettamente minore: x=Random(1,10); Filter(x < 5)",
      parse_cmd "x = Random(1, 10); x < 5 ?",
      [ "x", E.filter_refine_lt ];
    "Filter raffina strettamente maggiore: x=Random(1,10); Filter(x > 5)",
      parse_cmd "x = Random(1, 10); x > 5 ?",
      [ "x", E.filter_refine_gt ];
    "Filter tra variabili: x=5; y=3; Filter(x > y)",
      parse_cmd "x = 5; y = 3; x > y ?",
      [ "x", E.filter_rel_x; "y", E.filter_rel_y ];
    "Filter tra variabile ed espressione: x=4; y=-3; Filter(x > x+y)",
      parse_cmd "x = 4; y = -3; x > x + y ?",
      [ "x", E.filter_expr_x; "y", E.filter_expr_y ];
    "Filter uguaglianza: x=5; y=5; Filter(x == y)",
      parse_cmd "x = 5; y = 5; x == y ?",
      [ "x", E.filter_eq_x; "y", E.filter_eq_y ];
    "Filter booleano true: x=5; Filter(true)",
      parse_cmd "x = 5; filter(true)",
      [ "x", E.filter_true_x ];
    "Filter Not: x=5; Filter(not (x < 0))",
      parse_cmd "x = 5; filter(not (x < 0))",
      [ "x", E.filter_not_x ];
    "Filter And: x=5; Filter(x > 0 and x < 10)",
      parse_cmd "x = 5; filter(x > 0 and x < 10)",
      [ "x", E.filter_and_x ];
  ]

  (* ------------------------------------------------------------ *)
  (* ------------------------------------------------------------ *)
  (* TEST FILTRI CONTRADDITTORI (ATTESO BOTTOM)                   *)
  (* ------------------------------------------------------------ *)
  let bottomtests = [
    expect_bottom "Filtro impossibile su costante: x=5; Filter(x < 0)"
      (parse_cmd "x = 5; x < 0 ?");
    expect_bottom "Filtro impossibile su costante: x=5; Filter(x > 10)"
      (parse_cmd "x = 5; x > 10 ?");
    expect_bottom "Filtro uguaglianza incompatibile: x=5; Filter(x == 6)"
      (parse_cmd "x = 5; x == 6 ?");
    expect_bottom "Filtro tra variabili incompatibile: x=5; y=10; Filter(x > y)"
      (parse_cmd "x = 5; y = 10; x > y ?");
    expect_bottom "Filtro booleano false: x=5; Filter(false)"
      (parse_cmd "x = 5; filter(false)");
    expect_bottom "Filtro congiunzione incompatibile: x=5; Filter(x > 10 and x < 2)"
      (parse_cmd "x = 5; filter(x > 10 and x < 2)");
  ]

  (* ------------------------------------------------------------ *)
  (* TEST ISTRUZIONI CONDIZIONALI (IF)                            *)
  (* ------------------------------------------------------------ *)
  let iftests = [
    make_prog_case
      ( "If certo vero: if (x > 0) y = 1 else y = -1",
        parse_cmd "x = 5; if x > 0 then y = 1 else y = -1",
        [ "x", E.if_true_x; "y", E.if_true_y ] );

    make_prog_case
      ( "If certo falso: if (x < 0) y = 1 else y = -1",
        parse_cmd "x = 5; if x < 0 then y = 1 else y = -1",
        [ "x", E.if_false_x; "y", E.if_false_y ] );

    make_prog_case
      ( "If ambiguo: x=Random(1,10); if (x <= 5) y = 1 else y = 2",
        parse_cmd "x = Random(1, 10); if x <= 5 then y = 1 else y = 2",
        [ "y", E.if_ambig_y ] );

    make_prog_case
      ( "If ambiguo con range: x=Random(1,10); if (x <= 5) y = 10 else y = 20",
        parse_cmd "x = Random(1, 10); if x <= 5 then y = 10 else y = 20",
        [ "y", E.if_ambig_range_y ] );

    make_prog_case
      ( "If con raffinamento variabile: x=Random(1,10); if (x <= 5) x = x+10 else x = x-5",
        parse_cmd "x = Random(1, 10); if x <= 5 then x = x + 10 else x = x + (-5)",
        [ "x", E.if_refine_x ] );

    make_prog_case
      ( "If relazionale certo: x=10; y=20; if (x < y) z = 1 else z = 2",
        parse_cmd "x = 10; y = 20; if x < y then z = 1 else z = 2",
        [ "z", E.if_rel_true_z ] );

    make_prog_case
      ( "If relazionale ambiguo: x=Random(1,10); y=Random(1,10); if (x < y) k = 1 else k = 2",
        parse_cmd "x = Random(1, 10); y = Random(1, 10); if x < y then k = 1 else k = 2",
        [ "k", E.if_rel_ambig_k ] );

    make_prog_case
      ( "If annidato: x=5; if (x > 0) then (if (x < 10) y = 1 else y = 2) else y = 3",
        parse_cmd "x = 5; if x > 0 then (if x < 10 then y = 1 else y = 2) else y = 3",
        [ "y", E.if_nested_y ] );

    make_prog_case
      ( "If con assegnamento parziale: y=0; x=Random(1,5); if (x > 3) y = 7 else Skip",
        parse_cmd "y = 0; x = Random(1, 5); if x > 3 then y = 7 else skip",
        [ "y", E.if_partial_assign_y ] );

    expect_bottom "If con filtro falso prima dell'If"
      (parse_cmd "x = 5; filter(false); if true then y = 1 else y = 2");
  ]

  (* ------------------------------------------------------------ *)
  (* TEST CICLI WHILE                                             *)
  (* ------------------------------------------------------------ *)
  let whiletests = [
    make_prog_case
      ( "While mai eseguito: x = 5; while (x < 0) x = x + 1",
        parse_cmd "x = 5; while x < 0 do x = x + 1",
        [ "x", E.while_not_executed_x ] );

    make_prog_case
      ( "While incremento da negativo a zero: x = -5; while (x < 0) x = x + 1",
        parse_cmd "x = -5; while x < 0 do x = x + 1",
        [ "x", E.while_inc_from_neg_x ] );

    make_prog_case
      ( "While incremento da zero a dieci: x = 0; while (x < 10) x = x + 1",
        parse_cmd "x = 0; while x < 10 do x = x + 1",
        [ "x", E.while_inc_from_zero_x ] );

    make_prog_case
      ( "While decremento a zero: x = 10; while (x > 0) x = x - 1",
        parse_cmd "x = 10; while x > 0 do x = x + (-1)",
        [ "x", E.while_dec_to_zero_x ] );

    make_prog_case
      ( "While step 2: x = 0; while (x < 10) x = x + 2",
        parse_cmd "x = 0; while x < 10 do x = x + 2",
        [ "x", E.while_step_two_x ] );

    make_prog_case
      ( "While relazionale tra due variabili: x = 0; y = 10; while (x < y) x = x + 1",
        parse_cmd "x = 0; y = 10; while x < y do x = x + 1",
        [ "x", E.while_relational_x; "y", E.while_relational_y ] );

    make_prog_case
      ( "While con invariante preservata: x = 0; y = 42; while (x < 5) x = x + 1",
        parse_cmd "x = 0; y = 42; while x < 5 do x = x + 1",
        [ "x", E.while_invariant_x; "y", E.while_invariant_y ] );

    make_prog_case
      ( "While convergenza rapida: x = 5; while (x != 0) x = 0",
        parse_cmd "x = 5; while x != 0 do x = 0",
        [ "x", E.while_fast_converge_x ] );

    expect_bottom "While con filtro falso prima del ciclo"
      (parse_cmd "x = 5; filter(false); while x < 10 do x = 1");
  ]

  let relational_inconsistency_tests = [
    expect_bottom "Transitivita negativa a 3 nodi: x-y<=2; y-z<=3; Filter(x-z >= 10)"
      (parse_cmd "x = Random(0, 100); y = Random(0, 100); z = Random(0, 100); x - y <= 2 ?; y - z <= 3 ?; x - z >= 10 ?");
    expect_bottom "Transitivita a 4 nodi: x-y<=1; y-z<=1; z-w<=1; Filter(x-w >= 5)"
      (parse_cmd "x = Random(0, 50); y = Random(0, 50); z = Random(0, 50); w = Random(0, 50); x - y <= 1 ?; y - z <= 1 ?; z - w <= 1 ?; x - w >= 5 ?");
    expect_bottom "Auto-contraddizione: Filter(x < x)"
      (parse_cmd "x = 5; x < x ?");
    expect_bottom "Auto-contraddizione NotEquals: Filter(x != x)"
      (parse_cmd "x = 5; x != x ?");
    expect_bottom "Contraddizione diretta tra costanti: x=5; y=10; Filter(x >= y)"
      (parse_cmd "x = 5; y = 10; x >= y ?");
    expect_bottom "Differenza incompatibile con costanti: x=10; y=2; Filter(x-y < 5)"
      (parse_cmd "x = 10; y = 2; x - y < 5 ?");
    expect_bottom "Random range superiore violato: x in [1,5]; Filter(x > 10)"
      (parse_cmd "x = Random(1, 5); x > 10 ?");
    expect_bottom "Random range inferiore violato: x in [1,5]; Filter(x < 0)"
      (parse_cmd "x = Random(1, 5); x < 0 ?");
    expect_bottom "Random intervalli disgiunti: x in [1,5]; y in [10,20]; Filter(x >= y)"
      (parse_cmd "x = Random(1, 5); y = Random(10, 20); x >= y ?");
    expect_bottom "Random intervalli disgiunti uguaglianza: x in [1,5]; y in [10,20]; Filter(x == y)"
      (parse_cmd "x = Random(1, 5); y = Random(10, 20); x == y ?");
    expect_bottom "Random differenza incompatibile: x in [0,5]; y in [20,30]; Filter(x-y > 0)"
      (parse_cmd "x = Random(0, 5); y = Random(20, 30); x - y > 0 ?");
    expect_bottom "Random differenza incompatibile inversa: x in [20,30]; y in [0,5]; Filter(x-y < 0)"
      (parse_cmd "x = Random(20, 30); y = Random(0, 5); x - y < 0 ?");
    expect_bottom "Shift var e contraddizione: x=5; y=x+3; Filter(y <= x)"
      (parse_cmd "x = 5; y = x + 3; y <= x ?");
    expect_bottom "Shift var e contraddizione inversa: x=5; y=x-4; Filter(x <= y)"
      (parse_cmd "x = 5; y = x - 4; x <= y ?");
    expect_bottom "Shift e bound incompatibile: x=10; y=x+2; Filter(y < 12)"
      (parse_cmd "x = 10; y = x + 2; y < 12 ?");
    expect_bottom "Shift e bound incompatibile negativo: x=10; y=x-2; Filter(y > 8)"
      (parse_cmd "x = 10; y = x - 2; y > 8 ?");
    expect_bottom "Doppio filtro incompatibile: x in [1,10]; Filter(x < 3); Filter(x > 7)"
      (parse_cmd "x = Random(1, 10); x < 3 ?; x > 7 ?");
    expect_bottom "Ciclo chiuso di differenze impossibile: x-y<=0, y-x<=-1"
      (parse_cmd "x = Random(0, 10); y = Random(0, 10); x - y <= 0 ?; y - x <= -1 ?");
    expect_bottom "If con entrambi i rami contraddittori"
      (parse_cmd "x = Random(1, 5); if x > 10 then y = 1 else filter(false)");
    expect_bottom "While con condizione impossibile e stato iniziale vuoto"
      (parse_cmd "x = 5; x == 0 ?; while x < 10 do x = 1");
  ]

  let inc_dec_tests = [
    make_prog_case
      ( "Zones/Octagons: Assign con Inc: x = 5; x = inc(x)",
        parse_cmd "x = 5; x = inc(x)",
        [ "x", E.assign_inc_x ] );
    make_prog_case
      ( "Zones/Octagons: Assign con Dec: x = 5; x = dec(x)",
        parse_cmd "x = 5; x = dec(x)",
        [ "x", E.assign_dec_x ] );
    make_prog_case
      ( "Zones/Octagons: Assign con Inc e Dec a nuove variabili",
        parse_cmd "x = 0; y = inc(x); z = dec(x)",
        [ "y", E.assign_inc_0_y; "z", E.assign_dec_0_z ] );
    make_prog_case
      ( "Zones/Octagons: Sequenza di 3 Inc",
        parse_cmd "x = 0; x = inc(x); x = inc(x); x = inc(x)",
        [ "x", E.assign_seq_3_inc ] );
    make_prog_case
      ( "Zones/Octagons: Sequenza di 3 Dec",
        parse_cmd "x = 0; x = dec(x); x = dec(x); x = dec(x)",
        [ "x", E.assign_seq_3_dec ] );
    make_prog_case
      ( "Zones/Octagons: Alternanza Inc e Dec",
        parse_cmd "x = 10; x = inc(x); x = dec(x)",
        [ "x", E.assign_inc_dec_cancel ] );
    make_prog_case
      ( "Zones/Octagons: Inc con Random",
        parse_cmd "x = Random(1, 5); y = inc(x)",
        [ "y", E.assign_inc_rand_y ] );
    make_prog_case
      ( "Zones/Octagons: Dec con Random",
        parse_cmd "x = Random(1, 5); y = dec(x)",
        [ "y", E.assign_dec_rand_y ] );
    make_prog_case
      ( "Zones/Octagons: Filtro valido su Inc",
        parse_cmd "x = 5; y = inc(x); y == 6 ?",
        [ "x", E.filter_inc_valid_x; "y", E.filter_inc_valid_y ] );
    make_prog_case
      ( "Zones/Octagons: Filtro valido su Dec",
        parse_cmd "x = 5; y = dec(x); y == 4 ?",
        [ "x", E.filter_dec_valid_x; "y", E.filter_dec_valid_y ] );
    expect_bottom
      "Zones/Octagons: Filtro contraddittorio su Inc"
      (parse_cmd "x = 10; y = inc(x); y <= 10 ?");
    expect_bottom
      "Zones/Octagons: Filtro contraddittorio su Dec"
      (parse_cmd "x = 10; y = dec(x); y >= 10 ?");
    expect_bottom
      "Zones/Octagons: While con Inc contraddittorio all'uscita"
      (parse_cmd "x = 0; while x < 5 do x = inc(x); x > 5 ?");
    expect_bottom
      "Zones/Octagons: While con Dec contraddittorio all'uscita"
      (parse_cmd "x = 5; while x > 0 do x = dec(x); x < 0 ?");
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
    "Inc/Dec Relazionali", inc_dec_tests;
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
        parse_cmd "x = 5; y = -x",
        [ "x", abstract_int 5; "y", abstract_int (-5) ]
      );
      make_case (
        "Assegnamento affine variabile negativa: x=5; y=-x+2",
        parse_cmd "x = 5; y = -x + 2",
        [ "x", abstract_int 5; "y", abstract_int (-3) ]
      );
      make_case (
        "Filtro somma concorde positiva: x=Random(1,10); y=Random(1,10); Filter(x+y <= 12)",
        parse_cmd "x = Random(1, 10); y = Random(1, 10); x + y <= 12 ?",
        [ "x", abstract_range 1 10; "y", abstract_range 1 10 ]
      );
      expect_bottom
        "Filtro somma concorde contraddittorio: x=10; y=10; Filter(x+y <= 15)"
        (parse_cmd "x = 10; y = 10; x + y <= 15 ?");
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-10; y=-10; Filter(-x-y <= 15)"
        (parse_cmd "x = -10; y = -10; -x - y <= 15 ?");

      (* --- 35 TEST AGGIUNTIVI SPECIFICI SUGLI OTTAGONI --- *)
      make_case (
        "Assegnamento negativo con zero: x=0; y=-x",
        parse_cmd "x = 0; y = -x",
        [ "x", abstract_int 0; "y", abstract_int 0 ]
      );
      make_case (
        "Assegnamento negativo da valore negativo: x=-7; y=-x",
        parse_cmd "x = -7; y = -x",
        [ "x", abstract_int (-7); "y", abstract_int 7 ]
      );
      make_case (
        "Assegnamento affine negativo sottrazione: x=4; y=-x-2",
        parse_cmd "x = 4; y = -x - 2",
        [ "x", abstract_int 4; "y", abstract_int (-6) ]
      );
      make_case (
        "Assegnamento affine negativo su negativo: x=-5; y=-x+10",
        parse_cmd "x = -5; y = -x + 10",
        [ "x", abstract_int (-5); "y", abstract_int 15 ]
      );
      make_case (
        "Assegnamento affine negativo con offset zero: x=8; y=-x+0",
        parse_cmd "x = 8; y = -x + 0",
        [ "x", abstract_int 8; "y", abstract_int (-8) ]
      );
      make_case (
        "Doppia negazione di variabile: x=6; y=-x; z=-y",
        parse_cmd "x = 6; y = -x; z = -y",
        [ "x", abstract_int 6; "y", abstract_int (-6); "z", abstract_int 6 ]
      );
      make_case (
        "Catena di negazioni: x=-3; y=-x; z=-y; w=-z",
        parse_cmd "x = -3; y = -x; z = -y; w = -z",
        [ "x", abstract_int (-3); "y", abstract_int 3; "z", abstract_int (-3); "w", abstract_int 3 ]
      );
      make_case (
        "Somma di opposti: x=5; y=-x; s=x+y",
        parse_cmd "x = 5; y = -x; s = x + y",
        [ "x", abstract_int 5; "y", abstract_int (-5); "s", abstract_int 0 ]
      );
      make_case (
        "Filtro somma concorde upper bound: x=4; y=6; Filter(x+y <= 10)",
        parse_cmd "x = 4; y = 6; x + y <= 10 ?",
        [ "x", abstract_int 4; "y", abstract_int 6 ]
      );
      make_case (
        "Filtro somma concorde lower bound: x=4; y=6; Filter(x+y >= 10)",
        parse_cmd "x = 4; y = 6; x + y >= 10 ?",
        [ "x", abstract_int 4; "y", abstract_int 6 ]
      );
      make_case (
        "Filtro somma concorde uguaglianza: x=3; y=7; Filter(x+y == 10)",
        parse_cmd "x = 3; y = 7; x + y == 10 ?",
        [ "x", abstract_int 3; "y", abstract_int 7 ]
      );
      make_case (
        "Filtro -x-y <= -10 valido: x=5; y=5; Filter(-x-y <= -10)",
        parse_cmd "x = 5; y = 5; -x - y <= -10 ?",
        [ "x", abstract_int 5; "y", abstract_int 5 ]
      );
      make_case (
        "Filtro -x+y <= 6 valido: x=2; y=8; Filter(-x+y <= 6)",
        parse_cmd "x = 2; y = 8; -x + y <= 6 ?",
        [ "x", abstract_int 2; "y", abstract_int 8 ]
      );
      make_case (
        "Filtro x-y <= 10 valido: x=8; y=2; Filter(x-y <= 10)",
        parse_cmd "x = 8; y = 2; x - y <= 10 ?",
        [ "x", abstract_int 8; "y", abstract_int 2 ]
      );
      make_case (
        "Filtro affine positivo: x=3; y=x+5; Filter(x-y <= -5)",
        parse_cmd "x = 3; y = x + 5; x - y <= -5 ?",
        [ "x", abstract_int 3; "y", abstract_int 8 ]
      );
      make_case (
        "If con filtro somma vero: x=3; y=4; if (x+y <= 10) z=1 else z=2",
        parse_cmd "x = 3; y = 4; if x + y <= 10 then z = 1 else z = 2",
        [ "x", abstract_int 3; "y", abstract_int 4; "z", abstract_int 1 ]
      );
      make_case (
        "If con filtro somma falso: x=3; y=4; if (x+y <= 5) z=1 else z=2",
        parse_cmd "x = 3; y = 4; if x + y <= 5 then z = 1 else z = 2",
        [ "x", abstract_int 3; "y", abstract_int 4; "z", abstract_int 2 ]
      );
      make_case (
        "If con filtro somma negativa vero: x=-3; y=-4; if (-x-y <= 10) z=1 else z=2",
        parse_cmd "x = -3; y = -4; if -x - y <= 10 then z = 1 else z = 2",
        [ "x", abstract_int (-3); "y", abstract_int (-4); "z", abstract_int 1 ]
      );
      make_case (
        "If con filtro somma negativa falso: x=-3; y=-4; if (-x-y <= 5) z=1 else z=2",
        parse_cmd "x = -3; y = -4; if -x - y <= 5 then z = 1 else z = 2",
        [ "x", abstract_int (-3); "y", abstract_int (-4); "z", abstract_int 2 ]
      );
      make_case (
        "While mai eseguito su somma: x=10; y=10; while (x+y < 15) x=0",
        parse_cmd "x = 10; y = 10; while x + y < 15 do x = 0",
        [ "x", abstract_int 10; "y", abstract_int 10 ]
      );
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y <= 9)"
        (parse_cmd "x = 5; y = 5; x + y <= 9 ?");
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y < 10)"
        (parse_cmd "x = 5; y = 5; x + y < 10 ?");
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y > 10)"
        (parse_cmd "x = 5; y = 5; x + y > 10 ?");
      expect_bottom
        "Filtro somma contraddittorio: x=5; y=5; Filter(x+y >= 11)"
        (parse_cmd "x = 5; y = 5; x + y >= 11 ?");
      expect_bottom
        "Filtro somma negativa contraddittorio: x=2; y=3; Filter(-x-y >= 0)"
        (parse_cmd "x = 2; y = 3; -x - y >= 0 ?");
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-5; y=-5; Filter(-x-y <= 9)"
        (parse_cmd "x = -5; y = -5; -x - y <= 9 ?");
      expect_bottom
        "Filtro somma negativa contraddittorio: x=-5; y=-5; Filter(-x-y < 10)"
        (parse_cmd "x = -5; y = -5; -x - y < 10 ?");
      expect_bottom
        "Filtro -x+y contraddittorio: x=10; y=2; Filter(-x+y >= 0)"
        (parse_cmd "x = 10; y = 2; -x + y >= 0 ?");
      expect_bottom
        "Filtro x-y contraddittorio: x=2; y=10; Filter(x-y >= 0)"
        (parse_cmd "x = 2; y = 10; x - y >= 0 ?");
      expect_bottom
        "Filtro Random somma contraddittoria: x in [5,10]; y in [5,10]; Filter(x+y <= 8)"
        (parse_cmd "x = Random(5, 10); y = Random(5, 10); x + y <= 8 ?");
      expect_bottom
        "Filtro Random somma contraddittoria: x in [1,2]; y in [1,2]; Filter(x+y >= 10)"
        (parse_cmd "x = Random(1, 2); y = Random(1, 2); x + y >= 10 ?");
      expect_bottom
        "Filtro Random somma negativa contraddittoria: x in [5,10]; y in [5,10]; Filter(-x-y >= 0)"
        (parse_cmd "x = Random(5, 10); y = Random(5, 10); -x - y >= 0 ?");
      expect_bottom
        "Filtro Random -x-y contraddittoria: x in [-2, -1]; y in [-2, -1]; Filter(-x-y <= 1)"
        (parse_cmd "x = Random(-2, -1); y = Random(-2, -1); -x - y <= 1 ?");
      expect_bottom
        "While con filtro somma contraddittorio all'ingresso"
        (parse_cmd "x = 10; y = 10; x + y < 0 ?; while true do skip");
      expect_bottom
        "Filtro somma concorde uguaglianza contraddittoria: x=3; y=4; Filter(x+y == 10)"
        (parse_cmd "x = 3; y = 4; x + y == 10 ?");
    ]
  ]
end

module AdvancedInterpreterTests = struct

  let expect_bottom is_bottom eval desc prog =
    ( desc,
      `Quick,
      fun () ->
        if not (is_bottom (eval prog)) then
          Alcotest.fail (Printf.sprintf "%s: atteso Bottom, ottenuto stato valido" desc) )

  let expect_not_bottom is_bottom eval desc prog =
    ( desc,
      `Quick,
      fun () ->
        if is_bottom (eval prog) then
          Alcotest.fail (Printf.sprintf "%s: atteso stato valido, ottenuto Bottom" desc) )

  (* 1. TEST RELAZIONALI CONDIVISI (Zone e Ottagoni) *)
  let make_shared_relational_tests is_bottom eval = [
    expect_bottom is_bottom eval
      "Lockstep While: x=0; y=0; while(x<10) {x++; y++}; Filter(x != y)"
      (parse_cmd "x = 0; y = 0; while x < 10 do { x = x + 1; y = y + 1 }; x != y ?");

    expect_bottom is_bottom eval
      "Swap di variabili: x=10; y=20; t=x; x=y; y=t; Filter(x <= y)"
      (parse_cmd "x = 10; y = 20; t = x; x = y; y = t; x <= y ?");

    expect_bottom is_bottom eval
      "Transitivita a 5 nodi: a-b<=-2; b-c<=-3; c-d<=-1; d-e<=-2; Filter(a-e >= -5)"
      (parse_cmd "a = Random(0, 100); b = Random(0, 100); c = Random(0, 100); d = Random(0, 100); e = Random(0, 100); a - b <= -2 ?; b - c <= -3 ?; c - d <= -1 ?; d - e <= -2 ?; a - e >= -5 ?");

    expect_bottom is_bottom eval
      "Assegnamenti affini concatenati: x=Random(0,50); y=x-4; z=y+2; Filter(x-z != 2)"
      (parse_cmd "x = Random(0, 50); y = x - 4; z = y + 2; x - z != 2 ?");

    expect_bottom is_bottom eval
      "If con branch merging e bound relazionale: y=x+2 o y=x+5; Filter(y-x < 2)"
      (parse_cmd "x = Random(0, 20); if Random(0, 1) == 0 then y = x + 2 else y = x + 5; y - x < 2 ?");
  ]

  (* 2. TEST SPECIFICI PER GLI OTTAGONI (relazioni con somme e variabili negate) *)
  let octagon_advanced_tests = [
    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Assegnamento affine negativo y=-x+10; Filter(x+y != 10)"
      (parse_cmd "x = Random(0, 20); y = -x + 10; x + y != 10 ?");

    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Chiusura forte mista x+y<=4; -y+z<=2; Filter(x+z >= 10)"
      (parse_cmd "x = nondet(0, 50); y = nondet(0, 50); z = nondet(0, 50); x + y < 4 ?; -y + z < 2 ?; x + z >= 10 ?");

    expect_bottom Abstract_domains.Octagons.is_bottom OctagonInterp.eval
      "Ottagoni: Doppia negazione affine y=-x; z=-y; Filter(x-z != 0)"
      (parse_cmd "x = nondet(1, 10); y = -x; z = -y; x - z != 0 ?");
  ]

  (* 3. TEST AVANZATI PER GLI INTERVALLI (Cicli, Narrowing e Aritmetica) *)
  let is_interval_bottom = function
    | IntervalInterp.BottomEnv -> true
    | IntervalInterp.Env _ -> false

  let intervals_advanced_tests = [
    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While narrowing con incremento a passo 3: x=0; while(x<10) x=x+3; Filter(x > 15)"
      (parse_cmd "x = 0; while x < 10 do x = x + 3; x > 15 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While narrowing decremento a 0: x=10; while(x>0) x=x-2; Filter(x > 0)"
      (parse_cmd "x = 10; while x > 0 do x = x - 2; x > 0 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: Prodotto di intervalli discordi: x in [2,5]; y in [-4,-2]; z=x*y; Filter(z >= 0)"
      (parse_cmd "x = Random(2, 5); y = Random(-4, -2); z = x * y; z >= 0 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: Divisione sicura: x in [20,40]; y in [2,4]; z=x/y; Filter(z < 4)"
      (parse_cmd "x = Random(20, 40); y = Random(2, 4); z = x / y; z < 4 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While incrementale con Inc: x=0; while(x<10) x=inc(x); Filter(x != 10)"
      (parse_cmd "x = 0; while x < 10 do x = inc(x); x != 10 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While decrementale con Dec: x=10; while(x>0) x=dec(x); Filter(x != 0)"
      (parse_cmd "x = 10; while x > 0 do x = dec(x); x != 0 ?");

    expect_bottom is_interval_bottom IntervalInterp.eval
      "Intervals: While con doppio Inc: x=0; while(x<10) x=inc(inc(x)); Filter(x > 11)"
      (parse_cmd "x = 0; while x < 10 do x = inc(inc(x)); x > 11 ?");
  ]

  (* 4. TEST AVANZATI PER I DOMINI DEI SEGNI *)
  let make_sign_advanced_tests ?(has_neg_zero = true) is_bottom eval = [
    expect_bottom is_bottom eval
      "Signs: Negazione di positivo: x in [1,10]; y=-x; Filter(y > 0)"
      (parse_cmd "x = Random(1, 10); y = -x; y > 0 ?");

    expect_bottom is_bottom eval
      "Signs: Prodotto tra opposti: x in [1,10]; y in [-10,-1]; z=x*y; Filter(z > 0)"
      (parse_cmd "x = Random(1, 10); y = Random(-10, -1); z = x * y; z > 0 ?");

    expect_bottom is_bottom eval
      "Signs: If con entrambi rami positivi: if (b) x=5 else x=10; Filter(x < 0)"
      (parse_cmd "if true then x = 5 else x = 10; x < 0 ?");

    expect_bottom is_bottom eval
      "Signs: While mai eseguito: x=-5; while(x>0) x=x+1; Filter(x > 0)"
      (parse_cmd "x = -5; while x > 0 do x = x + 1; x > 0 ?");
  ] @ (if has_neg_zero then [
    expect_bottom is_bottom eval
      "Signs: Inc con valore negativo: x=-5; x=inc(x); Filter(x > 0)"
      (parse_cmd "x = -5; x = inc(x); x > 0 ?");
  ] else [
    expect_not_bottom is_bottom eval
      "Signs: Inc con valore negativo (SimplifiedSigns sovra-approssima inc(Neg) a SignTop)"
      (parse_cmd "x = -5; x = inc(x); x > 0 ?");
  ])
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
        AdvancedInterpreterTests.make_sign_advanced_tests ~has_neg_zero:true (function ExtendedSignInterp.BottomEnv -> true | _ -> false) ExtendedSignInterp.eval;
      "SimplifiedSigns: Sfide Avanzate Segni",
        AdvancedInterpreterTests.make_sign_advanced_tests ~has_neg_zero:false (function SimplifiedSignInterp.BottomEnv -> true | _ -> false) SimplifiedSignInterp.eval;
    ] @
    [ ("Parser BNF", Parser_tests.tests) ]
  )

