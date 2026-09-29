open Abstract_domains
open Interpeters
open Syntax

(* Helper testabili per Alcotest *)
let interval_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (Intervals.to_string v))
    (fun a b -> a = b)

let extended_sign_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (ExtendedSigns.to_string v))
    (fun a b -> a = b)

let simplified_sign_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (SimplifiedSigns.to_string v))
    (fun a b -> a = b)

let sign_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (Signs.to_string v))
    (fun a b -> a = b)

let simple_sign_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (SimpleSigns.to_string v))
    (fun a b -> a = b)

let zone_value_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (Zones.string_of_value v))
    (fun a b -> a = b)

let octagon_value_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (Octagons.string_of_value v))
    (fun a b -> a = b)

(* Helper per controllare il valore di variabili in uno stato *)
let check_interval_var desc final_env var expected =
  match final_env with
  | IntervalInterp.BottomEnv ->
      Alcotest.fail (Printf.sprintf "%s: atteso Env ma ottenuto BottomEnv" desc)
  | IntervalInterp.Env tbl ->
      match Hashtbl.find_opt tbl var with
      | Some v -> Alcotest.(check interval_testable) (desc ^ " - " ^ var) expected v
      | None -> Alcotest.fail (Printf.sprintf "%s: variabile '%s' non trovata" desc var)

let check_interval_vars desc final_env expected_list =
  List.iter (fun (var, exp) -> check_interval_var desc final_env var exp) expected_list

let expect_interval_bottom desc prog =
  ( desc,
    `Quick,
    fun () ->
      match IntervalInterp.eval prog with
      | IntervalInterp.BottomEnv -> ()
      | IntervalInterp.Env _ ->
          Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto stato valido" desc) )

let expect_relational_bottom is_bottom eval desc prog =
  ( desc,
    `Quick,
    fun () ->
      if not (is_bottom (eval prog)) then
        Alcotest.fail (Printf.sprintf "%s: atteso Bottom, ottenuto stato valido" desc) )

let expect_sign_bottom eval desc prog =
  ( desc,
    `Quick,
    fun () ->
      match eval prog with
      | ExtendedSignInterp.BottomEnv -> ()
      | ExtendedSignInterp.Env _ ->
          Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto stato valido" desc) )

let expect_simplified_sign_bottom eval desc prog =
  ( desc,
    `Quick,
    fun () ->
      match eval prog with
      | SimplifiedSignInterp.BottomEnv -> ()
      | SimplifiedSignInterp.Env _ ->
          Alcotest.fail (Printf.sprintf "%s: atteso BottomEnv, ottenuto stato valido" desc) )

(* -------------------------------------------------------------------------- *)
(* 1. Test Valutazione Espressioni: Intervalli                                *)
(* -------------------------------------------------------------------------- *)
let make_interval_env () =
  let tbl = Hashtbl.create 10 in
  Hashtbl.add tbl "x" (Intervals.abstract_range 0 10);
  Hashtbl.add tbl "y" (Intervals.abstract_range (-10) (-1));
  Hashtbl.add tbl "z" (Intervals.abstract_int 0);
  tbl

let eval_exp_interval_tests = [
  ( "Inc di costante 0 -> [1, 1]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Const 0)) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(0)" (Intervals.abstract_int 1) res );

  ( "Dec di costante 0 -> [-1, -1]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Const 0)) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(0)" (Intervals.abstract_int (-1)) res );

  ( "Inc di costante positiva 5 -> [6, 6]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Const 5)) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(5)" (Intervals.abstract_int 6) res );

  ( "Dec di costante positiva 5 -> [4, 4]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Const 5)) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(5)" (Intervals.abstract_int 4) res );

  ( "Inc di costante negativa -5 -> [-4, -4]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Const (-5))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(-5)" (Intervals.abstract_int (-4)) res );

  ( "Dec di costante negativa -5 -> [-6, -6]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Const (-5))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(-5)" (Intervals.abstract_int (-6)) res );

  ( "Inc di intervallo non deterministico Random(2, 8) -> [3, 9]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Random (2, 8))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(Random(2,8))" (Intervals.abstract_range 3 9) res );

  ( "Dec di intervallo non deterministico Random(2, 8) -> [1, 7]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Random (2, 8))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(Random(2,8))" (Intervals.abstract_range 1 7) res );

  ( "Doppio Inc annidato: Inc(Inc(Const 0)) -> [2, 2]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Inc (Const 0))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(Inc(0))" (Intervals.abstract_int 2) res );

  ( "Doppio Dec annidato: Dec(Dec(Const 0)) -> [-2, -2]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Dec (Const 0))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(Dec(0))" (Intervals.abstract_int (-2)) res );

  ( "Annullamento: Inc(Dec(Const 42)) -> [42, 42]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Inc (Dec (Const 42))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Inc(Dec(42))" (Intervals.abstract_int 42) res );

  ( "Annullamento: Dec(Inc(Const 42)) -> [42, 42]",
    `Quick,
    fun () ->
      let res = IntervalInterp.eval_exp (Dec (Inc (Const 42))) (IntervalInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check interval_testable) "Dec(Inc(42))" (Intervals.abstract_int 42) res );

  ( "Inc di variabile x in [0, 10] -> [1, 11]",
    `Quick,
    fun () ->
      let env = IntervalInterp.Env (make_interval_env ()) in
      let res = IntervalInterp.eval_exp (Inc (Var "x")) env in
      Alcotest.(check interval_testable) "Inc(x)" (Intervals.abstract_range 1 11) res );

  ( "Dec di variabile x in [0, 10] -> [-1, 9]",
    `Quick,
    fun () ->
      let env = IntervalInterp.Env (make_interval_env ()) in
      let res = IntervalInterp.eval_exp (Dec (Var "x")) env in
      Alcotest.(check interval_testable) "Dec(x)" (Intervals.abstract_range (-1) 9) res );

  ( "Inc di espressione composta Inc(x + 5) con x in [0, 10] -> [6, 16]",
    `Quick,
    fun () ->
      let env = IntervalInterp.Env (make_interval_env ()) in
      let res = IntervalInterp.eval_exp (Inc (BinaryOperation (Var "x", Add, Const 5))) env in
      Alcotest.(check interval_testable) "Inc(x+5)" (Intervals.abstract_range 6 16) res );

  ( "Dec di espressione composta Dec(x - 5) con x in [0, 10] -> [-6, 4]",
    `Quick,
    fun () ->
      let env = IntervalInterp.Env (make_interval_env ()) in
      let res = IntervalInterp.eval_exp (Dec (BinaryOperation (Var "x", Sub, Const 5))) env in
      Alcotest.(check interval_testable) "Dec(x-5)" (Intervals.abstract_range (-6) 4) res );
]

(* -------------------------------------------------------------------------- *)
(* 2. Test Valutazione Espressioni: Domini dei Segni                          *)
(* -------------------------------------------------------------------------- *)
let eval_exp_sign_tests = [
  ( "ExtendedSigns: Inc Zero -> Pos",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Inc (Const 0)) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Inc(Zero)" ExtendedSigns.Pos res );

  ( "ExtendedSigns: Dec Zero -> Neg",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Dec (Const 0)) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Dec(Zero)" ExtendedSigns.Neg res );

  ( "ExtendedSigns: Inc PosZero -> Pos",
    `Quick,
    fun () ->
      let env = Hashtbl.create 1 in
      Hashtbl.add env "w" ExtendedSigns.PosZero;
      let res = ExtendedSignInterp.eval_exp (Inc (Var "w")) (ExtendedSignInterp.Env env) in
      Alcotest.(check extended_sign_testable) "Inc(PosZero)" ExtendedSigns.Pos res );

  ( "ExtendedSigns: Dec NegZero -> Neg",
    `Quick,
    fun () ->
      let env = Hashtbl.create 1 in
      Hashtbl.add env "k" ExtendedSigns.NegZero;
      let res = ExtendedSignInterp.eval_exp (Dec (Var "k")) (ExtendedSignInterp.Env env) in
      Alcotest.(check extended_sign_testable) "Dec(NegZero)" ExtendedSigns.Neg res );

  ( "ExtendedSigns: Inc Neg -> NegZero",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Inc (Const (-5))) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Inc(Neg)" ExtendedSigns.NegZero res );

  ( "ExtendedSigns: Dec Pos -> PosZero",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Dec (Const 5)) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Dec(Pos)" ExtendedSigns.PosZero res );

  ( "ExtendedSigns: Inc Pos -> Pos",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Inc (Const 5)) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Inc(Pos)" ExtendedSigns.Pos res );

  ( "ExtendedSigns: Dec Neg -> Neg",
    `Quick,
    fun () ->
      let res = ExtendedSignInterp.eval_exp (Dec (Const (-5))) (ExtendedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check extended_sign_testable) "Dec(Neg)" ExtendedSigns.Neg res );

  ( "SimplifiedSigns: Inc Zero -> Pos",
    `Quick,
    fun () ->
      let res = SimplifiedSignInterp.eval_exp (Inc (Const 0)) (SimplifiedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simplified_sign_testable) "Inc(Zero)" SimplifiedSigns.Pos res );

  ( "SimplifiedSigns: Dec Zero -> Neg",
    `Quick,
    fun () ->
      let res = SimplifiedSignInterp.eval_exp (Dec (Const 0)) (SimplifiedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simplified_sign_testable) "Dec(Zero)" SimplifiedSigns.Neg res );

  ( "SimplifiedSigns: Inc Neg -> SignTop",
    `Quick,
    fun () ->
      let res = SimplifiedSignInterp.eval_exp (Inc (Const (-3))) (SimplifiedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simplified_sign_testable) "Inc(Neg)" SimplifiedSigns.SignTop res );

  ( "SimplifiedSigns: Dec Pos -> SignTop",
    `Quick,
    fun () ->
      let res = SimplifiedSignInterp.eval_exp (Dec (Const 3)) (SimplifiedSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simplified_sign_testable) "Dec(Pos)" SimplifiedSigns.SignTop res );

  ( "Signs: Inc Pos -> Pos",
    `Quick,
    fun () ->
      let res = SignInterp.eval_exp (Inc (Const 5)) (SignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check sign_testable) "Inc(Pos)" Signs.Pos res );

  ( "Signs: Dec Neg -> Neg",
    `Quick,
    fun () ->
      let res = SignInterp.eval_exp (Dec (Const (-5))) (SignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check sign_testable) "Dec(Neg)" Signs.Neg res );

  ( "Signs: Inc Neg -> SignTop",
    `Quick,
    fun () ->
      let res = SignInterp.eval_exp (Inc (Const (-5))) (SignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check sign_testable) "Inc(Neg)" Signs.SignTop res );

  ( "Signs: Dec Pos -> SignTop",
    `Quick,
    fun () ->
      let res = SignInterp.eval_exp (Dec (Const 5)) (SignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check sign_testable) "Dec(Pos)" Signs.SignTop res );

  ( "SimpleSigns: Inc Zero -> PosZero",
    `Quick,
    fun () ->
      let res = SimpleSignInterp.eval_exp (Inc (Const 0)) (SimpleSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simple_sign_testable) "Inc(Zero)" SimpleSigns.PosZero res );

  ( "SimpleSigns: Dec Zero -> NegZero",
    `Quick,
    fun () ->
      let res = SimpleSignInterp.eval_exp (Dec (Const 0)) (SimpleSignInterp.Env (Hashtbl.create 1)) in
      Alcotest.(check simple_sign_testable) "Dec(Zero)" SimpleSigns.NegZero res );
]

(* -------------------------------------------------------------------------- *)
(* 3. Test Valutazione Espressioni: Domini Relazionali (Zone e Ottagoni)     *)
(* -------------------------------------------------------------------------- *)
let eval_exp_relational_tests = [
  ( "Zones: Inc Const 3 -> [4, 4]",
    `Quick,
    fun () ->
      let env = Zones.init ["x"] in
      let res = ZoneInterp.eval_exp (Inc (Const 3)) env in
      Alcotest.(check zone_value_testable) "Inc(3)" (Zones.abstract_int 4) res );

  ( "Zones: Dec Const 3 -> [2, 2]",
    `Quick,
    fun () ->
      let env = Zones.init ["x"] in
      let res = ZoneInterp.eval_exp (Dec (Const 3)) env in
      Alcotest.(check zone_value_testable) "Dec(3)" (Zones.abstract_int 2) res );

  ( "Zones: Inc Var x in [1, 5] -> [2, 6]",
    `Quick,
    fun () ->
      let env = Zones.assign "x" (Zones.abstract_range 1 5) (Zones.init ["x"]) in
      let res = ZoneInterp.eval_exp (Inc (Var "x")) env in
      Alcotest.(check zone_value_testable) "Inc(x)" (Zones.abstract_range 2 6) res );

  ( "Zones: Dec Var x in [1, 5] -> [0, 4]",
    `Quick,
    fun () ->
      let env = Zones.assign "x" (Zones.abstract_range 1 5) (Zones.init ["x"]) in
      let res = ZoneInterp.eval_exp (Dec (Var "x")) env in
      Alcotest.(check zone_value_testable) "Dec(x)" (Zones.abstract_range 0 4) res );

  ( "Octagons: Inc Const 3 -> [4, 4]",
    `Quick,
    fun () ->
      let env = Octagons.init ["x"] in
      let res = OctagonInterp.eval_exp (Inc (Const 3)) env in
      Alcotest.(check octagon_value_testable) "Inc(3)" (Octagons.abstract_int 4) res );

  ( "Octagons: Dec Const 3 -> [2, 2]",
    `Quick,
    fun () ->
      let env = Octagons.init ["x"] in
      let res = OctagonInterp.eval_exp (Dec (Const 3)) env in
      Alcotest.(check octagon_value_testable) "Dec(3)" (Octagons.abstract_int 2) res );

  ( "Octagons: Inc Var x in [1, 5] -> [2, 6]",
    `Quick,
    fun () ->
      let env = Octagons.assign "x" (Octagons.abstract_range 1 5) (Octagons.init ["x"]) in
      let res = OctagonInterp.eval_exp (Inc (Var "x")) env in
      Alcotest.(check octagon_value_testable) "Inc(x)" (Octagons.abstract_range 2 6) res );

  ( "Octagons: Dec Var x in [1, 5] -> [0, 4]",
    `Quick,
    fun () ->
      let env = Octagons.assign "x" (Octagons.abstract_range 1 5) (Octagons.init ["x"]) in
      let res = OctagonInterp.eval_exp (Dec (Var "x")) env in
      Alcotest.(check octagon_value_testable) "Dec(x)" (Octagons.abstract_range 0 4) res );
]

(* -------------------------------------------------------------------------- *)
(* 4. Test Programmi: Assegnamenti (Intervalli)                               *)
(* -------------------------------------------------------------------------- *)
let assignment_tests = [
  ( "Assign con Inc: x = 5; x = Inc(x) -> x = [6, 6]",
    `Quick,
    fun () ->
      let prog = Sequence (Assign ("x", Const 5), Assign ("x", Inc (Var "x"))) in
      check_interval_var "x=5; x=Inc(x)" (IntervalInterp.eval prog) "x" (Intervals.abstract_int 6) );

  ( "Assign con Dec: x = 5; x = Dec(x) -> x = [4, 4]",
    `Quick,
    fun () ->
      let prog = Sequence (Assign ("x", Const 5), Assign ("x", Dec (Var "x"))) in
      check_interval_var "x=5; x=Dec(x)" (IntervalInterp.eval prog) "x" (Intervals.abstract_int 4) );

  ( "Assign a nuova variabile: x = 0; y = Inc(x); z = Dec(x)",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 0),
          Sequence (
            Assign ("y", Inc (Var "x")),
            Assign ("z", Dec (Var "x"))
          )
        )
      in
      check_interval_vars "x=0; y=Inc(x); z=Dec(x)" (IntervalInterp.eval prog) [
        ("x", Intervals.abstract_int 0);
        ("y", Intervals.abstract_int 1);
        ("z", Intervals.abstract_int (-1));
      ] );

  ( "Sequenza di 3 Inc: x = 0; x = Inc(x); x = Inc(x); x = Inc(x) -> x = [3, 3]",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 0),
          Sequence (
            Assign ("x", Inc (Var "x")),
            Sequence (
              Assign ("x", Inc (Var "x")),
              Assign ("x", Inc (Var "x"))
            )
          )
        )
      in
      check_interval_var "3 Inc consecutivi" (IntervalInterp.eval prog) "x" (Intervals.abstract_int 3) );

  ( "Sequenza di 3 Dec: x = 0; x = Dec(x); x = Dec(x); x = Dec(x) -> x = [-3, -3]",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 0),
          Sequence (
            Assign ("x", Dec (Var "x")),
            Sequence (
              Assign ("x", Dec (Var "x")),
              Assign ("x", Dec (Var "x"))
            )
          )
        )
      in
      check_interval_var "3 Dec consecutivi" (IntervalInterp.eval prog) "x" (Intervals.abstract_int (-3)) );

  ( "Alternanza Inc e Dec: x = 10; x = Inc(x); x = Dec(x) -> x = [10, 10]",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 10),
          Sequence (
            Assign ("x", Inc (Var "x")),
            Assign ("x", Dec (Var "x"))
          )
        )
      in
      check_interval_var "Inc e poi Dec" (IntervalInterp.eval prog) "x" (Intervals.abstract_int 10) );

  ( "Inc con Random: x = Random(1, 5); y = Inc(x) -> y = [2, 6]",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Random (1, 5)),
          Assign ("y", Inc (Var "x"))
        )
      in
      check_interval_var "y = Inc(Random(1,5))" (IntervalInterp.eval prog) "y" (Intervals.abstract_range 2 6) );

  ( "Dec con Random: x = Random(1, 5); y = Dec(x) -> y = [0, 4]",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Random (1, 5)),
          Assign ("y", Dec (Var "x"))
        )
      in
      check_interval_var "y = Dec(Random(1,5))" (IntervalInterp.eval prog) "y" (Intervals.abstract_range 0 4) );
]

(* -------------------------------------------------------------------------- *)
(* 5. Test Programmi: Filtri e Contraddizioni                                *)
(* -------------------------------------------------------------------------- *)
let filter_contradiction_tests = [
  expect_interval_bottom
    "Filtro contraddittorio su Inc: x=5; y=Inc(x); Filter(y <= 5)"
    (Sequence (
      Assign ("x", Const 5),
      Sequence (
        Assign ("y", Inc (Var "x")),
        Filter (Comparison (Var "y", SmallerEquals, Const 5))
      )
    ));

  expect_interval_bottom
    "Filtro contraddittorio su Dec: x=5; y=Dec(x); Filter(y >= 5)"
    (Sequence (
      Assign ("x", Const 5),
      Sequence (
        Assign ("y", Dec (Var "x")),
        Filter (Comparison (Var "y", BiggerEquals, Const 5))
      )
    ));

  expect_interval_bottom
    "Filtro contraddittorio con Random e Inc: x in [1,5]; y=Inc(x); Filter(y < 2)"
    (Sequence (
      Assign ("x", Random (1, 5)),
      Sequence (
        Assign ("y", Inc (Var "x")),
        Filter (Comparison (Var "y", Smaller, Const 2))
      )
    ));

  expect_interval_bottom
    "Filtro contraddittorio con Random e Dec: x in [1,5]; y=Dec(x); Filter(y > 4)"
    (Sequence (
      Assign ("x", Random (1, 5)),
      Sequence (
        Assign ("y", Dec (Var "x")),
        Filter (Comparison (Var "y", Bigger, Const 4))
      )
    ));

  expect_interval_bottom
    "Filtro diretto su Inc in espressione guardia: x=10; Filter(Inc(x) <= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Filter (Comparison (Inc (Var "x"), SmallerEquals, Const 10))
    ));

  expect_interval_bottom
    "Filtro diretto su Dec in espressione guardia: x=10; Filter(Dec(x) >= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Filter (Comparison (Dec (Var "x"), BiggerEquals, Const 10))
    ));

  expect_interval_bottom
    "Catena di 3 Inc e filtro != 3: x=0; x=Inc(Inc(Inc(x))); Filter(x != 3)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        Assign ("x", Inc (Inc (Inc (Var "x")))),
        Filter (Comparison (Var "x", NotEquals, Const 3))
      )
    ));

  expect_interval_bottom
    "Catena di 3 Dec e filtro != 0: x=3; x=Dec(Dec(Dec(x))); Filter(x != 0)"
    (Sequence (
      Assign ("x", Const 3),
      Sequence (
        Assign ("x", Dec (Dec (Dec (Var "x")))),
        Filter (Comparison (Var "x", NotEquals, Const 0))
      )
    ));

  ( "Filtro valido su Inc preserva lo stato: x=5; y=Inc(x); Filter(y == 6)",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 5),
          Sequence (
            Assign ("y", Inc (Var "x")),
            Filter (Comparison (Var "y", Equals, Const 6))
          )
        )
      in
      check_interval_var "Filtro valido y==6" (IntervalInterp.eval prog) "y" (Intervals.abstract_int 6) );

  ( "Filtro valido su Dec preserva lo stato: x=5; y=Dec(x); Filter(y == 4)",
    `Quick,
    fun () ->
      let prog =
        Sequence (
          Assign ("x", Const 5),
          Sequence (
            Assign ("y", Dec (Var "x")),
            Filter (Comparison (Var "y", Equals, Const 4))
          )
        )
      in
      check_interval_var "Filtro valido y==4" (IntervalInterp.eval prog) "y" (Intervals.abstract_int 4) );
]

(* -------------------------------------------------------------------------- *)
(* 6. Test Programmi: Condizionali If                                        *)
(* -------------------------------------------------------------------------- *)
let if_tests = [
  expect_interval_bottom
    "If rami Inc e Dec con bound esterno superiore: if(b) x=Inc(x) else x=Dec(x); Filter(x > 1)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        If (Boolean true, Assign ("x", Inc (Var "x")), Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", Bigger, Const 1))
      )
    ));

  expect_interval_bottom
    "If rami Inc e Dec con bound esterno inferiore: if(b) x=Inc(x) else x=Dec(x); Filter(x < -1)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        If (Boolean true, Assign ("x", Inc (Var "x")), Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", Smaller, Const (-1))
      )
    )));

  expect_interval_bottom
    "If con guardia Inc: x=0; if(Inc(x) > 0) y=1 else y=2; Filter(y == 2)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        If (Comparison (Inc (Var "x"), Bigger, Const 0),
            Assign ("y", Const 1),
            Assign ("y", Const 2)),
        Filter (Comparison (Var "y", Equals, Const 2))
      )
    ));

  expect_interval_bottom
    "If con guardia Dec: x=0; if(Dec(x) < 0) y=1 else y=2; Filter(y == 2)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        If (Comparison (Dec (Var "x"), Smaller, Const 0),
            Assign ("y", Const 1),
            Assign ("y", Const 2)),
        Filter (Comparison (Var "y", Equals, Const 2))
      )
    ));

  expect_interval_bottom
    "If su valore positivo con Inc: x=5; if(x > 0) x=Inc(x) else x=Dec(x); Filter(x != 6)"
    (Sequence (
      Assign ("x", Const 5),
      Sequence (
        If (Comparison (Var "x", Bigger, Const 0),
            Assign ("x", Inc (Var "x")),
            Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", NotEquals, Const 6))
      )
    ));
]

(* -------------------------------------------------------------------------- *)
(* 7. Test Programmi: Cicli While                                             *)
(* -------------------------------------------------------------------------- *)
let while_tests = [
  expect_interval_bottom
    "While incrementale: x=0; while(x < 10) x=Inc(x); Filter(x != 10)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        While (Comparison (Var "x", Smaller, Const 10),
               Assign ("x", Inc (Var "x"))),
        Filter (Comparison (Var "x", NotEquals, Const 10))
      )
    ));

  expect_interval_bottom
    "While decrementale countdown: x=10; while(x > 0) x=Dec(x); Filter(x != 0)"
    (Sequence (
      Assign ("x", Const 10),
      Sequence (
        While (Comparison (Var "x", Bigger, Const 0),
               Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", NotEquals, Const 0))
      )
    ));

  expect_interval_bottom
    "While con doppio Inc nel corpo: x=0; while(x < 10) x=Inc(Inc(x)); Filter(x > 11)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        While (Comparison (Var "x", Smaller, Const 10),
               Assign ("x", Inc (Inc (Var "x")))),
        Filter (Comparison (Var "x", Bigger, Const 11))
      )
    ));

  expect_interval_bottom
    "While decrementale mai eseguito: x=-5; while(x > 0) x=Dec(x); Filter(x > 0)"
    (Sequence (
      Assign ("x", Const (-5)),
      Sequence (
        While (Comparison (Var "x", Bigger, Const 0),
               Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", Bigger, Const 0))
      )
    ));
]

(* -------------------------------------------------------------------------- *)
(* 8. Test Programmi: Domini Relazionali (Zone e Ottagoni)                   *)
(* -------------------------------------------------------------------------- *)
let relational_program_tests = [
  expect_relational_bottom Zones.is_bottom ZoneInterp.eval
    "Zones: x=10; y=Inc(x); Filter(y <= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Sequence (
        Assign ("y", Inc (Var "x")),
        Filter (Comparison (Var "y", SmallerEquals, Const 10))
      )
    ));

  expect_relational_bottom Zones.is_bottom ZoneInterp.eval
    "Zones: x=10; y=Dec(x); Filter(y >= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Sequence (
        Assign ("y", Dec (Var "x")),
        Filter (Comparison (Var "y", BiggerEquals, Const 10))
      )
    ));

  expect_relational_bottom Zones.is_bottom ZoneInterp.eval
    "Zones: While con Inc: x=0; while(x < 5) x=Inc(x); Filter(x > 5)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        While (Comparison (Var "x", Smaller, Const 5),
               Assign ("x", Inc (Var "x"))),
        Filter (Comparison (Var "x", Bigger, Const 5))
      )
    ));

  expect_relational_bottom Octagons.is_bottom OctagonInterp.eval
    "Octagons: x=10; y=Inc(x); Filter(y <= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Sequence (
        Assign ("y", Inc (Var "x")),
        Filter (Comparison (Var "y", SmallerEquals, Const 10))
      )
    ));

  expect_relational_bottom Octagons.is_bottom OctagonInterp.eval
    "Octagons: x=10; y=Dec(x); Filter(y >= 10)"
    (Sequence (
      Assign ("x", Const 10),
      Sequence (
        Assign ("y", Dec (Var "x")),
        Filter (Comparison (Var "y", BiggerEquals, Const 10))
      )
    ));

  expect_relational_bottom Octagons.is_bottom OctagonInterp.eval
    "Octagons: While con Dec: x=5; while(x > 0) x=Dec(x); Filter(x < 0)"
    (Sequence (
      Assign ("x", Const 5),
      Sequence (
        While (Comparison (Var "x", Bigger, Const 0),
               Assign ("x", Dec (Var "x"))),
        Filter (Comparison (Var "x", Smaller, Const 0))
      )
    ));
]

(* -------------------------------------------------------------------------- *)
(* 9. Test Programmi: Domini dei Segni                                       *)
(* -------------------------------------------------------------------------- *)
let sign_program_tests = [
  expect_sign_bottom ExtendedSignInterp.eval
    "ExtendedSigns: x=0; x=Inc(x); Filter(x <= 0)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        Assign ("x", Inc (Var "x")),
        Filter (Comparison (Var "x", SmallerEquals, Const 0))
      )
    ));

  expect_sign_bottom ExtendedSignInterp.eval
    "ExtendedSigns: x=0; x=Dec(x); Filter(x >= 0)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        Assign ("x", Dec (Var "x")),
        Filter (Comparison (Var "x", BiggerEquals, Const 0))
      )
    ));

  expect_sign_bottom ExtendedSignInterp.eval
    "ExtendedSigns: x=-5; x=Inc(x); Filter(x > 0) -> Bottom (Inc Neg = NegZero)"
    (Sequence (
      Assign ("x", Const (-5)),
      Sequence (
        Assign ("x", Inc (Var "x")),
        Filter (Comparison (Var "x", Bigger, Const 0))
      )
    ));

  expect_simplified_sign_bottom SimplifiedSignInterp.eval
    "SimplifiedSigns: x=0; x=Inc(x); Filter(x <= 0)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        Assign ("x", Inc (Var "x")),
        Filter (Comparison (Var "x", SmallerEquals, Const 0))
      )
    ));

  expect_simplified_sign_bottom SimplifiedSignInterp.eval
    "SimplifiedSigns: x=0; x=Dec(x); Filter(x >= 0)"
    (Sequence (
      Assign ("x", Const 0),
      Sequence (
        Assign ("x", Dec (Var "x")),
        Filter (Comparison (Var "x", BiggerEquals, Const 0))
      )
    ));
]

(* Lista completa esportata per l'esecuzione in Alcotest *)
let tests = [
  ("Inc/Dec: Valutazione Espressioni Intervalli", eval_exp_interval_tests);
  ("Inc/Dec: Valutazione Espressioni Segni", eval_exp_sign_tests);
  ("Inc/Dec: Valutazione Espressioni Domini Relazionali", eval_exp_relational_tests);
  ("Inc/Dec: Assegnamenti e Comandi (Intervalli)", assignment_tests);
  ("Inc/Dec: Filtri e Contraddizioni", filter_contradiction_tests);
  ("Inc/Dec: Condizionali If", if_tests);
  ("Inc/Dec: Cicli While e Fixpoint", while_tests);
  ("Inc/Dec: Domini Relazionali (Zone e Ottagoni)", relational_program_tests);
  ("Inc/Dec: Domini dei Segni (Programmi)", sign_program_tests);
]
