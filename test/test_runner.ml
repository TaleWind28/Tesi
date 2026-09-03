(* let () =
  Alcotest.run "Abstract Interpretation Tests" ([
    (* Espande i sottogruppi di Test_alcosigns *)
    "Signs - Somma", List.assoc "Somma" Test_alcosigns.tests;
    "Signs - Sottrazione", List.assoc "Sottrazione" Test_alcosigns.tests;
    "Signs - Moltiplicazione", List.assoc "Moltiplicazione" Test_alcosigns.tests;
    "Signs - Divisione", List.assoc "Divisione" Test_alcosigns.tests;
    "Signs - Negazione", List.assoc "Negazione" Test_alcosigns.tests;
    "Signs - Random", List.assoc "Random" Test_alcosigns.tests;
    (* Test sui comandi (eval_cmd / eval) *)
    "Signs - Assegnazioni", List.assoc "Assegnazioni" Test_alcosigns.tests;
    "Signs - Sequenze", List.assoc "Sequenze" Test_alcosigns.tests;
    "Signs - Overwrite", List.assoc "Overwrite" Test_alcosigns.tests;
    "Signs - Skip", List.assoc "Skip" Test_alcosigns.tests;
    "Signs - Stato precompilato", List.assoc "Stato precompilato" Test_alcosigns.tests;
    "Signs - Test Prog", List.assoc "Test Prog" Test_alcosigns.tests;

    "Sign - Filter - Certain", List.assoc "Filter - casi certi" Test_alcosigns.tests;
    "Sign - Filter - Uncertain", List.assoc "Filter - casi ambigui" Test_alcosigns.tests;
    "Sign - Filter - Simmetric", List.assoc "Filter - simmetria" Test_alcosigns.tests;
    "Sign - Filter - Derived", List.assoc "Filter - operatori derivati" Test_alcosigns.tests;
    "Sign - Filter - Composition", List.assoc "Filter - composizione And/Or/Not" Test_alcosigns.tests;
    "Sign - Filter - Bottom", List.assoc "Filter - valore Bottom" Test_alcosigns.tests;
    "Sign - Filter - Chaining", List.assoc "Filter - incatenato" Test_alcosigns.tests;
    (*Test Su IF*)
    "Sign - IF - Ramo Then", List.assoc "IF - Ramo Then" Test_alcosigns.tests;
    "Sign - IF - Ramo Else", List.assoc "IF - Ramo Else" Test_alcosigns.tests;
    "Sign - IF - Ambiguità", List.assoc "IF - Ambiguità" Test_alcosigns.tests;
    "Sign - IF - Assegnamento Parziale", List.assoc "IF - Assegnamento parziale" Test_alcosigns.tests;
    "Sign - IF - Ambiente Indipendente", List.assoc "IF - Ambiente Indipendente" Test_alcosigns.tests;
    "Sign - IF - Annidazioni", List.assoc "IF - Annidazioni" Test_alcosigns.tests;
    "Sign - IF - Condizioni Composte", List.assoc "IF - Condizioni Composte" Test_alcosigns.tests;
    "Sign - IF - Propagazione di BottomEnv", List.assoc "IF - Propagazione di BottomEnv" Test_alcosigns.tests;
    (* Test su While *)
    "Sign - While - Non Eseguito",List.assoc "While - non eseguito" Test_alcosigns.tests;
    "Sign - While - Converge",List.assoc "While - converge" Test_alcosigns.tests;
    "Sign - While - Perdita Precisione",List.assoc "While - perdita precisione" Test_alcosigns.tests;
    "Sign - While - Loop Infinito",List.assoc "While - loop infinito" Test_alcosigns.tests;
    "Sign - While - Propagazione Bottom",List.assoc "While - propagazione bottom" Test_alcosigns.tests;
    "Sign - While - Boundary Uguaglianza",List.assoc "While - boundary uguaglianza" Test_alcosigns.tests;
    "Sign - While - Annidati",List.assoc "While - annidati" Test_alcosigns.tests;
    "Sign - While - Personalizzati",List.assoc "While - Personali" Test_alcosigns.tests;

    (* Test su SimpleSign *)

    (* Espande i sottogruppi di Test_alcosimplesigns *)
    "SimpleSign - Somma", List.assoc "Somma" Test_alcosimplesigns.tests;
    "SimpleSign - Sottrazione", List.assoc "Sottrazione" Test_alcosimplesigns.tests;
    "SimpleSign - Moltiplicazione", List.assoc "Moltiplicazione" Test_alcosimplesigns.tests;
    "SimpleSign - Divisione", List.assoc "Divisione" Test_alcosimplesigns.tests;
    "SimpleSign - Negazione", List.assoc "Negazione" Test_alcosimplesigns.tests;
    "SimpleSign - Random", List.assoc "Random" Test_alcosimplesigns.tests;
    (* Test sui comandi (eval_cmd / eval) *)
    "SimpleSign - Assegnazioni", List.assoc "Assegnazioni" Test_alcosimplesigns.tests;
    "SimpleSign - Sequenze", List.assoc "Sequenze" Test_alcosimplesigns.tests;
    "SimpleSign - Overwrite", List.assoc "Overwrite" Test_alcosimplesigns.tests;
    "SimpleSign - Skip", List.assoc "Skip" Test_alcosimplesigns.tests;
    "SimpleSign - Stato precompilato", List.assoc "Stato precompilato" Test_alcosimplesigns.tests;
    "SimpleSign - Test Prog", List.assoc "Test Prog" Test_alcosimplesigns.tests;
    (* Test su Filter *)
    "SimpleSign - Filter - Certain", List.assoc "Filter - casi certi" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Uncertain", List.assoc "Filter - casi ambigui" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Simmetric", List.assoc "Filter - simmetria" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Derived", List.assoc "Filter - operatori derivati" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Composition", List.assoc "Filter - composizione And/Or/Not" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Bottom", List.assoc "Filter - valore Bottom" Test_alcosimplesigns.tests;
    "SimpleSign - Filter - Chaining", List.assoc "Filter - incatenato" Test_alcosimplesigns.tests;
    (* Test Su IF *)
    "SimpleSign - IF - Ramo Then", List.assoc "IF - Ramo Then" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Ramo Else", List.assoc "IF - Ramo Else" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Ambiguità", List.assoc "IF - Ambiguità" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Assegnamento Parziale", List.assoc "IF - Assegnamento parziale" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Ambiente Indipendente", List.assoc "IF - Ambiente Indipendente" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Annidazioni", List.assoc "IF - Annidazioni" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Condizioni Composte", List.assoc "IF - Condizioni Composte" Test_alcosimplesigns.tests;
    "SimpleSign - IF - Propagazione di BottomEnv", List.assoc "IF - Propagazione di BottomEnv" Test_alcosimplesigns.tests;
    (* Test su While *)
    "SimpleSign - While - Non Eseguito",List.assoc "While - non eseguito" Test_alcosimplesigns.tests;
    "SimpleSign - While - Converge",List.assoc "While - converge" Test_alcosimplesigns.tests;
    "SimpleSign - While - Perdita Precisione",List.assoc "While - perdita precisione" Test_alcosimplesigns.tests;
    "SimpleSign - While - Loop Infinito",List.assoc "While - loop infinito" Test_alcosimplesigns.tests;
    "SimpleSign - While - Propagazione Bottom",List.assoc "While - propagazione bottom" Test_alcosimplesigns.tests;
    "SimpleSign - While - Boundary Uguaglianza",List.assoc "While - boundary uguaglianza" Test_alcosimplesigns.tests;
    "SimpleSign - While - Annidati",List.assoc "While - annidati" Test_alcosimplesigns.tests;
    "SimpleSign - While - Personalizzati",List.assoc "While - Personali" Test_alcosimplesigns.tests;
  ]
  @ Test_alcointervals.tests
  ) *)
open Abstract_domains
open Interpeters
open Oracles

module Make_Sign_Tests (D : DOMAIN) (E: EXPECTED_VALUES with type t = D.t ) = struct
  (* Generiamo l'interprete specifico per questo dominio direttamente all'interno *)
  module Interp = AbsInterp (D)

  (* Visualizzatore Alcotest basato sul compare_type del dominio *)
  let sign_testable =
    Alcotest.testable
      (fun fmt s -> Format.fprintf fmt "%s" (D.to_string s)) (* Usa D.to_string se disponibile *)
      (fun a b -> a = b)

  (* 
     Stato di test generico: usiamo abstract_int e abstract_range forniti da D 
     in modo che la costruzione dello stato rispetti le capacità del dominio.
  *)
  let make_test_state () =
    let st = Hashtbl.create 10 in
    let mappings = [
      ("x", D.abstract_range 1 10);      (* Valore Positivo *)
      ("y", D.abstract_range (-10) (-1)); (* Valore Negativo *)
      ("z", D.abstract_int 0);           (* Zero *)
      ("w", D.abstract_range 0 10);      (* PosZero / Non-negativo *)
      ("k", D.abstract_range (-10) 0);   (* NegZero / Non-positivo *)
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

  let make_prog_case_with_env (desc, prog, expected_vars) =
    ( desc,
      `Quick,
      fun () ->
        let final_st = Interp.eval_cmd prog (Interp.Env (make_test_state ())) in
        check_vars desc final_st expected_vars )

  (* ------------------------------------------------------------ *)
  (* TEST ESPRESSIONI                                             *)
  (* ------------------------------------------------------------ *)

  let sumtests = List.map make_case [
    "Sum: Pos + Zero", BinaryOperation (Var "x", Add, Var "z"), E.expected_sum_1;
    "Sum: Zero + Zero", BinaryOperation (Var "z", Add, Var "z"), E.expected_sum_2;
    "Sum: Top + Pos", BinaryOperation (Var "t", Add, Var "x"), E.expected_sum_3;
    "Sum: Bottom + Pos", BinaryOperation (Var "b", Add, Var "x"), E.expected_sum_4;
    "Sum: Pos + Neg", BinaryOperation (Var "x", Add, Var "y"), E.expected_sum_5;
  ]

  let multests = List.map make_case [
    "Mul: Pos * Zero", BinaryOperation (Var "x", Mul, Var "z"), E.expected_mul_1;
    "Mul: Top * Zero", BinaryOperation (Var "t", Mul, Var "z"), E.expected_mul_2;
    "Mul: Bottom * Pos", BinaryOperation (Var "b", Mul, Var "x"), E.expected_mul_3;
    "Mul: Neg * Neg", BinaryOperation (Var "y", Mul, Var "y"), E.expected_mul_4;
  ]

  let divtests = List.map make_case [
    "Div: Const / Zero", BinaryOperation (Const 10, Div, Var "z"), E.expected_div_1;
    "Div: Zero / Pos", BinaryOperation (Var "z", Div, Var "x"), E.expected_div_2;
  ]

  (* ------------------------------------------------------------ *)
  (* TEST COMANDI E CONTROLLO DI FLUSSO                           *)
  (* ------------------------------------------------------------ *)

  let assigntests = [
    make_prog_case_with_env ("Assign: x = 5", Assign ("x", Const 5), ["x", E.expected_assign_1]);
    make_prog_case_with_env ("Assign: x = -5", Assign ("x", Const (-5)), ["x", E.expected_assign_2]);
  ]

  let iftests = [
    make_prog_case_with_env
      ( "If Pos: if (x > 0) then y = 1 else y = -1",
        If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1), Assign ("y", Const (-1))),
        ["y", E.expected_if_1] );
  ]

  let tests = [
    "Somma", sumtests;
    "Moltiplicazione", multests;
    "Divisione", divtests;
    "Assegnamenti", assigntests;
    "Istruzioni Condizionali", iftests;
  ]
end
module TestSuite_Signs = Make_Sign_Tests (Abstract_domains.Signs) (Expected_Signs)
module TestSuite_SimpleSigns = Make_Sign_Tests (Abstract_domains.SimpleSigns) (Expected_SimpleSigns)
module TestSuite_ReducedSigns = Make_Sign_Tests (Abstract_domains.ReducedSigns)
module TestSuite_SimplifiedSigns = Make_Sign_Tests (Abstract_domains.SimplifiedSigns)
module TestSuite_StrangeSigns = Make_Sign_Tests (Abstract_domains.StrangeSigns)
(* module TestSuite_Intervals = Make_Sign_Tests (Abstract_domains.Intervals) *)

(* 2. Esecuzione tramite Alcotest *)
(* let () =
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
    @ List.map (fun (name, test_list) -> ("SimpleSigns: " ^ name, test_list)) TestSuite_SimpleSigns.tests
  )