open Syntax
open Abstract_domains

module Make_Sign_Tests (D : DOMAIN) = struct
  (* Generiamo l'interprete specifico per questo dominio direttamente all'interno *)
  module Interp = AbsInterp (D)

  (* Visualizzatore Alcotest basato sul compare_type del dominio *)
  let sign_testable =
    Alcotest.testable
      (fun fmt _s -> Format.fprintf fmt "<abstract_value>") (* Usa D.to_string se disponibile *)
      (fun a b -> D.compare_type a b = 0 || D.compare_type a b  = 2)

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
    "Sum: Pos + Zero", BinaryOperation (Var "x", Add, Var "z"), D.abstract_range 1 10;
    "Sum: Zero + Zero", BinaryOperation (Var "z", Add, Var "z"), D.abstract_int 0;
    "Sum: Top + Pos", BinaryOperation (Var "t", Add, Var "x"), D.top;
    "Sum: Bottom + Pos", BinaryOperation (Var "b", Add, Var "x"), D.bottom;
    "Sum: Pos + Neg", BinaryOperation (Var "x", Add, Var "y"), D.sum (D.abstract_range 1 10) (D.abstract_range (-10) (-1));
  ]

  let multests = List.map make_case [
    "Mul: Pos * Zero", BinaryOperation (Var "x", Mul, Var "z"), D.abstract_int 0;
    "Mul: Top * Zero", BinaryOperation (Var "t", Mul, Var "z"), D.abstract_int 0;
    "Mul: Bottom * Pos", BinaryOperation (Var "b", Mul, Var "x"), D.bottom;
    "Mul: Neg * Neg", BinaryOperation (Var "y", Mul, Var "y"), D.mul (D.abstract_range (-10) (-1)) (D.abstract_range (-10) (-1));
  ]

  let divtests = List.map make_case [
    "Div: Const / Zero", BinaryOperation (Const 10, Div, Var "z"), D.bottom;
    "Div: Zero / Pos", BinaryOperation (Var "z", Div, Var "x"), D.abstract_int 0;
  ]

  (* ------------------------------------------------------------ *)
  (* TEST COMANDI E CONTROLLO DI FLUSSO                           *)
  (* ------------------------------------------------------------ *)

  let assigntests = [
    make_prog_case_with_env ("Assign: x = 5", Assign ("x", Const 5), ["x", D.abstract_int 5]);
    make_prog_case_with_env ("Assign: x = -5", Assign ("x", Const (-5)), ["x", D.abstract_int (-5)]);
  ]

  let iftests = [
    make_prog_case_with_env
      ( "If Pos: if (x > 0) then y = 1 else y = -1",
        If (Comparison (Var "x", Bigger, Const 0), Assign ("y", Const 1), Assign ("y", Const (-1))),
        ["y", D.abstract_int 1] );
  ]

  let tests = [
    "Somma", sumtests;
    "Moltiplicazione", multests;
    "Divisione", divtests;
    "Assegnamenti", assigntests;
    "Istruzioni Condizionali", iftests;
  ]
end