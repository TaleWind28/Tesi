open Syntax
open Interpeters
open Abstract_domains.Signs

let test_prog =(
  (* Sequence(
    Sequence(
      Assign ("x", Const(0)), Assign("y", Const (-9))
    ), *)
    (* Filter (Comparison (Var "x", Bigger, Var "y")) *)
    (* Filter (Comparison (Var "y", Smaller, Var "x")) *)
    (* Filter (Comparison (Var "x", Equals, Var "x")) *)
    (* Filter (Comparison (Var "x", NotEquals, Var "y")) *)
      (* Assign("z", BinaryOperation(Var "x", Add, Var "y")),
      Sequence(
        Filter(Comparison(Var "x",Smaller,Var "y")),
        Assign("z", BinaryOperation(Var "x", Add, Var "y"))
      )
    ) *)
    (* Filter (Comparison (Var "x", Bigger, Var "y")) *)
    (* If(Comparison(Var "x", NotEquals, Var "y"),Assign ("x", Const(-9)),Assign ("y", Const(0))) *)
    Sequence(
      Sequence(
        Assign ("x", Const(1)), Assign("y", Const (2))
      ),
      While(
        Comparison(Var"x",Equals,Var"y"),
        Assign("x",BinaryOperation(Var("x"),Add,Const (-1)))
        )
    )
  )
  (* ) *)
  
let string_of_sign v =
  match v with
  | Pos -> "+"
  | Neg -> "-"
  | Zero -> "0"
  | PosZero -> ">=0"
  | NegZero -> "<=0"
  | NonZero -> "!=0"
  | SignTop -> "T (Top)"
  | SignBottom -> "_|_ (Bottom)"

let outputStatePrinter state = 
  match state with
  | SignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (string_of_sign v)
      ) tbl

let () =
  let risultato : SignInterp.state = SignInterp.eval test_prog in
  outputStatePrinter risultato

(* let () =
  let env = Hashtbl.create 10 in
  Hashtbl.replace env "x" Pos;
  Hashtbl.replace env "y" Neg;
  (* Hashtbl.replace env "w" NonZero; *)

  let risultato : SignInterp.state =
    SignInterp.eval_cmd test_prog (SignInterp.Env env)
  in
  match risultato with
  | SignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (string_of_sign v)
      ) tbl *)