open Syntax
open Interpeters
let test_prog =(
    (* Sequence(
      Assign("x",Const(5)),
      If (
        Comparison (Var "x", Bigger, Const 0), 
        Assign ("y", Const 1),
        Assign ("y", Const (-1))
      )
    ) *)
     Sequence (
      Assign ("x", Const (5)), 
      While (
        Comparison (Var "x", NotEquals, Const 0), 
        Assign ("x", Const 0))
      )
)

let outputStatePrinter state = 
  match state with
  | SimpleSignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SimpleSignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (Abstract_domains.SimpleSigns.to_string v)
      ) tbl
  (* 
  | SignInterp.BottomEnv ->  print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (Abstract_domains.Signs.to_string v)
      ) tbl *)


let () =
  let risultato = SimpleSignInterp.eval test_prog in
  outputStatePrinter risultato;;
  (* let risultato : SignInterp.state = SignInterp.eval test_prog in
  outputStatePrinter risultato;; *)
