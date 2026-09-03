open Syntax
open Interpeters
let test_prog =(
    Sequence(
      Assign("x",Const(5)),
      If (
        Comparison (Var "x", Bigger, Const 0), 
        Assign ("y", Const 1),
        Assign ("y", Const (-1))
      )
    )
)

  (* let bound_to_string = function
    | NegInf -> "-inf"
    | PosInf -> "+inf"
    | Int n -> string_of_int n

  let interval_to_string = function
    | Bottom -> "Bottom"
    | Interval (a, b) ->
        Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b) *)

let outputStatePrinter state = 
  match state with
  (* | SimpleSignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SimpleSignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (Abstract_domains.SimpleSigns.to_string v)
      ) tbl *)
  | SignInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | SignInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (Abstract_domains.Signs.to_string v)
      ) tbl


let () =
  (* let env = Hashtbl.create 10 in 
  Hashtbl.replace env "x" (Interval (Int 2, Int 7));
  Hashtbl.replace env "y" (Interval (Int (-8), Int (-3)));
  Hashtbl.replace env "z" (Interval (Int 0, Int 0));
  Hashtbl.replace env "w" (Interval (Int 0, Int 5));
  Hashtbl.replace env "k" (Interval (Int (-5), Int 0));
  Hashtbl.replace env "n" (Interval (Int (-3), Int 4));
  Hashtbl.replace env "t" top;
  Hashtbl.replace env "p" (Interval (Int 1, PosInf));
  Hashtbl.replace env "m" (Interval (NegInf, Int (-1)));
  Hashtbl.replace env "b" bottom; *)
  (* let risultato : SimpleSignInterp.state = SimpleSignInterp.eval test_prog in
  outputStatePrinter risultato;; *)
  let risultato : SignInterp.state = SignInterp.eval test_prog in
  outputStatePrinter risultato;;

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

