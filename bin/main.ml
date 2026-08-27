open Syntax
open Interpeters
(* open Abstract_domains.Signs-- *)
(* open Abstract_domains.SimpleSigns *)
open Abstract_domains.Intervals
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
    (* Sequence(
      Sequence(
        Assign ("x", Const(1)),
        Sequence(
          Assign("y", Const (2)),
          Assign("z", Random((-3),5) )
        ) 
      ),
      While(
        Comparison(Var"x",Equals,Var"y"),
        If( 
          Comparison(Var"y",Bigger,Var "z"),
          Assign("x",BinaryOperation(Var("x"),Add,Const(-2))),
          Assign("y",BinaryOperation(Var("x"),Add,Var("y")))
        )
        
      )
    ) *)
     (* If (Comparison (Var "n", Bigger, Const 0),
         Sequence (Assign ("x", Const 999), Skip),
         Assign("y",Var "x") *)
         (* Assign ("y", BinaryOperation (Var "x", Add, Const 0))) *)
    (* Sequence (
      (* Assign ("b", Bottom),
      Sequence( *)
        Assign("x",Random (2,7)), 
        Filter (Comparison (Var "b", Equals, Var "x"))
      (* ) *)
    ) *)
    Sequence (Assign ("x", Const 0),
       While (Comparison (Var "x", Smaller, Const 10),
              Assign ("x", BinaryOperation (Var "x", Add, Const 2))))
  )
  
(* let string_of_sign v =
  match v with
  | Pos -> "+"
  | Neg -> "-"
  | Zero -> "0"
  | PosZero -> ">=0"
  | NegZero -> "<=0"
  | NonZero -> "!=0"
  | SignTop -> "T (Top)"
  | SignBottom -> "_|_ (Bottom)" *)

(* let string_of_simple_sign v =
  match v with
  | Pos -> "+"
  | Neg -> "-"
  | Zero -> "0"
  | SignTop -> "T (Top)"
  | SignBottom -> "_|_ (Bottom)" *)

  let bound_to_string = function
    | NegInf -> "-inf"
    | PosInf -> "+inf"
    | Int n -> string_of_int n

  let interval_to_string = function
    | Bottom -> "Bottom"
    | Interval (a, b) ->
        Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

let outputStatePrinter state = 
  match state with
  | IntervalInterp.BottomEnv ->
      print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
  | IntervalInterp.Env tbl ->
      Hashtbl.iter (fun var v ->
        Printf.printf "%s : %s\n" var (interval_to_string v)
      ) tbl


let () =
  let env = Hashtbl.create 10 in 
  Hashtbl.replace env "x" (Interval (Int 2, Int 7));
  Hashtbl.replace env "y" (Interval (Int (-8), Int (-3)));
  Hashtbl.replace env "z" (Interval (Int 0, Int 0));
  Hashtbl.replace env "w" (Interval (Int 0, Int 5));
  Hashtbl.replace env "k" (Interval (Int (-5), Int 0));
  Hashtbl.replace env "n" (Interval (Int (-3), Int 4));
  Hashtbl.replace env "t" top;
  Hashtbl.replace env "p" (Interval (Int 1, PosInf));
  Hashtbl.replace env "m" (Interval (NegInf, Int (-1)));
  Hashtbl.replace env "b" bottom;
  let risultato : IntervalInterp.state = IntervalInterp.eval_cmd test_prog (Env(env)) in
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