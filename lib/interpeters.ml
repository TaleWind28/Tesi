open Abstract_domains
open Syntax

(* Interprete astratto parametrico sul dominio D *)
module AbsInterp (D : DOMAIN) = struct
  
    type state = (string, D.t) Hashtbl.t

    let rec eval (exp : exp) (st : state) : D.t =
    match exp with
        | Const n ->
            D.abstract_int n
        | Var s ->
            (
            match Hashtbl.find_opt st s with
                | Some v -> v
                | None   -> D.top
            )
        | BinaryOperation (e1, Add, e2) ->
            D.sum (eval e1 st) (eval e2 st)
        | BinaryOperation (e1, Sub, e2) ->
            D.sum (eval e1 st) (D.negate(eval e2 st))
        | BinaryOperation (e1, Mul, e2) ->
            D.mul (eval e1 st) (eval e2 st)
        | BinaryOperation (e1, Div, e2) ->
            D.div (eval e1 st) (eval e2 st)
        | UnaryOperation (Negation, e1) ->
            D.negate (eval e1 st)
        | Random (a, b) ->
            D.abstract_range a b
end

(* Istanza concreta con il dominio dei segni *)
module SignInterp = AbsInterp (Signs)

(* Istanza concreta con il dominio degli Intervalli *)
module IntervalInterp = AbsInterp (Intervals)
