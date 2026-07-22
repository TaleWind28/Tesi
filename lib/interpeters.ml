open Abstract_domains
open Syntax

(* Interprete astratto parametrico sul dominio D *)
module AbsInterp (D : DOMAIN) = struct

    type state =
        | Env of (string, D.t) Hashtbl.t
        | BottomEnv 

    let lub_env e1 e2 : state = failwith "not implemented"

    let negate_comp comp = match comp with
    | Bigger -> Smaller
    | Smaller -> Bigger
    | BiggerEquals -> SmallerEquals
    | SmallerEquals -> BiggerEquals
    | Equals -> NotEquals
    | NotEquals -> Equals

    let rec negate_cond cd = match cd with
    | Not cd -> cd
    | Boolean b -> Boolean (not b)
    | And (cd1,cd2) -> Or(negate_cond cd1,negate_cond cd2)
    | Or (cd1, cd2) -> And(negate_cond cd1, negate_cond cd2)
    | Comparison (e1,comp,e2) -> Comparison(e1,negate_comp comp ,e2)
  

    let rec eval_exp (exp : exp) (st : state) : D.t =
    match st with
    | BottomEnv -> D.bottom
    | Env st -> 
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
            D.sum (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | BinaryOperation (e1, Sub, e2) ->
            D.sum (eval_exp e1 (Env(st))) (D.negate(eval_exp e2 (Env(st))))
        | BinaryOperation (e1, Mul, e2) ->
            D.mul (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | BinaryOperation (e1, Div, e2) ->
            D.div (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | UnaryOperation (Negation, e1) ->
            D.negate (eval_exp e1 (Env(st)))
        | Random (a, b) ->
            D.abstract_range a b

    let rec eval_cond (cond : cond) (env : state) :  state = 
        match env with 
        |BottomEnv -> BottomEnv
        | Env env -> 
            match cond with
            | Boolean true -> Env(env)
            | Boolean false -> BottomEnv
            | Not cd -> 
                eval_cond (negate_cond cd) (Env( env ))
            | And (cd1,cd2) -> 
                let env' = eval_cond cd1 (Env(env)) in 
                eval_cond cd2 env'
            | Or (cd1,cd2) -> 
                let env1 = eval_cond cd1 (Env(Hashtbl.copy env)) in
                let env2 = eval_cond cd2 (Env(Hashtbl.copy env)) in
                lub_env env1 env2
            | Comparison (e1,comp,e2) -> failwith "not implemented"

    let rec eval_cmd (command : cmd) (env : state) : state =
         match env with 
        |BottomEnv -> BottomEnv
        | Env env -> 
            match command with
            | Assign( ide,exp) -> 
                let v =  eval_exp exp (Env(env)) in 
                Hashtbl.replace env ide v;
                Env(env)
            | Sequence(c1,c2) -> 
                let env1  = eval_cmd c1 (Env(env)) in 
                eval_cmd c2 env1

            | Filter(cd) -> eval_cond cd (Env(env)) 
            
            | Skip -> Env(env)
    
    let eval (prog : cmd) : state =
        let initial_env = Env(Hashtbl.create 10) in
        eval_cmd prog initial_env
end

(* Istanza concreta con il dominio dei segni *)
module SignInterp = AbsInterp (Signs)

(* Istanza concreta con il dominio degli Intervalli *)
module IntervalInterp = AbsInterp (Intervals)
