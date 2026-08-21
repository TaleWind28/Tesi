open Abstract_domains
open Syntax

(* Interprete astratto parametrico sul dominio D *)
module AbsInterp (D : DOMAIN) = struct

    type state =
    | Env of (string, D.t) Hashtbl.t
    | BottomEnv 

    let lub_env e1 e2 : state = match e1,e2 with 
    | BottomEnv, e | e, BottomEnv -> e
    | Env t1, Env t2 -> 
        let result = Hashtbl.create (Hashtbl.length t1 + Hashtbl.length t2) in
        Hashtbl.iter(
        fun var v1 -> 
            match Hashtbl.find_opt t2 var with 
                | Some v2 -> Hashtbl.replace result var (D.lub v1 v2)
                | None -> Hashtbl.replace result var v1
        )
        t1;

        Hashtbl.iter(fun var v2 -> if not (Hashtbl.mem result var) then Hashtbl.replace result var v2)
        t2;
        Env(result)

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
            | Comparison (e1,comp,e2) -> 
                let val1 = eval_exp e1 (Env(env)) in 
                let val2 = eval_exp e2 (Env(env)) in 
                match comp with
                | Equals -> eval_cond (Boolean(let condition = (D.compare_type val1 val2) in condition == 0 || condition == 2)) (Env(env))
                | NotEquals -> eval_cond (Boolean(let condition = (D.compare_type val1 val2) in condition != 0 )) (Env(env))
                | Bigger -> eval_cond (Boolean(let condition = (D.compare_type val1 val2) in condition == 1 || condition == 2)) (Env(env))
                | Smaller -> eval_cond (Boolean(let condition = (D.compare_type val1 val2) in condition == -1 || condition == 2)) (Env(env))
                | BiggerEquals -> eval_cond(Or(Comparison(e1,Bigger,e2),Comparison(e1,Equals,e2))) (Env(env))
                | SmallerEquals -> eval_cond(Or(Comparison(e1,Smaller,e2),Comparison(e1,Equals,e2))) (Env(env))
                
    let rec eval_cmd (command : cmd) (env : state) : state =
         match env with 
        |BottomEnv -> BottomEnv
        | Env env -> 
            match command with
            | Assign(ide,exp) -> 
                let v =  eval_exp exp (Env(env)) in 
                Hashtbl.replace env ide v;
                Env(env)
            | Sequence(c1,c2) -> 
                let env1  = eval_cmd c1 (Env(env)) in 
                eval_cmd c2 env1

            | Filter(cd) -> eval_cond cd (Env(env)) 

            | Skip -> Env(env)

            | If(cond,thencmd,elsecmd) -> 
                let e1 = eval_cmd (Sequence(Filter(cond),thencmd)) (Env(Hashtbl.copy env)) in 
                let e2 = eval_cmd (Sequence(Filter(Not(cond)),elsecmd)) (Env(Hashtbl.copy env)) in 
                lub_env e1 e2
                    
            | While(cond,cmd) -> 
                let filteredState = eval_cmd (Filter(cond)) (Env(env)) in 
                (
                    match filteredState with
                    | BottomEnv -> eval_cmd Skip (Env(env))
                    | Env(env') -> 
                        let iteratedState = eval_cmd cmd filteredState 
                        in eval_cmd (While(cond,cmd)) (iteratedState)
                )


    let eval (prog : cmd) : state =
        let initial_env = Env(Hashtbl.create 10) in
        eval_cmd prog initial_env
end

(* Istanza concreta con il dominio dei segni *)
module SignInterp = AbsInterp (Signs)

(* Istanza concreta con il dominio degli Intervalli *)
module IntervalInterp = AbsInterp (Intervals)
