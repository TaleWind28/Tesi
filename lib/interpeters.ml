open Abstract_domains
open Syntax

(* Interprete astratto parametrico sul dominio D *)
module AbsInterp (D : DOMAIN) = struct


    type state =
    | Env of (string, D.t) Hashtbl.t
    | BottomEnv 

    let outputStatePrinter state = 
        match state with
            | BottomEnv ->
                print_endline "Lo stato finale è BottomEnv (irraggiungibile / bottom)"
            | Env tbl ->
                Hashtbl.iter (fun var v ->
                    Printf.printf "%s : %s\n" var (D.to_string v)
                ) tbl

    let lub_env e1 e2 : state = match e1,e2 with 
    | BottomEnv,BottomEnv -> BottomEnv
    | BottomEnv, Env e | Env e, BottomEnv -> Env(Hashtbl.copy e)
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

    let inv_comp comp = match comp with
    | Bigger -> Smaller
    | Smaller -> Bigger
    | BiggerEquals -> SmallerEquals
    | SmallerEquals -> BiggerEquals
    | Equals -> Equals
    | NotEquals -> NotEquals

    let refine_vars e1 e2 v1 v2 env comp =
        let new_env = Hashtbl.copy env in
        let became_bottom = ref false in

        (* Raffina e1 usando il vincolo derivato da val2 e dal comparatore diretto *)
        (match e1 with
        | Var s1 ->
            let filtered = D.filter_rel comp v2 in
            let refined = D.glb v1 filtered in
            if refined = D.bottom then became_bottom := true;
            Hashtbl.replace new_env s1 refined
        | _ -> ());

        (* Raffina e2 usando il vincolo derivato da val1 e dal comparatore CONVERSO *)
        (match e2 with
        | Var s2 ->
            let inve_comp = inv_comp comp in
            let filtered = D.filter_rel inve_comp v1 in
            let refined = D.glb v2 filtered in
            if refined = D.bottom then became_bottom := true;
            Hashtbl.replace new_env s2 refined
        | _ -> ());

        if !became_bottom then BottomEnv
        else Env new_env

    
    let widen_env e1 e2 =
        match e1, e2 with
        | BottomEnv, e | e, BottomEnv -> e
        | Env(t1), Env(t2) ->
            let result = Hashtbl.create (Hashtbl.length t1) in
            Hashtbl.iter (fun var v1 ->
                let v2 = try Hashtbl.find t2 var with Not_found -> D.bottom in
                Hashtbl.add result var (D.widen v1 v2)
            ) t1;
            (* eventuali variabili presenti solo in env2 *)
            Hashtbl.iter (fun var v2 ->
                if not (Hashtbl.mem result var) then
                Hashtbl.add result var (D.widen D.bottom v2)
            ) t2;
        Env(result)

    let narrow_env e1 e2 =
    match e1, e2 with
    | BottomEnv, _ -> BottomEnv
    | e, BottomEnv -> e
    | Env(t1), Env(t2) ->
        let result = Hashtbl.create (Hashtbl.length t1) in
        Hashtbl.iter (fun var v1 ->
            let v2 = try Hashtbl.find t2 var with Not_found -> v1 in
            Hashtbl.add result var (D.narrow v1 v2)
        ) t1;
        (* variabili presenti solo in e2 (raro, ma per simmetria con widen_env) *)
        Hashtbl.iter (fun var v2 ->
            if not (Hashtbl.mem result var) then
            Hashtbl.add result var (D.narrow D.top v2)
        ) t2;
        Env(result)
    
    let leq_env e1 e2 =
        match e1, e2 with
        | BottomEnv, _ -> true
        | _, BottomEnv -> false
        | Env(t1),Env(t2) ->

            Hashtbl.fold (fun var v1 acc ->
                acc &&
                let v2 = try Hashtbl.find t2 var with Not_found -> D.bottom in
                D.leq v1 v2
            ) t1 true
            



    let rec negate_cond cd = match cd with
    | Not cd -> cd
    | Boolean b -> Boolean (not b)
    | And (cd1,cd2) -> Or(negate_cond cd1,negate_cond cd2)
    | Or (cd1, cd2) -> And(negate_cond cd1, negate_cond cd2)
    | Comparison (e1,comp,e2) -> Comparison(e1,negate_comp comp ,e2)
  
    (* Valutazione Espressioni *)
    let rec eval_exp (exp : exp) (st : state) : D.t =
    (* Controllo lo stato *)
    match st with
    | BottomEnv -> D.bottom (* Se sono in bottomEnv si è verificato un'errore => restituisco il valore di bottom del dominio in analisi *)
    | Env st -> 
        match exp with
        | Const n -> (* Astraggo il valore *)
            D.abstract_int n 
        | Var s -> (* Cerco nello stato il valore associato alla variabile s *)
            (
            match Hashtbl.find_opt st s with
                | Some v -> v
                | None   -> D.top
            )
        | BinaryOperation (e1, Add, e2) -> (* valuto la somma *)
            D.sum (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | BinaryOperation (e1, Sub, e2) -> (* valuto la sottrazione cambiando di segno il secondo membro per avere così una somma algebrica *)
            D.sum (eval_exp e1 (Env(st))) (D.negate(eval_exp e2 (Env(st))))
        | BinaryOperation (e1, Mul, e2) -> (* valuto la moltiplicazione *)
            D.mul (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | BinaryOperation (e1, Div, e2) -> (* valuto la divisione *)
            D.div (eval_exp e1 (Env(st))) (eval_exp e2 (Env(st)))
        | UnaryOperation (Negation, e1) -> (* valuto la negazione, ossia il cambio di segno *)
            D.negate (eval_exp e1 (Env(st)))
        | Random (a, b) -> (* valuto un valore intero non deterministico *)
            D.abstract_range a b
    
    (* Valutazione Condizioni *)
    let rec eval_cond (cond : cond) (env : state) :  state = 
        match env with 
        |BottomEnv -> BottomEnv (* Se sono in bottomEnv si è verificato un'errore => restituisco BottomEnv *)
        | Env env -> 
            match cond with
            | Boolean true -> Env(env) (* Mantengo lo stato invariato *)
            | Boolean false -> BottomEnv (* Segnalo un'errore *)
            | Not cd -> (* Nego la condizione *)
                eval_cond (negate_cond cd) (Env( env ))
            | And (cd1,cd2) -> (* And Logico tra le condizioni *)
                let env' = eval_cond cd1 (Env(env)) in 
                eval_cond cd2 env'
            | Or (cd1,cd2) -> (* Or Logico tra le condizioni *)
                let env1 = eval_cond cd1 (Env(Hashtbl.copy env)) in
                let env2 = eval_cond cd2 (Env(Hashtbl.copy env)) in
                lub_env env1 env2
            | Comparison (e1,comp,e2) -> (* Comparazione tra espressioni mediante un comparatore*) 
                let val1 = eval_exp e1 (Env(env)) in (* Valuto e1 *)
                let val2 = eval_exp e2 (Env(env)) in (* Valuto e2 *)
                let condition = D.compare_type val1 val2 in (* compare_type viene implementato dal dominio in analisi*)
                let res = match comp with                 (* Pattern Matching per applicare il comparatore richiesto *)
                | Bigger  -> condition == 1 || condition == 2 (*1 -> val1 > val2*)
                | Smaller -> condition == -1 || condition == 2 (*-1 -> val1 < val2*)
                | BiggerEquals -> condition <> -1 (*-1 -> val1 <= val2*)
                | SmallerEquals -> condition <> 1 (*1 -> val1 >= val2*)
                | Equals -> condition == 0 || condition == 2 (*0 -> val1 == val2*)
                | NotEquals -> condition <> 0 (*0 -> val1 == val2*)
                in 
                if not res then BottomEnv
                else refine_vars e1 e2 val1 val2 env comp

    (* Valutazione Comandi *)
    let rec eval_cmd (command : cmd) (env : state) : state =
        match env with 
        | BottomEnv -> BottomEnv (* Se sono in bottomEnv si è verificato un'errore => restituisco BottomEnv *)
        | Env env -> 
            match command with
            | Assign(ide,exp) -> (* Assegnamento di un valore ad un identificatore *)
                let v =  eval_exp exp (Env(env)) in (* Valuto l'espressione*)
                let env' = Hashtbl.copy env in 
                Hashtbl.replace env' ide v; (* Rimpiazzo il valore se presente, altrimenti viene creata una nuova entry*)
                Env(env') (* Restituisco lo stato aggiornato *)
            
            | Sequence(c1,c2) -> (* Sequenza di Comandi *)
                let env1  = eval_cmd c1 (Env(env)) in  (* Valuto il primo memorizzando l'ambiente risultante *)
                eval_cmd c2 env1  (* Valuto il secondo utilizzando l'ambiente risultante dalla valutazione del primo *)
                

            | Filter(cd) -> eval_cond cd (Env(env)) (* Controllo se una condizione è rispettata *)
                        
            | Skip -> Env(env) (* Skip *)

            | If(cond,thencmd,elsecmd) -> (* Istruzione Condizionale i cui rami then ed else vengono sempre valutati e successivamente tramite lub si restringe lo stato *)
                let e1 = eval_cmd (Sequence(Filter(cond), thencmd)) (Env(Hashtbl.copy env)) in
                let e2 = eval_cmd (Sequence(Filter(Not(cond)), elsecmd)) (Env(Hashtbl.copy env)) in
                (* DEBUG *)
                let () = match e1 with BottomEnv -> print_endline "e1 is Bottom" | Env _ -> print_endline "e1 is Env" in
                let () = match e2 with BottomEnv -> print_endline "e2 is Bottom" | Env _ -> print_endline "e2 is Env" in
                (* lub_env e1 e2 *)
                lub_env e1 e2
                    
            | While(cond,cmd) -> (* Ciclo che tramite Least Fixpoint valuta  *)
                let f x = lub_env (Env(env)) (eval_cmd cmd (eval_cond cond x)) in (* Funzione che si occupa di valutare lo stato aggiornandolo ad ogni iterazione *)
                let lfp f = (*Tramite funzione ausiliaria kleene lfp restituisce, se possibile, il punto dopo il quale il ciclo smette di produrre risultati che espandono lo stato corrente *)
                    let rec kleene x = 
                        let x' = widen_env x (f x) in (* Viene effettuato un Widening sullo stato attuale e lo stato dopo aver applicato f *) 
                            if leq_env x' x then x (* Se gli stati sono uguali allora ho raggiunto il Least Fixpoint, altrimenti continuo ad iterare *)
                            else kleene x' 
                    in kleene (Env(env)) (* Parto dallo stato Vuoto e vado a "salire" *)
                in eval_cmd (Filter((Not(cond)))) (lfp f) (*Valuto la condizione che fa uscire dal while con lo stato una volta raggiunto il Least Fixpoint*) 
               

    (* Funzione eval generale *)
    let eval (prog : cmd) : state =
        let initial_env = Env(Hashtbl.create 10) in
        eval_cmd prog initial_env
end

(* Istanza concreta con il dominio dei segni *)
module SignInterp = AbsInterp (Signs)

module SimpleSignInterp = AbsInterp (SimpleSigns)

module ReducedSignInterp = AbsInterp (ReducedSigns)

module SimplifiedSignInterp = AbsInterp (SimplifiedSigns)

module StrangeSignInterp = AbsInterp (StrangeSigns)

(* Istanza concreta con il dominio degli Intervalli *)
module IntervalInterp = AbsInterp (Intervals)
