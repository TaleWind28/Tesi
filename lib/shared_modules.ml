open Syntax

module IntervalArith = struct
  type bound = NegInf | Int of int | PosInf 
  type value = Interval of bound * bound | Bottom

  let abstract_int n = Interval (Int n,Int n)

  let abstract_range c1 c2 = 
  if c1 > c2 then Interval (Int c2, Int c1)
  else if c2 > c1 then Interval (Int c1, Int c2)
  else abstract_int c1

  let bound_to_string bound = match bound with
  | NegInf -> "-Inf"
  | Int(n) -> string_of_int n
  | PosInf -> "+Inf" 
  let to_string t = match t with
  | Bottom -> "Bottom"
  | Interval (b1,b2)-> 
    Printf.sprintf "[%s, %s]" (bound_to_string b1) (bound_to_string b2)

  let next_bound bound = match bound with
  | NegInf -> NegInf
  | PosInf -> PosInf
  | Int n -> Int (n+1)

  let prev_bound bound = match bound with
    | NegInf -> NegInf
    | PosInf -> PosInf
    | Int n -> Int (n-1)

  let min_bound x y = match x,y with
  | _,NegInf | NegInf,_ -> NegInf
  | PosInf,a | a,PosInf -> a
  | Int a, Int b -> Int( min a b)

  let max_bound x y = match x,y with
  | _,PosInf | PosInf,_ -> PosInf
  | NegInf,a | a,NegInf -> a
  | Int a, Int b -> Int( max a b)

  let compare_bound c1 c2 = match c1,c2 with
  | x,y when x = y -> 0
  | NegInf,_ | _,PosInf -> -1
  | _,NegInf | PosInf,_ -> 1
  | Int x, Int y -> compare x y

  let leq  c1 c2 = match c1,c2 with
    |Bottom,_ -> true
    |_,Bottom -> false
    |Interval (a,b), Interval(c,d)-> compare_bound a c >= 0 && compare_bound d b >= 0

  let compare_type x y =  match x,y with
    | Bottom,_ -> -1
    | _,Bottom -> 1
    | Interval(a,b),Interval(c,d) -> 
      if compare_bound a b = 0 && compare_bound a c = 0 
       && compare_bound b d = 0 && compare_bound c d = 0 then 0
      (* singleton coincidenti: a=b=c=d *)
      else if compare_bound a d > 0 then 1   (* minimo di x supera massimo di y *)
      else if compare_bound b c < 0 then -1  (* massimo di x è sotto il minimo di y *)
      else 2   
    
  let lub c1 c2 = match c1,c2 with 
    | Bottom,x | x,Bottom -> x
    | Interval(a,b), Interval(c,d) -> Interval(min_bound a c ,max_bound b d )

  let widen c1 c2 = match c1,c2 with
  | Bottom,x | x,Bottom -> x
  | Interval(a,b), Interval(c,d) -> 
    let min = if compare_bound a c <= 0 then a else NegInf in 
    let max = if compare_bound d b <= 0 then b else PosInf in
    Interval(min,max)

  let narrow x y = if leq y x then y else x

  let glb c1 c2 = match c1,c2 with
  | Bottom,_ | _,Bottom -> Bottom
  | Interval(a,b) , Interval(c,d) -> 
    let lo = max_bound a c in 
    let hi = min_bound b d in 
    if compare_bound lo hi > 0 then Bottom else Interval(lo, hi) 

  (*Helper*)
  let add_bound a b = match a,b with 
    | PosInf,NegInf | NegInf,PosInf -> PosInf (*dovrebbe dare bottom*)
    | PosInf,_ | _,PosInf -> PosInf
    | NegInf,_ | _,NegInf -> NegInf
    | Int a, Int b -> Int (a+b)

  
  let mul_bound x y = match x,y with
  | Int x, Int y -> Int( x* y)
  | NegInf, NegInf | PosInf,PosInf -> PosInf
  | NegInf,PosInf | PosInf,NegInf -> NegInf
  | Int 0, _ | _, Int 0 -> Int 0
  | Int x, PosInf | PosInf, Int x -> if x>= 0 then PosInf else NegInf
  | Int x, NegInf | NegInf, Int x -> if x>= 0 then NegInf else PosInf
  
  let mul_helper a b c d = 
    let p1 = mul_bound a c in
    let p2 = mul_bound a d in
    let p3 = mul_bound b c in
    let p4 = mul_bound b d in 
    
    Interval (min_bound (min_bound p1 p2) (min_bound p3 p4), max_bound (max_bound p1 p2) (max_bound p3 p4) )
  

  let div_bound x y = match x,y with
  | Int 0,_ -> Int 0
  | _,Int 0 -> PosInf
  | _,PosInf | _,NegInf -> Int 0
  | PosInf, Int b -> if b > 0 then PosInf else NegInf
  | NegInf, Int b -> if b > 0 then NegInf else PosInf 
  | Int a, Int b -> Int (a/b)


  let neg_bound = function
    | PosInf -> NegInf 
    | NegInf -> PosInf
    | Int n -> Int (-n)

  let sum c1 c2 = match c1, c2 with
    |Bottom,_ | _,Bottom -> Bottom
    | Interval(a,b),Interval(c,d) -> 
      Interval (add_bound a c,add_bound b d)
  
  let negate = function 
    |Bottom -> Bottom
    | Interval(a,b) -> Interval(neg_bound b,neg_bound a)

  let mul c1 c2 = match c1,c2 with
    | Bottom,_ | _,Bottom -> Bottom
    | Interval(a,b),Interval(c,d) -> mul_helper a b c d
  
  let rec div c1 c2 = 
    let div_helper a b c d = 
      let p1 = glb (Interval(c,d)) (Interval(Int 1,PosInf)) in 
      let p2 = glb (Interval(c,d)) (Interval(NegInf,Int (-1))) in 
      let p3 = Interval(a,b) in 
      let r1 = div p3 p1 in 
      let r2 = div p3 p2 in
      lub r1 r2 in  
    match c1,c2 with
    | Bottom,_ | _,Bottom -> Bottom
    | Interval(a,b), Interval(c,d) -> 
      if c >= Int 1 then Interval(min_bound (div_bound a c) (div_bound a d), max_bound (div_bound b c) (div_bound b d))
      else if d <= Int(-1) then  Interval(min_bound (div_bound b c) (div_bound b d), max_bound (div_bound a c) (div_bound a d))
      else div_helper a b c d

end

module DBMOperations = struct
include IntervalArith

  type dbm = {
    n : int;
    env : (ide * int) list;
    matrix : bound array array;
  }

  type t = Bottom | Env of dbm

  let create_type_dbm n env matrix = Env { n; env; matrix }

  let copy_matrix m = Array.map Array.copy m

  let index_of env x = try Some (List.assoc x env) with Not_found -> None

  let resolve_index env x =
    match index_of env x with
    | Some i -> i
    | None -> failwith (Printf.sprintf "Zones: variabile '%s' non dichiarata" x)

  let b_leq b1 b2 = 
    match b1, b2 with
    | NegInf, _ -> true
    | Int x, Int y -> x <= y
    | PosInf, Int _ -> false
    | _, PosInf -> true
    | _ -> false

  let leq_matrix m n = 
    Array.for_all2 (
      fun riga1 riga2 -> Array.for_all2 b_leq riga1 riga2
    ) m.matrix n.matrix

  let lub_matrix m n = 
        Array.map2 (
          fun rigam rigan -> Array.map2 max_bound rigam rigan
        ) m.matrix n.matrix 

  let glb_matrix m n = 
      Array.map2 (
          fun rigam rigan -> Array.map2 min_bound rigam rigan
        ) m.matrix n.matrix 

  let widen_matrix m n = 
    let widen_bound bm bn = if b_leq bn bm then bm else PosInf in 
    Array.map2 (
      fun rigam rigan -> Array.map2 widen_bound rigam rigan
    ) m.matrix n.matrix
  
  let narrow_matrix m n = 
    let narrow_bound bm bn = 
        match bm with
        | PosInf -> bn 
        | _ -> bm
      in 
        Array.map2 (
          fun rigam rigan -> Array.map2 narrow_bound rigam rigan
        ) m.matrix n.matrix 
  let iter_cube dim f = 
    for k = 0 to dim - 1 do 
      for i = 0 to dim - 1 do 
        for j = 0 to dim - 1 do
          f k i j
        done
      done
    done

  let floyd_wharshall matrix n = 
    let res_m = copy_matrix matrix in 
    iter_cube n (fun k i j ->  
      match res_m.(i).(k), res_m.(k).(j) with
      | Int x, Int y -> 
        let actual_val = res_m.(i).(j) in  
        let k_path = Int (x + y) in
        if b_leq k_path actual_val then res_m.(i).(j) <- k_path
      | _ -> ()); 
    res_m
  let rec has_neg_cycle matrix dim i = 
        if i >= dim then false
        else match matrix.(i).(i) with
        | Int x -> if x < 0 then true else has_neg_cycle matrix dim (i + 1)
        | _ -> has_neg_cycle matrix dim (i + 1)
end

module VariableRetrieval = struct
  let rec retrieve_var_from_exp (exp : exp) : ide list = 
  match exp with
  | Const _-> []
  | Random _ -> []
  | Var x -> [x]
  | BinaryOperation(e1,bop,e2) -> retrieve_var_from_exp e1 @ retrieve_var_from_exp e2
  | UnaryOperation(uop,e) -> retrieve_var_from_exp e
  let rec retrieve_var_from_cond (cond:cond) : ide list = 
    match cond with
    | Comparison(e1,comp,e2) -> retrieve_var_from_exp e1 @ retrieve_var_from_exp e2
    | Boolean _ -> []
    | Not cd -> retrieve_var_from_cond cd
    | And(cd1,cd2) -> retrieve_var_from_cond cd1 @ retrieve_var_from_cond cd2
    | Or(cd1,cd2) -> retrieve_var_from_cond cd1 @ retrieve_var_from_cond cd2
  let rec get_all_var (prog: cmd) : ide list = 
    match prog with
    | Skip -> []
    | Sequence(c1,c2)-> get_all_var c1 @ get_all_var c2
    | If(cd,cthen,celse) -> get_all_var cthen @ get_all_var celse @ retrieve_var_from_cond cd
    | While(cd,c) -> get_all_var c @ retrieve_var_from_cond cd
    | Filter(cd) -> retrieve_var_from_cond cd
    | Assign(ide,e1) -> ide :: retrieve_var_from_exp e1
  
end

module SyntaxUtils = struct
  let negate_comp comp = match comp with
    | Bigger -> SmallerEquals
    | Smaller -> BiggerEquals
    | BiggerEquals -> Smaller
    | SmallerEquals -> Bigger
    | Equals -> NotEquals
    | NotEquals -> Equals

  let rec negate_cond cd = match cd with
    | Not cd -> cd
    | Boolean b -> Boolean (not b)
    | And (cd1,cd2) -> Or(negate_cond cd1,negate_cond cd2)
    | Or (cd1, cd2) -> And(negate_cond cd1, negate_cond cd2)
    | Comparison (e1,comp,e2) -> Comparison(e1,negate_comp comp ,e2)
  
  let inv_comp comp = match comp with
    | Bigger -> Smaller
    | Smaller -> Bigger
    | BiggerEquals -> SmallerEquals
    | SmallerEquals -> BiggerEquals
    | Equals -> Equals
    | NotEquals -> NotEquals

end

module Fixpoint = struct
  let compute_invariant ~widen ~narrow ~leq ~f init =
    let rec kleene x =
      let x' = widen x (f x) in
      if leq x' x then x else kleene x'
    in
    let post_fp = kleene init in
    let rec descend x =
      let x' = narrow x (f x) in
      if leq x x' then x else descend x'
    in
    descend post_fp
end
