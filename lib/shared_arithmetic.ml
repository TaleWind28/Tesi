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
