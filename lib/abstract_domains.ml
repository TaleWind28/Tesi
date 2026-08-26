(* Firma del dominio astratto *)
module type DOMAIN = sig
  type t
  val top    : t
  val bottom : t
  val lub    : t -> t -> t
  val leq    : t -> t -> bool
  val glb    : t-> t -> t
  val widen  : t -> t -> t 
  val compare_type : t -> t -> int

  val abstract_int   : int -> t
  val abstract_range : int -> int -> t
  val sum    : t -> t -> t
  val mul    : t -> t -> t
  val div    : t -> t -> t
  val negate : t -> t
end

(* Dominio dei segni *)
module Signs = struct
  type t = SignTop | Pos |PosZero | Zero | NegZero | Neg | NonZero | SignBottom
  
  let top    = SignTop
  let bottom = SignBottom

  let compare_type x y = match x,y with 
    | Zero,Zero -> 0
    | x,y when x = y -> 2
    | SignTop, _ -> 1
    | _,SignTop -> -1
    | SignBottom,_ -> -1
    | _,SignBottom -> 1
    | NonZero,_ -> 2
    | _,NonZero -> 2
    | PosZero,Pos -> 2
    | PosZero,NegZero -> 2
    | PosZero,_ -> 1
    | Pos,PosZero -> 2
    | NegZero, PosZero -> 2
    | _,PosZero -> -1
    | Pos,_ -> 1
    | _,Pos -> -1
    | Zero,NegZero | NegZero,Zero -> 2
    | Zero,_ -> 1
    | _,Zero -> -1
    | NegZero,Neg -> 2
    | Neg,NegZero -> 2
    | NegZero, NegZero -> 2
    | Neg,Neg -> 2

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x

    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | NonZero, NonZero | NonZero,Pos | NonZero,Neg | Neg,NonZero | Pos,NonZero | Neg,Pos | Pos,Neg -> NonZero
    | Zero, Pos | Pos,Zero | PosZero,Zero | Pos,PosZero  -> PosZero
    | Zero, Neg | Neg, Zero | NegZero,Neg | Neg, NegZero -> NegZero
    | _,_ -> SignTop

  let widen x y = lub x y
  
  let glb s1 s2 = match s1,s2 with
  | _,SignBottom | SignBottom,_ -> SignBottom
  | x,SignTop | SignTop,x -> x
  | x,y when x = y -> x
  | NonZero,Pos | NonZero,PosZero| Pos, PosZero | Pos,NonZero | PosZero,NonZero | PosZero,Pos-> Pos
  | NonZero,Neg | NonZero,NegZero |Neg,NegZero | Neg,NonZero | NegZero,NonZero | NegZero,Neg -> Neg
  | Zero,PosZero | Zero, NegZero | PosZero, NegZero | PosZero, Zero | NegZero, Zero | NegZero, PosZero-> Zero
  | _,_ -> SignBottom

  let leq s1 s2 = (glb s1 s2) = s1

  let abstract_int n =
    if n > 0 then Pos
    else if n < 0 then Neg
    else Zero

  let abstract_range a b = 
    if a > b then SignBottom
  else if a < 0 && b > 0 then SignTop
  else lub (abstract_int a) (abstract_int b)

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _       | _, Zero      -> Zero
    | Pos, Pos | Neg,Neg -> Pos
    | Neg,Pos | Pos,Neg -> Neg
    | PosZero, PosZero | NegZero, NegZero | PosZero, Pos |NegZero,Neg | Pos,PosZero |Neg,NegZero-> PosZero
    | PosZero, NegZero | PosZero, Neg | NegZero,Pos | NegZero, PosZero | Neg, PosZero | Pos, NegZero-> NegZero
    | NonZero,NonZero | NonZero,Pos | NonZero,Neg | Pos,NonZero | Neg,NonZero -> NonZero
    | _,_ -> SignTop

  let sum s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom  -> SignBottom
    | Zero,Zero -> Zero
    | Zero, n       | n, Zero      -> n
    | Pos, Pos | PosZero, Pos | Pos, PosZero -> Pos
    | Neg, Neg | NegZero, Neg | Neg, NegZero -> Neg
    | PosZero,PosZero  -> PosZero
    | NegZero,NegZero -> NegZero
    | _,_ -> SignTop
  
  (*Non del tutto corretta in quanto dovrebbe essere divisione intera*)
  let div s1 s2 = match s1, s2 with
    (*Errore/Irraggiungibile*)
    | SignBottom, _ | _, SignBottom -> SignBottom
    (* divisione per zero *)
    | _, Zero -> SignBottom
    | Zero,(Pos | Neg | NonZero) -> Zero
    (*Unici casi noti della tabella della divisione*)
    | Pos, Pos    | Neg, Neg     -> PosZero
    | Pos, Neg    | Neg, Pos     -> NegZero
    | PosZero,Neg | NegZero, Pos -> NegZero
    | PosZero,Pos | NegZero,Neg  -> PosZero
    (*Casi con possibili divisioni per 0 oppure divisioni con NonZero*)
    | _ -> SignTop

  let negate = function
    | Pos        -> Neg
    | PosZero -> NegZero
    | Neg        -> Pos
    | NegZero -> PosZero 
    | x          -> x   (* Zero, SignTop, SignBottom, NonZero invariati *)
end

module SimpleSigns = struct
  type t = SignTop | Pos | Zero | Neg | SignBottom
  
  let top    = SignTop
  let bottom = SignBottom

  let compare_type x y = match x,y with 
    | Zero,Zero -> 0
    | x,y when x = y -> 2
    | SignTop, _ -> 1
    | _,SignTop -> -1
    | SignBottom,_ -> -1
    | _,SignBottom -> 1
    | Pos,_ -> 1
    | _,Pos -> -1
    | Zero,_ -> 1
    | _,Zero -> -1
    | Neg,Neg -> 2

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x
    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | _,_ -> SignTop

  let widen x y = lub x y
  
  let glb s1 s2 = match s1, s2 with
  (* 1. Elemento Assorbente (Bottom) *)
  | SignBottom, _ | _, SignBottom -> SignBottom
  (* 2. Elemento Neutro (Top) *)
  | x, SignTop | SignTop, x -> x
  (* 3. Idempotenza (stesso elemento con se stesso) *)
  | x, y when x = y -> x
  (* 4. Tutti gli altri casi sono disgiunti (es. Pos con Neg, Zero con Pos, ecc.) *)
  | _, _ -> SignBottom

  let leq s1 s2 = (glb s1 s2) = s1

  let abstract_int n =
    if n > 0 then Pos
    else if n < 0 then Neg
    else Zero

  let abstract_range a b = 
    if a > b then SignBottom
  else if a < 0 && b > 0 then SignTop
  else lub (abstract_int a) (abstract_int b)

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _       | _, Zero      -> Zero
    | Pos, Pos | Neg,Neg -> Pos
    | Neg,Pos | Pos,Neg -> Neg
    | _,_ -> SignTop

  let sum s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom  -> SignBottom
    | x,y when x == y -> x
    | Zero, n       | n, Zero      -> n
    | _,_ -> SignTop
  
  (*Non del tutto corretta in quanto dovrebbe essere divisione intera*)
  let div s1 s2 = match s1, s2 with
    (*Errore/Irraggiungibile*)
    | SignBottom, _ | _, SignBottom -> SignBottom
    (* divisione per zero *)
    | _, Zero -> SignBottom
    | Zero,(Pos | Neg ) -> Zero
    (*Unici casi noti della tabella della divisione*)
    | Pos, Pos    | Neg, Neg     -> Pos
    | Pos, Neg    | Neg, Pos     -> Neg
    (*Casi con possibili divisioni per 0 oppure divisioni con NonZero*)
    | _ -> SignTop

  let negate = function
    | Pos        -> Neg
    | Neg        -> Pos
    | x          -> x   (* Zero, SignTop, SignBottom*)

end

(*Dominio degli Intervalli*)
module Intervals = struct
  type bound = NegInf | Int of int | PosInf 
  type t = Interval of bound * bound | Bottom
  
  let bottom = Bottom
  let top = Interval (NegInf,PosInf)

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
    | Interval(a,b),Interval(c,d) -> if compare_bound a c >= 0 && compare_bound d b >= 0 then 1 else -1 
    | Bottom,_ -> -1
    | _,Bottom -> 1
    
  let lub c1 c2 = match c1,c2 with 
    | Bottom,x | x,Bottom -> x
    | Interval(a,b), Interval(c,d) -> Interval(min_bound a c ,max_bound b d )

  let widen c1 c2 = failwith "not implemented" 

  let glb c1 c2 = match c1,c2 with
  | Bottom,_ | _,Bottom -> Bottom
  | Interval(a,b) , Interval(c,d) -> 
    let lo = max_bound a c in 
    let hi = min_bound b d in 
    if lo > hi then Bottom else Interval(lo, hi) 

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

  let abstract_int n = Interval (Int n,Int n)
  let abstract_range c1 c2 = 
    if c1 > c2 then Interval (Int c2, Int c1)
    else if c2 > c1 then Interval (Int c1, Int c2)
    else abstract_int c1

  let sum c1 c2 = match c1, c2 with
    |Bottom,_ | _,Bottom -> Bottom
    | Interval(a,b),Interval(c,d) -> Interval (add_bound a c,add_bound b d)
  
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

  