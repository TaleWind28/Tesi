(* Firma del dominio astratto *)
module type DOMAIN = sig
  type t
  val top    : t
  val bottom : t
  val lub    : t -> t -> t
  val leq    : t -> t -> bool

  val abstract_int   : int -> t
  val abstract_range : int -> int -> t
  val sum    : t -> t -> t
  val mul    : t -> t -> t
  val div    : t -> t -> t
  val negate : t -> t
end

(* Dominio dei segni *)
module Signs = struct
  type t = SignTop | Pos |PosZero | Neg | NegZero | Zero | SignBottom
  
  let top    = SignTop
  let bottom = SignBottom

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x
    | Neg,Pos | Pos,Neg | PosZero, NegZero | NegZero,PosZero | Pos,NegZero | NegZero,Pos | PosZero,Neg | Neg,PosZero | SignTop,_ | _,SignTop                            -> SignTop
    | Neg, Neg -> Neg
    | Pos,Pos -> Pos
    | PosZero,PosZero | Pos,PosZero | PosZero, Pos | Pos,Zero | Zero, Pos | PosZero, Zero  | Zero, PosZero -> PosZero
    | NegZero,NegZero | NegZero,Neg | Neg,NegZero | Neg,Zero | Zero, Neg | NegZero, Zero  | Zero, NegZero -> NegZero
    | Zero,Zero -> Zero

  let leq s1 s2 = match s1, s2 with
    | SignBottom, _                -> true
    | _, SignTop                   -> true
    | x, y                        -> x = y

  let abstract_int n =
    if n > 0 then Pos
    else if n < 0 then Neg
    else Zero

  let abstract_range a b = 
    if a > b then SignBottom
    else lub (abstract_int a) (abstract_int b)

  (*| SignBottom,_ | _, SignBottom -> SignBottom
  | Pos,Pos -> Pos
  | Neg,Neg -> Neg
  | Zero,Zero -> Zero
  | Zero, Pos | Pos, Zero | PosZero,Pos | Pos,PosZero | Zero,PosZero | PosZero,Zero | PosZero,PosZero -> PosZero
  | Zero, Neg | Neg, Zero | NegZero,Neg | Neg,NegZero | NegZero,Zero | Zero,NegZero| NegZero,NegZero-> NegZero
  | _,_ -> SignTop *)
  (*| Pos,Neg | Neg,Pos | Pos,NegZero | NegZero,Pos | PosZero,Neg | Neg,PosZero | NegZero,PosZero | PosZero,NegZero -> SignTop*)
  
  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _       | _, Zero      -> Zero
    | SignTop, _    | _, SignTop  -> SignTop
    | Pos, Pos      | Neg, Neg     -> Pos
    | NegZero, NegZero  | PosZero,PosZero | Neg,NegZero| NegZero,Neg | Pos,PosZero | PosZero,Pos -> PosZero
    | NegZero, PosZero  | PosZero,NegZero | Pos,NegZero| NegZero,Pos | Neg,PosZero | PosZero,Neg-> NegZero
    | Pos, Neg      | Neg, Pos     -> Neg

  let sum s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom  -> SignBottom
    | Zero, n       | n, Zero      -> n
    | Pos, Pos                      -> Pos
    | PosZero, PosZero | PosZero,Pos | Pos,PosZero  -> PosZero
    | NegZero, NegZero | NegZero,Neg | Neg,NegZero  -> NegZero
    | Neg, Neg                      -> Neg
    | SignTop, _    | _, SignTop
    | Pos, Neg      | Neg, Pos | PosZero,NegZero | NegZero,PosZero | Pos,NegZero | NegZero,Pos | Neg,PosZero | PosZero,Neg     -> SignTop
    

  let div s1 s2 = match s1, s2 with
    | _, Zero       | _, PosZero | _,NegZero                 -> SignBottom  (* divisione per zero *)
    | SignBottom, _ | _, SignBottom   -> SignBottom
    | Zero, _                        -> Zero
    | PosZero, _ -> PosZero
    | NegZero, _ -> NegZero
    | Pos, Pos      | Neg, Neg       -> Pos
    | Pos, Neg      | Neg, Pos       -> Neg
    | SignTop, _    | _, SignTop      -> SignTop

  let negate = function
    | Pos        -> Neg
    | PosZero -> NegZero
    | Neg        -> Pos
    | NegZero -> PosZero 
    | x          -> x   (* Zero, SignTop, SignBottom invariati *)
end

(*Dominio degli Intervalli*)
module Intervals = struct
  type bound = Int of int | PosInf | NegInf
  type t = 
    |Interval of bound * bound
    |Bottom
  
  let bottom = Bottom
  let top = Interval (NegInf,PosInf)
  
  let lub  c1 c2 = failwith "not Implemented"
  let leq c1 c2 = failwith "not Implemented"

  (*Helper*)
  let add_bound a b = match a,b with 
    | PosInf,NegInf | NegInf,PosInf -> PosInf (*dovrebbe dare bottom*)
    | PosInf,_ | _,PosInf -> PosInf
    | NegInf,_ | _,NegInf -> NegInf
    | Int a, Int b -> Int (a+b)
  
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

  let mul c1 c2 = failwith "not implemented"
  let div c1 c2 = failwith "not implemented"
end

  