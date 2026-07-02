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
  type t = SignTop | Pos |PosZero | Zero | NegZero | Neg | NonZero | SignBottom
  
  let top    = SignTop
  let bottom = SignBottom

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x
    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | NonZero, NonZero | NonZero,Pos | NonZero,Neg | Neg,NonZero | Pos,NonZero | Neg,Pos | Pos,Neg -> NonZero
    | Zero, Pos | Pos,Zero | PosZero,Zero | Pos,PosZero  -> PosZero
    | Zero, Neg | Neg, Zero | NegZero,Neg | Neg, NegZero -> NegZero
    | _,_ -> SignTop

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
    | _, Zero       | _, PosZero | _,NegZero                 -> SignBottom  (* divisione per zero *)
    | SignBottom, _ | _, SignBottom   -> SignBottom
    | Zero, _                        -> Zero
    | PosZero, _ -> PosZero
    | NegZero, _ -> NegZero
    | Pos, Pos      | Neg, Neg       -> Pos
    | Pos, Neg      | Neg, Pos       -> Neg
    | NonZero, _ | _, NonZero  -> NonZero
    | SignTop, _    | _, SignTop      -> SignTop

  let negate = function
    | Pos        -> Neg
    | PosZero -> NegZero
    | Neg        -> Pos
    | NegZero -> PosZero 
    | x          -> x   (* Zero, SignTop, SignBottom, NonZero invariati *)
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

  