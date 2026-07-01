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
  type t = SignTop | Pos | Neg | Zero | SignBottom
  
  let top    = SignTop
  let bottom = SignBottom

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x
    | _                            -> SignTop

  let leq s1 s2 = match s1, s2 with
    | SignBottom, _                -> true
    | _, SignTop                   -> true
    | x, y                        -> x = y

  let abstract_int n =
    if n > 0 then Pos
    else if n < 0 then Neg
    else Zero

  let abstract_range a b =
    if a > 0 then Pos
    else if b < 0 then Neg
    else if a = 0 && b = 0 then Zero
    else SignTop

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _       | _, Zero      -> Zero
    | SignTop, _    | _, SignTop    -> SignTop
    | Pos, Pos      | Neg, Neg     -> Pos
    | Pos, Neg      | Neg, Pos     -> Neg

  let sum s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom  -> SignBottom
    | Zero, x       | x, Zero       -> x

    | SignTop, _    | _, SignTop
    | Pos, Neg      | Neg, Pos      -> SignTop
    
    | Pos, Pos                      -> Pos
    | Neg, Neg                      -> Neg

  let div s1 s2 = match s1, s2 with
    | _, Zero                        -> SignBottom  (* divisione per zero *)
    | SignBottom, _ | _, SignBottom   -> SignBottom
    | Zero, _                        -> Zero
    | Pos, Pos      | Neg, Neg       -> Pos
    | Pos, Neg      | Neg, Pos       -> Neg
    | SignTop, _    | _, SignTop      -> SignTop

  let negate = function
    | Pos        -> Neg
    | Neg        -> Pos
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

  