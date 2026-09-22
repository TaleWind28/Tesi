(* Firma del dominio astratto *)
open Syntax

module type NonRelationalDomain = sig
  type t
  val top    : t
  val bottom : t
  val lub    : t -> t -> t
  val leq    : t -> t -> bool
  val glb    : t-> t -> t
  val widen  : t -> t -> t 
  val narrow : t -> t -> t
  val compare_type : t -> t -> int
  val filter_rel : comparator -> t -> t

  val abstract_int   : int -> t
  val abstract_range : int -> int -> t
  val sum    : t -> t -> t
  val mul    : t -> t -> t
  val div    : t -> t -> t
  val negate : t -> t

  val to_string : t -> string
end

(* Dominio dei segni *)
module ExtendedSigns = struct
  type t = SignTop | Pos |PosZero | Zero | NegZero | Neg | NonZero | SignBottom

  let to_string t = match t with
  | SignTop -> "Top"
  | Pos -> ">0"
  | PosZero -> ">=0"
  | Zero -> "0"
  | NegZero -> "<=0"
  | Neg -> "<0"
  | NonZero -> "!=0"
  | SignBottom -> "Bottom"
  
  let top    = SignTop
  let bottom = SignBottom

  let filter_rel comp value = match comp,value with
  | Equals,value -> value 
  (* x != value *)
  | NotEquals, Zero -> NonZero
  | NotEquals, (Pos | Neg | NonZero | PosZero | NegZero| SignTop) -> SignTop


  (* x > value *)
  | Bigger,( Zero | PosZero | Pos) -> Pos
  | Bigger, (Neg | NegZero | NonZero | SignTop)  -> SignTop

  (* x >= value *)
  | BiggerEquals, Pos -> Pos
  | BiggerEquals, ( PosZero | Zero ) -> PosZero
  | BiggerEquals, (Neg | NegZero | NonZero | SignTop )  -> SignTop

  (* x < value *)
  | Smaller,( Neg | NegZero | Zero ) -> Neg
  | Smaller, (PosZero | Pos | NonZero | SignTop) -> SignTop

  (* x <= value *)
  | SmallerEquals, Neg -> Neg
  | SmallerEquals, ( NegZero | Zero) -> NegZero
  | SmallerEquals, (Pos | PosZero | NonZero | SignTop )  -> SignTop

  |_,SignBottom -> SignBottom

  let compare_type x y = match x,y with 
    | SignBottom,_ -> -1
    | _,SignBottom -> 1  
    
    | Zero,Zero -> 0
    | x,y when x = y -> 2
    | SignTop, _ | _,SignTop -> 2
    
    | NonZero,_ | _,NonZero -> 2
    | PosZero, (Zero | Pos | NegZero) | (Zero | Pos | NegZero), PosZero | NegZero,Neg | Neg,NegZero | NegZero, NegZero | Neg,Neg-> 2
    (* | Pos,PosZero -> 2
    | NegZero, PosZero | Zero,PosZero -> 2 *)
    | PosZero,_ -> 1
    | _,PosZero -> -1
    | Pos,_ -> 1
    | _,Pos -> -1
    | Zero,NegZero | NegZero,Zero -> 2
    | Zero,_ -> 1
    | _,Zero -> -1

    (* | NegZero,Neg | Neg,NegZero | NegZero, NegZero | Neg,Neg-> 2 *)

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | x, y when x = y              -> x

    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | NonZero, NonZero | NonZero,Pos | NonZero,Neg | Neg,Pos | Neg,NonZero | Pos,NonZero  | Pos,Neg -> NonZero
    | Zero, Pos | Pos,Zero | PosZero,Zero | Pos,PosZero | Zero,PosZero | PosZero,Pos -> PosZero
    | Zero, Neg | Neg, Zero | NegZero,Neg | Neg, NegZero | Zero,NegZero | NegZero,Zero -> NegZero
    | _,_ -> SignTop

  let glb s1 s2 = match s1,s2 with
  | _,SignBottom | SignBottom,_ -> SignBottom
  | x,SignTop | SignTop,x -> x
  | x,y when x = y -> x
  | NonZero,Pos | NonZero,PosZero| Pos, PosZero | Pos,NonZero | PosZero,NonZero | PosZero,Pos-> Pos
  | NonZero,Neg | NonZero,NegZero |Neg,NegZero | Neg,NonZero | NegZero,NonZero | NegZero,Neg -> Neg
  | Zero,PosZero | Zero, NegZero | PosZero, NegZero | PosZero, Zero | NegZero, Zero | NegZero, PosZero-> Zero
  | _,_ -> SignBottom

  let leq s1 s2 = (glb s1 s2) = s1

  let narrow x y = if leq y x then y else x

  let widen x y = lub x y

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

module SimplifiedSigns = struct
  type t = SignTop | Pos | Zero | Neg | SignBottom
  let to_string t = match t with
  | SignTop -> "Top"
  | Pos -> ">0"
  | Zero -> "0"
  | Neg -> "<0"
  | SignBottom -> "Bottom"
  let top    = SignTop
  let bottom = SignBottom
  let filter_rel op value = match op, value with
  (* 1. Caso base: se v2 è Bottom *)
  | _, SignBottom -> SignBottom

  (* 2. Uguaglianza *)
  | Equals, value -> value

  (* 3. Diversità (NotEquals: x <> v2) *)
  | NotEquals, Zero -> SignTop (* In SimpleSigns non c'è NonZero *)
  | NotEquals, _    -> SignTop

  (* 4. Maggiore Stretto (Bigger: x > v2) *)
  | Bigger, (Zero | Pos) -> Pos
  | Bigger, Neg          -> SignTop
  | Bigger, SignTop      -> SignTop

  (* 5. Maggiore o Uguale (BiggerEquals: x >= v2) *)
  | BiggerEquals, Pos     -> Pos     (* x >= Pos (es. x >= 5) => x dev'essere Pos *)
  | BiggerEquals, Zero    -> SignTop (* In SimpleSigns non c'è PosZero, quindi include Pos e Zero *)
  | BiggerEquals, Neg     -> SignTop
  | BiggerEquals, SignTop -> SignTop

  (* 6. Minore Stretto (Smaller: x < v2) *)
  | Smaller, (Zero | Neg) -> Neg
  | Smaller, Pos          -> SignTop
  | Smaller, SignTop      -> SignTop

  (* 7. Minore o Uguale (SmallerEquals: x <= v2) *)
  | SmallerEquals, Neg     -> Neg     (* x <= Neg (es. x <= -3) => x dev'essere Neg *)
  | SmallerEquals, Zero    -> SignTop (* In SimpleSigns non c'è NegZero, quindi include Neg e Zero *)
  | SmallerEquals, Pos     -> SignTop
  | SmallerEquals, SignTop -> SignTop
  let compare_type x y = match x,y with 
    | Zero,Zero -> 0
    | x,y when x = y -> 2
    | SignTop, _ -> 2
    | _,SignTop -> 2
    | SignBottom,_ -> -1
    | _,SignBottom -> 1
    | Pos,Pos -> 2
    | Pos,_ -> 1
    | _,Pos -> -1
    | Zero,_ -> 1
    | _,Zero -> -1
    | Neg,Neg -> 2
  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | Zero,Zero -> Zero
    | _,_ -> SignTop

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

  let narrow x y = if leq y x then y else x
  let widen x y = lub x y

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

module Signs = struct
  type t = SignTop | Pos | Neg | SignBottom
  let to_string t = match t with
  | SignTop -> "Top"
  | Pos -> ">0"
  | Neg -> "<0"
  | SignBottom -> "Bottom"
  let top    = SignTop
  let bottom = SignBottom
  let filter_rel op value = match op, value with
  (* 1. Caso base: se v2 è Bottom *)
  | _, SignBottom -> SignBottom
  (* 2. Uguaglianza *)
  | Equals, value -> value
  (* 3. Diversità (NotEquals: x <> v2) *)
  | NotEquals, _    -> SignTop
  (* 4. Maggiore Stretto (Bigger: x > v2) *)
  | Bigger, Pos -> Pos
  | Bigger, Neg          -> SignTop
  | Bigger, SignTop      -> SignTop
  (* 5. Maggiore o Uguale (BiggerEquals: x >= v2) *)
  | BiggerEquals, Pos     -> Pos     (* x >= Pos (es. x >= 5) => x dev'essere Pos *)
  | BiggerEquals, Neg     -> SignTop
  | BiggerEquals, SignTop -> SignTop
  (* 6. Minore Stretto (Smaller: x < v2) *)
  | Smaller, Neg -> Neg
  | Smaller, Pos          -> SignTop
  | Smaller, SignTop      -> SignTop
  (* 7. Minore o Uguale (SmallerEquals: x <= v2) *)
  | SmallerEquals, Neg     -> Neg     (* x <= Neg (es. x <= -3) => x dev'essere Neg *)
  | SmallerEquals, Pos     -> SignTop
  | SmallerEquals, SignTop -> SignTop

  let compare_type x y = match x,y with 
    | x,y when x = y -> 2
    | SignTop, _  | _,SignTop | Pos,Pos | Neg,Neg  -> 2
    | SignBottom,_ -> -1
    | _,SignBottom -> 1
    | Pos,_ -> 1
    | _,Pos -> -1

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | Neg, Neg -> Neg
    | Pos, Pos -> Pos
    | _,_ -> SignTop


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

  let narrow x y = if leq y x then y else x
  let widen x y = lub x y

  let abstract_int n =
    if n > 0 then Pos
    else if n < 0 then Neg
    else SignTop

  let abstract_range a b = 
    if a > b then SignBottom
  else if a < 0 && b > 0 then SignTop
  else lub (abstract_int a) (abstract_int b)

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Pos, Pos | Neg,Neg -> Pos
    | Neg,Pos | Pos,Neg -> Neg
    | _,_ -> SignTop

  let sum s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom  -> SignBottom
    | x,y when x == y -> x
    | _,_ -> SignTop
  
  (*Non del tutto corretta in quanto dovrebbe essere divisione intera*)
  let div s1 s2 = match s1, s2 with
    (*Errore/Irraggiungibile*)
    | SignBottom, _ | _, SignBottom -> SignBottom
    (*Unici casi noti della tabella della divisione*)
    | Pos, Pos    | Neg, Neg     -> Pos
    | Pos, Neg    | Neg, Pos     -> Neg
    (*Casi con possibili divisioni per 0 oppure divisioni con NonZero*)
    | _ -> SignTop

  let negate = function
    | Pos        -> Neg
    | Neg        -> Pos
    | x          -> x   (* SignTop, SignBottom *)

end

module SimpleSigns = struct (* a regola è questo SimpleSigns però bisogna controllare meglio*)
  type t = SignTop | PosZero | Zero | NegZero | SignBottom
  let to_string t = match t with
  | SignTop -> "Top"
  | PosZero -> ">=0"
  | Zero -> "0"
  | NegZero -> "<=0"
  | SignBottom -> "Bottom"
  
  let top    = SignTop
  let bottom = SignBottom

  let filter_rel op value = match op, value with
  (* 1. Caso base: se v2 è Bottom *)
  | _, SignBottom -> SignBottom

  (* 2. Uguaglianza *)
  | Equals, value -> value

  (* 3. Diversità (NotEquals: x <> v2) *)
  | NotEquals, Zero -> SignTop (* In SimpleSigns non c'è NonZero *)
  | NotEquals, _    -> SignTop

  (* 4. Maggiore Stretto (Bigger: x > v2) *)
  | Bigger, (Zero | PosZero) -> PosZero
  | Bigger, NegZero          -> SignTop
  | Bigger, SignTop      -> SignTop

  (* 5. Maggiore o Uguale (BiggerEquals: x >= v2) *)
  | BiggerEquals, PosZero     -> PosZero     (* x >= Pos (es. x >= 5) => x dev'essere Pos *)
  | BiggerEquals, Zero    -> PosZero (* In SimpleSigns non c'è PosZero, quindi include Pos e Zero *)
  | BiggerEquals, NegZero     -> SignTop
  | BiggerEquals, SignTop -> SignTop

  (* 6. Minore Stretto (Smaller: x < v2) *)
  | Smaller, (Zero | NegZero) -> NegZero  
  | Smaller, PosZero          -> SignTop
  | Smaller, SignTop      -> SignTop

  (* 7. Minore o Uguale (SmallerEquals: x <= v2) *)
  | SmallerEquals, NegZero     -> NegZero     (* x <= Neg (es. x <= -3) => x dev'essere Neg *)
  | SmallerEquals, Zero    -> NegZero (* In SimpleSigns non c'è NegZero, quindi include Neg e Zero *)
  | SmallerEquals, PosZero     -> SignTop
  | SmallerEquals, SignTop -> SignTop

  let compare_type x y = match x,y with 
    | Zero,Zero -> 0
    | SignTop, _ | _,SignTop | PosZero,PosZero | NegZero,NegZero | PosZero,Zero | Zero,PosZero | NegZero,Zero -> 2
    | SignBottom,_ | NegZero,PosZero -> -1
    | _,SignBottom | PosZero,_  | Zero,_ -> 1
  
  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | NegZero,Zero | NegZero, NegZero | Zero,NegZero -> NegZero
    | PosZero,Zero | PosZero, PosZero | Zero,PosZero -> PosZero
    | Zero,Zero -> Zero
    | _,_ -> SignTop
  
  let glb s1 s2 = match s1, s2 with
  (* 1. Elemento Assorbente (Bottom) *)
  | SignBottom, _ | _, SignBottom -> SignBottom
  (* 2. Elemento Neutro (Top) *)
  | x, SignTop | SignTop, x -> x
  (* 3. Idempotenza (stesso elemento con se stesso) *)
  | x, y when x = y -> x
  | PosZero,Zero | Zero,PosZero | NegZero,Zero | Zero,NegZero | PosZero, NegZero | NegZero,PosZero -> Zero
  (* 4. Tutti gli altri casi sono disgiunti (es. Pos con Neg, Zero con Pos, ecc.) *)
  | _, _ -> SignBottom

  let leq s1 s2 = (glb s1 s2) = s1

  let narrow x y = if leq y x then y else x
  let widen x y = lub x y

  let abstract_int n =
    if n > 0 then PosZero
    else if n < 0 then NegZero
    else Zero

  let abstract_range a b = 
    if a > b then SignBottom
  else if a < 0 && b > 0 then SignTop
  else lub (abstract_int a) (abstract_int b)

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _       | _, Zero       -> Zero
    | PosZero, PosZero | NegZero,NegZero -> PosZero
    | NegZero,PosZero | PosZero,NegZero -> NegZero
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
    | Zero,(PosZero | NegZero ) -> Zero
    (*Unici casi noti della tabella della divisione*)
    | PosZero, PosZero    | NegZero, NegZero     -> PosZero
    | PosZero, NegZero    | NegZero, PosZero     -> NegZero
    (*Casi con possibili divisioni per 0 oppure divisioni con NonZero*)
    | _ -> SignTop

  let negate = function
    | PosZero        -> NegZero
    | NegZero        -> PosZero
    | x          -> x   (* Zero, SignTop, SignBottom*)

end

module StrangeSigns = struct 
  type t = SignTop | PosZero | Zero | Neg | SignBottom

  let to_string t = match t with
  | SignTop -> "Top"
  | PosZero -> ">=0"
  | Zero -> "0"
  | Neg -> "<0"
  | SignBottom -> "Bottom"
  
  let top    = SignTop
  let bottom = SignBottom

  let filter_rel op value = match op, value with
  (* 1. Caso base: se v2 è Bottom *)
  | _, SignBottom -> SignBottom

  (* 2. Uguaglianza *)
  | Equals, value -> value

  (* 3. Diversità (NotEquals: x <> v2) *)
  | NotEquals, _    -> SignTop

  (* 4. Maggiore Stretto (Bigger: x > v2) *)
  | Bigger, (Zero | PosZero) -> PosZero
  | Bigger, Neg          -> SignTop
  | Bigger, SignTop      -> SignTop

  (* 5. Maggiore o Uguale (BiggerEquals: x >= v2) *)
  | BiggerEquals, (Zero | PosZero)    -> PosZero     (* x >= Pos (es. x >= 5) => x dev'essere Pos *)
  | BiggerEquals, (Neg | SignTop)     -> SignTop

  (* 6. Minore Stretto (Smaller: x < v2) *)
  | Smaller, (Zero | Neg) -> Neg  
  | Smaller, (PosZero | SignTop) -> SignTop
  
  (* 7. Minore o Uguale (SmallerEquals: x <= v2) *)
  | SmallerEquals, Neg     -> Neg     (* x <= Neg (es. x <= -3) => x dev'essere Neg *)
  | SmallerEquals, Zero     -> SignTop (* x <= 0 -> Neg, Zero o PosZero *)
  | SmallerEquals, ( PosZero | SignTop )   -> SignTop
  let compare_type x y = match x,y with 
    | Zero,Zero -> 0
    | SignTop, _ | _,SignTop | PosZero,PosZero | Neg,Neg | PosZero,Zero | Zero,PosZero -> 2
    | SignBottom,_ -> -1
    | _,SignBottom -> 1
    | PosZero,_ -> 1
    | _,PosZero -> -1
    | Zero,_ -> 1
    | _,Zero -> -1

  let lub s1 s2 = match s1, s2 with
    | SignBottom, x | x, SignBottom -> x
    | Neg, Neg -> Neg
    | PosZero, PosZero -> PosZero
    | Zero,Zero -> Zero
    | PosZero,Zero | Zero,PosZero -> PosZero
    | _,_ -> SignTop


  let glb s1 s2 = match s1, s2 with
  (* 1. Elemento Assorbente (Bottom) *)
  | SignBottom, _ | _, SignBottom -> SignBottom
  (* 2. Elemento Neutro (Top) *)
  | x, SignTop | SignTop, x -> x
  (* 3. Idempotenza (stesso elemento con se stesso) *)
  | x, y when x = y -> x
  | PosZero,Zero | Zero,PosZero -> Zero
  (* 4. Tutti gli altri casi sono disgiunti (es. PosZero con Neg, Zero con Neg, ecc.) *)
  | _, _ -> SignBottom

  let leq s1 s2 = (glb s1 s2) = s1

  let narrow x y = if leq y x then y else x
  let widen x y = lub x y

  let abstract_int n =
    if n > 0 then PosZero
    else if n < 0 then Neg
    else Zero

  let abstract_range a b = 
    if a > b then SignBottom
  else if a < 0 && b > 0 then SignTop
  else lub (abstract_int a) (abstract_int b)

  let mul s1 s2 = match s1, s2 with
    | SignBottom, _ | _, SignBottom -> SignBottom
    | Zero, _ | _, Zero      -> Zero
    | PosZero, PosZero | Neg,Neg -> PosZero
    | Neg,PosZero | PosZero,Neg -> SignTop
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
    | Zero, _ -> Zero
    (* divisione per zero *)
    | _, Zero -> SignBottom
    | PosZero, Neg -> SignTop
    (*Unici casi noti della tabella della divisione*)
    | PosZero,PosZero | Neg, Neg -> PosZero
    (*Casi con possibili divisioni per 0 oppure divisioni con NonZero*)
    | _ -> SignTop

  let negate = function
    | PosZero    -> top
    | Neg        -> PosZero
    | x          -> x   (* Zero, SignTop, SignBottom*)

end

(*Dominio degli Intervalli*)
module Intervals = struct
  include Shared_arithmetic.IntervalArith
  type t = value

  let abstract_int n = Interval (Int n,Int n)

  let abstract_range c1 c2 = 
  if c1 > c2 then Interval (Int c2, Int c1)
  else if c2 > c1 then Interval (Int c1, Int c2)
  else abstract_int c1
  let bottom = Bottom
  let top = Interval (NegInf,PosInf)

  let filter_rel comp value = match value with
  | Bottom -> Bottom
  | Interval(l,u) ->
    match comp with 
    | Equals -> Interval(l,u)
    | NotEquals -> top 
    | Bigger -> Interval(next_bound l,PosInf) 
    | BiggerEquals -> Interval(l,PosInf)
    | Smaller ->  Interval(NegInf ,(prev_bound u))
    | SmallerEquals -> Interval(NegInf,u)
end 

(* Domini Relazionali *)
module type WeakRelationalDomain = sig
  type t (*done*)

  type value (*done*) 
  val bottom : t (*done*)
  val init : ide list -> t (*done*)
  val is_bottom : t -> bool (*done*)
  val normalize : t -> t (*done*)
  val leq : t -> t -> bool (*done*)
  val lub : t -> t -> t (*done*)
  val glb : t -> t -> t (*done*)
  val widen : t -> t -> t 
  val narrow : t -> t -> t

  val compare_type :  value -> value -> t -> int

  val retrieve_variable : ide -> t -> value

  (** {4 Funzioni di Trasferimento} *)

  (** Assegnamento astratto: aggiorna la DBM a seguito dell'istruzione x := e.
      Gestisce sia assegnamenti esatti (costanti, traslazioni x := x + c)
      sia assegnamenti affini approssimati tramite intervalli *)
  val assign_const : ide -> int -> t -> t (*done*)

  val assign : ide -> value -> t -> t 
  val assign_var : ide -> sign -> ide -> int -> t -> t  

  val shift_var : ide -> int -> t -> t (*done*)

  (** Forget / Reset: rimuove tutti i vincoli che coinvolgono la variabile x.
      Richiede la chiusura preventiva della DBM prima di impostare riga e colonna a +infinity *)
  val forget : ide -> t -> t (*done*)

  (** Filtro atomico sulle condizioni: raffina la DBM applicando la guardia c
      (es. vincoli di differenza Vj - Vi <= c o guardie unarie Vi <= c) *)
  val filter_atom : rel_atom -> t -> t

  val abstract_int   : int -> value
  val abstract_range : int -> int -> value

  val sum    : value -> value -> value
  val mul    : value -> value -> value
  val div    : value -> value -> value
  val negate : value -> value

  val resolve_index : (ide * int)list  -> ide -> int

  val unpack_value : value -> bool -> int option

  val to_string : t -> string
  val string_of_value : value -> string
end

module Zones : WeakRelationalDomain = struct
  include Shared_arithmetic.IntervalArith
  include Shared_arithmetic.DBMOperations

  (** {2 Struttura DBM e Inizializzazione} *)
  let bottom = Bottom

  let is_bottom env = match env with
    | Bottom -> true
    | _ -> false

  let init (vars : ide list) : t =
    let xs = List.sort_uniq compare vars in
    let n = List.length xs in
    let env = List.mapi (fun i x -> (x, i + 1)) xs in
    let dim = n + 1 in
    let matrix = Array.make_matrix dim dim PosInf in
    for i = 0 to dim - 1 do
      matrix.(i).(i) <- Int 0
    done;
    create_type_dbm n env matrix

  (** {2 Chiusura e Normalizzazione DBM} *)

  let close_dbm env = match env with
    | Bottom -> Bottom
    | Env dbm ->
      let computated_matrix = floyd_wharshall dbm.matrix (dbm.n + 1) in 
      if has_neg_cycle computated_matrix (dbm.n + 1) 0 then Bottom
      else create_type_dbm dbm.n dbm.env computated_matrix 

  let normalize dbm = close_dbm dbm 

  (** {2 Operazioni di Reticolo} *)

  let leq m n = 
    match normalize m, n with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Env m1, Env n1 -> leq_matrix m1 n1 
  let lub m n = 
    match normalize m, normalize n with
    | Bottom, Env e | Env e, Bottom -> Env e
    | Bottom, Bottom -> Bottom
    | Env m1, Env n1 ->
      let minmat = lub_matrix m1 n1 in
      close_dbm (create_type_dbm m1.n m1.env minmat)
  let glb m n = 
    match m, n with
    | Bottom, _ | _, Bottom -> Bottom
    | Env m1, Env n1 ->
      let maxmat = glb_matrix m1 n1 
      in
      close_dbm (create_type_dbm m1.n m1.env maxmat)

  let widen m n = 
    match normalize m, normalize n with
    | Bottom, Bottom -> Bottom 
    | Bottom, Env e | Env e, Bottom -> Env e
    | Env m1, Env n1 -> 
      let widen_mat = widen_matrix m1 n1 in
      close_dbm (create_type_dbm m1.n m1.env widen_mat)

  let narrow m n = 
    match normalize m, normalize n with
    | Bottom, _ -> Bottom
    | x, Bottom -> x
    | Env m1, Env n1 -> 
      let narrow_mat = narrow_matrix m1 n1  in
      close_dbm (create_type_dbm m1.n m1.env narrow_mat)

  (** {3 Confronti e Proiezioni} *)

  let compare_type b1 b2 env = Shared_arithmetic.IntervalArith.compare_type b1 b2 

  let retrieve_variable ide env = match env with
    | Bottom -> Shared_arithmetic.IntervalArith.Bottom
    | Env dbm -> 
      let idx = resolve_index dbm.env ide in
      let lo  = neg_bound (dbm.matrix.(0).(idx)) in
      let hi = dbm.matrix.(idx).(0) in
      Interval (lo, hi)

  (** {2 Funzioni di Trasferimento: Assegnamenti e Filtri} *)

  let forget ide env =  
    match close_dbm env with
    | Bottom -> Bottom
    | Env dbm -> 
      let i = resolve_index dbm.env ide in 
      for k = 0 to dbm.n do
        if k <> i then begin 
          dbm.matrix.(i).(k) <- PosInf;
          dbm.matrix.(k).(i) <- PosInf;
        end
      done;
      create_type_dbm dbm.n dbm.env dbm.matrix

  let filter_atom rel env = 
    match rel, env with
    | _, Bottom -> Bottom
    (* Vincolo Unario: x <= c *)
    | Unary (Pos, x, c), Env dbm ->
      let i = resolve_index dbm.env x in 
      let new_m = copy_matrix dbm.matrix in
      new_m.(i).(0) <- min_bound new_m.(i).(0) (Int c); 
      close_dbm (create_type_dbm dbm.n dbm.env new_m) 
    (* Vincolo Unario: -x <= c *)
    | Unary (Neg, x, c), Env dbm ->
      let i = resolve_index dbm.env x in 
      let new_m = copy_matrix dbm.matrix in
      new_m.(0).(i) <- min_bound new_m.(0).(i) (Int c); 
      close_dbm (create_type_dbm dbm.n dbm.env new_m)
    (* Vincolo Binario: x - y <= c *)
    | Binary (Pos, x, Neg, y, c), Env dbm ->
      let i = resolve_index dbm.env x in 
      let j = resolve_index dbm.env y in 
      let new_m = copy_matrix dbm.matrix in
      new_m.(i).(j) <- min_bound new_m.(i).(j) (Int c); 
      close_dbm (create_type_dbm dbm.n dbm.env new_m)
    (* Vincolo Binario: y - x <= c *)
    | Binary (Neg, x, Pos, y, c), Env dbm -> 
      let i = resolve_index dbm.env x in 
      let j = resolve_index dbm.env y in 
      let new_m = copy_matrix dbm.matrix in
      new_m.(j).(i) <- min_bound new_m.(j).(i) (Int c); 
      close_dbm (create_type_dbm dbm.n dbm.env new_m)
    (* Somme concordi: Safe over-approximation (le Zone non le supportano) *)
    | Binary (Pos, _, Pos, _, _), Env dbm -> Env dbm
    | Binary (Neg, _, Neg, _, _), Env dbm -> Env dbm

  let assign ide value env = match env, value with
    | Bottom, _ | _, Shared_arithmetic.IntervalArith.Bottom -> Bottom
    | Env dbm, Interval (lo, hi) -> 
      let i = resolve_index dbm.env ide in 
      match forget ide env with
      | Bottom -> Bottom
      | Env dbm' ->
        dbm'.matrix.(i).(0) <- hi;          (* x <= hi *)
        dbm'.matrix.(0).(i) <- neg_bound lo; (* v0 - x <= -lo, cioè x >= lo *)
        close_dbm (Env dbm')

  let assign_const ide c env = assign ide (abstract_int c) env

  let shift_var ide c env = match env with
    | Bottom -> Bottom
    | Env dbm ->
      let new_m = copy_matrix dbm.matrix in
      let i = resolve_index dbm.env ide in 
      for k = 0 to dbm.n do 
        if k <> i then begin 
          new_m.(i).(k) <- add_bound new_m.(i).(k) (Int c);
          new_m.(k).(i) <- add_bound new_m.(k).(i) (Int (-c));
        end
      done;
      create_type_dbm dbm.n dbm.env new_m

  let assign_var x sign y c env = 
    match sign, env with
    | Pos, Env dbm -> 
      if x = y then 
        (* x := x + c *)
        shift_var x c env
      else
        (* x := y + c *)
        let i = resolve_index dbm.env x in 
        let j = resolve_index dbm.env y in
        (match forget x env with
        | Bottom -> Bottom
        | Env dbm' -> 
          let new_m = copy_matrix dbm'.matrix in 
          new_m.(i).(j) <- Int c;
          new_m.(j).(i) <- Int (-c);
          close_dbm (create_type_dbm dbm'.n dbm'.env new_m))
    | Neg, Env dbm -> 
      (* x := -y + c approssimato tramite intervalli *)
      let y' = retrieve_variable y env in 
      let new_val = sum (negate y') (abstract_int c) in 
      assign x new_val env
    | _, Bottom -> Bottom

  (** {4 Costruzione e Scomposizione Valori Astratti} *)

  let abstract_int x = Interval (Int x, Int x)

  let abstract_range x y = 
    if x > y then Interval (Int y, Int x) 
    else Interval (Int x, Int y)

  let unpack_value (value : value) (flag : bool) : int option = 
    match value with
    | Bottom -> None
    | Interval (lo, hi) -> 
      let b = if flag then hi else lo in
      match b with
      | Int x -> Some x
      | PosInf | NegInf -> None

  (** {5 Pretty Printing} *)

  let to_string t = match t with
    | Bottom -> "Bottom"
    | Env dbm ->
      let idx_to_name i =
        if i = 0 then "v0"
        else
          match List.find_opt (fun (_, idx) -> idx = i) dbm.env with
          | Some (ide, _) -> ide
          | None -> "?"
      in
      let names = List.init (dbm.n + 1) idx_to_name in
      let header = "\t" ^ String.concat "\t" names in
      let rows =
        List.init (dbm.n + 1) (fun i ->
          let row_cells =
            List.init (dbm.n + 1) (fun j -> bound_to_string dbm.matrix.(i).(j))
          in
          idx_to_name i ^ "\t" ^ String.concat "\t" row_cells)
      in
      header ^ "\n" ^ String.concat "\n" rows

  let string_of_value valore = Shared_arithmetic.IntervalArith.to_string valore
end

module Octagons = struct 
  include Shared_arithmetic.IntervalArith
  include Shared_arithmetic.DBMOperations
  let bottom = Bottom
  let is_bottom env = 
    match env with
    |Bottom -> true
    |_ -> false
  let init (vars : ide list) : t =
    let xs = List.sort_uniq compare vars in
    let n = List.length xs in
    let env = List.mapi (fun i x -> (x, i)) xs in
    let dim = n * 2 in
    let matrix = Array.make_matrix dim dim PosInf in
    for i = 0 to dim - 1 do
      matrix.(i).(i) <- Int 0
    done;
    create_type_dbm n env matrix
  let has_inconsistency m dim =
    let rec check k = 
    if k >= dim/2 then false
    else
      let pos = 2*k in 
      let neg = pos+1 in
      match m.(pos).(neg), m.(neg).(pos) with
      | Int pos',Int neg' -> if (pos' asr 1) + (neg' asr 1) < 0 then true else check (k+1)
      | _ -> check (k+1)
    in check 0
  let dual i = if i mod 2 = 0 then i+1 else i-1
  let div_2_bound = function
  | Int x -> Int (x asr 1)
  | b -> b
  let strenghten_elements m dim = 
    let res = copy_matrix m in 
    for i = 0 to dim -1 do 
      for j = 0 to dim -1 do 
        let mii = m.(i).(dual i) in 
        let mjj = m.(dual j).(j) in 
        match add_bound mii mjj with
        | Int s -> 
          res.(i).(j) <- min_bound (res.(i).(j)) (div_2_bound (Int(s)))
        | _ -> ()
        done;
      done;
    res
  let strong_closure env = match env with
  | Bottom -> Bottom
  | Env dbm -> 
    let dim = 2 * dbm.n in 
    let m' = floyd_wharshall dbm.matrix dim in 
    if has_neg_cycle m' dim 0 ||  has_inconsistency m' dim then Bottom 
    else 
      let m3 = floyd_wharshall (strenghten_elements m' dim) dim in 
      if has_neg_cycle m3 dim 0 then Bottom 
      else create_type_dbm dbm.n dbm.env m3
  let normalize env = strong_closure env
  let lub m n = match m,n  with
  | Bottom,x | x,Bottom -> x
  | Env m1, Env n1  -> strong_closure(create_type_dbm m1.n m1.env (lub_matrix m1 n1))  
  let glb m n = match strong_closure m,strong_closure n with
  | Bottom,_ | _,Bottom -> Bottom
  | Env m1, Env n1 -> create_type_dbm m1.n m1.env (glb_matrix  m1 n1)
  let leq m n =  match strong_closure m, n with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Env m1, Env n1 -> leq_matrix m1 n1 
  let widen m n = 
    match normalize m, normalize n with
    | Bottom, Bottom -> Bottom 
    | Bottom, Env e | Env e, Bottom -> Env e
    | Env m1, Env n1 -> 
      let widen_mat = widen_matrix m1 n1 in
      strong_closure (create_type_dbm m1.n m1.env widen_mat)
  let narrow m n = 
    match normalize m, normalize n with
    | Bottom, _ -> Bottom
    | x, Bottom -> x
    | Env m1, Env n1 -> 
      let narrow_mat = narrow_matrix m1 n1  in
      strong_closure (create_type_dbm m1.n m1.env narrow_mat)
  let compare_type b1 b2 env = Shared_arithmetic.IntervalArith.compare_type b1 b2 
 
  (** {4 Funzioni di Trasferimento} *)

  let retrieve_variable id env = 
    match normalize env with
    | Bottom -> Shared_arithmetic.IntervalArith.Bottom
    | Env dbm -> 
      let k = resolve_index dbm.env id in 
      let pos = 2*k in 
      let neg = pos +1 in
      let hi = div_2_bound(dbm.matrix.(pos).(neg)) in 
      let lo = neg_bound (div_2_bound(dbm.matrix.(neg).(pos))) in 
      Interval(lo,hi)

  (* val forget : ide -> t -> t  *)
  let forget id env = 
    match strong_closure env with
    | Bottom -> Bottom
    | Env dbm -> 
      let k = resolve_index dbm.env id in 
      let pos = 2*k in 
      let neg = pos +1 in 
      let dim = dbm.n * 2 in 
      let new_m = copy_matrix dbm.matrix in 
      for c = 0 to dim -1 do 
        if c <> pos then begin 
          new_m.(pos).(c) <- PosInf;
          new_m.(c).(pos) <- PosInf;
        end;
        if c <> neg then begin 
          new_m.(neg).(c) <- PosInf;
          new_m.(c).(neg) <- PosInf;
        end;
      done;
      create_type_dbm dbm.n dbm.env new_m

  (** Assegnamento astratto: aggiorna la DBM a seguito dell'istruzione x := e.
      Gestisce sia assegnamenti esatti (costanti, traslazioni x := x + c)
      sia assegnamenti affini approssimati tramite intervalli *)
  (* val assign_const : ide -> int -> t -> t 
  val assign : ide -> value -> t -> t 
  val assign_var : ide -> sign -> ide -> int -> t -> t  

  val shift_var : ide -> int -> t -> t  *)
  

  (** Filtro atomico sulle condizioni: raffina la DBM applicando la guardia c
      (es. vincoli di differenza Vj - Vi <= c o guardie unarie Vi <= c) *)
  (* val filter_atom : rel_atom -> t -> t

  val resolve_index : (ide * int)list  -> ide -> int

  val unpack_value : value -> bool -> int option

  val to_string : t -> string *)
  let string_of_value value = Shared_arithmetic.IntervalArith.to_string value
end