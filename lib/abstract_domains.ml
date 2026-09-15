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

  val retrieve_variable : ide -> t -> value

  (** {4 Funzioni di Trasferimento} *)

  (** Assegnamento astratto: aggiorna la DBM a seguito dell'istruzione x := e.
      Gestisce sia assegnamenti esatti (costanti, traslazioni x := x + c)
      sia assegnamenti affini approssimati tramite intervalli *)
  val assign_const : int -> int -> t -> t (*done*)
  val assign_var_offset : int -> int -> int -> t -> t (*done*)

  val shift_var : int -> int -> t -> t (*done*)

  (** Forget / Reset: rimuove tutti i vincoli che coinvolgono la variabile x.
      Richiede la chiusura preventiva della DBM prima di impostare riga e colonna a +infinity *)
  val forget : int -> t -> t (*done*)

  (** Filtro condizionale: raffina la DBM applicando la guardia c
      (es. vincoli di differenza Vj - Vi <= c o guardie unari Vi <= c) *)
  val filter_rel : int -> int -> int -> t -> t (*done*)

  val abstract_int   : int -> value
  val abstract_range : int -> int -> value

  val sum    : value -> value -> value
  val mul    : value -> value -> value
  val div    : value -> value -> value
  val negate : value -> value

  val to_string : t -> string
  val string_of_value : value -> string
  val print : t -> unit
end

module Zones : WeakRelationalDomain = struct
  include Shared_arithmetic.IntervalArith
  type dbm = 
  {
    n : int; 
    env : (ide * int) list ;
    matrix : bound array array 
  }
  type t  = Bottom | Env of dbm
  let bottom = Bottom



  let create_type_dbm n env matrix = Env{n; env;matrix}
  
  let is_bottom env = match env with
    | Bottom -> true
    | _ -> false

  let copy_matrix m = Array.map Array.copy m
  (* ottieni index della x *)
  (* let index_of env x = try Some (List.assoc x env) with Not_found -> None
 
  let resolve_index env x =
    match index_of env x with
    | Some i -> i
    | None -> failwith (Printf.sprintf "Zones: variabile '%s' non dichiarata" x) *)

  let init (vars : ide list) : t =
    (* 1. Ordino la lista ed elimino i duplicati *)
    let xs = List.sort_uniq compare vars in
    let n = List.length xs in

    (* 2. Mappo gli identificatori con indici da 1 a n
      (l'indice 0 è riservato a V0) *)
    let env = List.mapi (fun i x -> (x, i + 1)) xs in

    (* 3. Creo la matrice (n+1) x (n+1)
      inizializzata a PosInf (Top) *)
    let dim = n + 1 in
    let matrix = Array.make_matrix dim dim PosInf in

    (* 4. Imposto solo la diagonale a Int 0 (m_ii = 0) *)
    for i = 0 to dim - 1 do
      matrix.(i).(i) <- Int 0
    done;
    create_type_dbm n env matrix

  let b_leq b1 b2 = 
    match b1, b2 with
    | NegInf, _ -> true
    | Int x, Int y -> x <= y
    | PosInf, Int _ -> false
    |_, PosInf -> true
    | _ -> false

  (* Algoritmo di chiusura della DBM *)
  let close_dbm env = match env with
  | Bottom -> Bottom
  | Env dbm ->
    (* let s = dbm.n +1 in  *)
    let iter_cube dim f = 
    for k = 0 to dim -1 do 
      for i = 0 to dim -1 do 
        for j = 0 to dim -1 do
          f k i j
        done
      done
    done in 
    let floyd_wharshall matrix n = 
      let res_m = copy_matrix matrix in 
      iter_cube n (fun k i j ->  
      match res_m.(i).(k), res_m.(k).(j) with
      | Int x, Int y -> 
        let actual_val = res_m.(i).(j) in  
        let k_path = Int(x + y) in
        if b_leq k_path actual_val 
        then res_m.(i).(j) <- k_path;
      | _ ->  ()); 
      res_m in 
    let rec has_neg_cycle matrix dim i = 
    if i >= dim then false
    else match matrix.(i).(i) with
    | Int x -> if x < 0 then true else has_neg_cycle matrix dim (i+1)
    | _ -> has_neg_cycle matrix dim (i+1) in 

    let computated_matrix = floyd_wharshall dbm.matrix (dbm.n + 1) in 
    if (has_neg_cycle computated_matrix (dbm.n + 1) 0 ) then Bottom
    else create_type_dbm dbm.n dbm.env computated_matrix 

  (* Normalizzazione della dbm *)
  let normalize dbm = close_dbm dbm 

  (* Operazioni su dbm *)
  let leq m n = (* confrontando le celle elemento per elemento m* < n  *)
    match normalize m,n with
    | Bottom,_ -> true
    | _,Bottom -> false
    | Env m1, Env n1 -> 
      (* 
      For_all2 scorre contemporaneamente due array ed applica una funzione ai loro elementi 
      il primo scorre le colonne applicando la funzione che scorre le righe ed applica b_leq
      appena b_leq dà false termina, altrimenti restituisce true
      *) 
      Array.for_all2  (
        fun riga1 riga2 -> Array.for_all2 b_leq riga1 riga2
      ) m1.matrix n1.matrix

  let lub m n = (* m U n -> m* U n* -> min(leq) o t.c. y(m) U y(n) contenuto y(o)    *)
  match normalize m,normalize n with
    | Bottom,_ | _,Bottom -> Bottom
    | Env m1, Env n1 ->
      (* 
      map2 scorre contemporaneamente due array ed applica una funzione ai loro elementi 
      il primo scorre le colonne applicando la funzione che scorre le righe ed applica b_max
      *) 
      let maxmat = 
      Array.map2 (
        fun rigam rigan -> Array.map2 max_bound rigam rigan
      ) m1.matrix n1.matrix in
      close_dbm (create_type_dbm m1.n m1.env maxmat)

  let glb m n = (* stringenti tramite il minimo cella per cella *)
    match m,n with
    | Bottom,_ | _,Bottom -> Bottom
    | Env m1, Env n1 ->
      (* 
      map2 scorre contemporaneamente due array ed applica una funzione ai loro elementi 
      il primo scorre le colonne applicando la funzione che scorre le righe ed applica b_min
      *) 
      let minmat = 
      Array.map2 (
        fun rigam rigan -> Array.map2 min_bound rigam rigan
      ) m1.matrix n1.matrix in
      close_dbm (create_type_dbm m1.n m1.env minmat)
  let widen m n = failwith "not implemented"
  let narrow m n = failwith "not implemented"

  (* Operazioni su valori di dbm *)
  (* Reset non deterministico *)
  let forget i env =  
    match close_dbm env with
    |Bottom -> Bottom
    |Env dbm -> 
      for k = 0 to dbm.n do
        if k<> i then begin 
          dbm.matrix.(i).(k) <- PosInf;
          dbm.matrix.(k).(i) <- PosInf;
        end
      done;
      create_type_dbm dbm.n dbm.env dbm.matrix

  let filter_rel i j c env = match env with
  | Bottom -> Bottom
  | Env dbm -> 
    let new_m = copy_matrix dbm.matrix in 
    new_m.(i).(j) <- min_bound new_m.(i).(j) (Int (c));
    close_dbm (Env dbm)
    
  (* modella assegnazioni di vincoli del tipo Vj = Vi + c *)
  let assign_var_offset i j c env = match forget i env with
    | Bottom -> Bottom
    | Env dbm -> 
      dbm.matrix.(j).(i) <- Int c;
      dbm.matrix.(i).(j) <- Int (-c);
      close_dbm (Env dbm )
  
  (* modella assegnazioni di vincoli del tipo Vj = c *)
  let assign_const i c env = assign_var_offset i 0 c env
  (* modella assegnazioni di vincoli del tipo Vj = Vj + c *)
  let shift_var i c env = match env with
  | Bottom -> Bottom
  | Env dbm ->
    let new_m = copy_matrix dbm.matrix in 
    for k = 0 to dbm.n do 
      if k <> i then begin 
        new_m.(k).(i) <- add_bound new_m.(k).(i) (Int c );
        new_m.(i).(k) <- add_bound new_m.(i).(k) (Int(-c));
      end
    done;
    create_type_dbm dbm.n dbm.env new_m

  let string_of_value valore = to_string valore

  let to_string env = failwith "not implemented"
  let print env = failwith "not implemented"

  let abstract_int x = Interval(Int(x),Int(x))
  let abstract_range x y = if x > y then Interval(Int(y),Int(x)) else Interval(Int(x),Int(y))

  let retrieve_variable id env = failwith "retrieve var not implemented"

end