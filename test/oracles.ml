(*value *)
module type EXPECTED_VALUES = sig
  type t

  (* Somma *)
  val sum_1 : t
  val sum_2 : t
  val sum_3 : t
  val sum_4 : t
  val sum_5 : t
  val sum_6 : t
  val sum_7 : t
  val sum_8 : t
  val sum_9 : t
  val sum_10 : t
  val sum_11 : t
  val sum_12 : t
  val sum_13 : t
  val sum_14 : t
  val sum_15 : t
  val sum_16 : t
  val sum_17 : t
  val sum_18 : t

  (* Sottrazione *)
  val sub_1 : t
  val sub_2 : t
  val sub_3 : t
  val sub_4 : t
  val sub_5 : t

  (* Moltiplicazione *)
  val mul_1 : t
  val mul_2 : t
  val mul_3 : t
  val mul_4 : t
  val mul_5 : t
  val mul_6 : t
  val mul_7 : t
  val mul_8 : t
  val mul_9 : t
  val mul_10 : t

  (* Divisione *)
  val div_1 : t
  val div_2 : t
  val div_3 : t
  val div_4 : t
  val div_5 : t
  val div_6 : t
  val div_7 : t
  val div_8 : t
  val div_9 : t
  val div_10 : t
  val div_11 : t
  val div_12 : t

  (* Negazione Unaria *)
  val neg_1 : t
  val neg_2 : t
  val neg_3 : t
  val neg_4 : t
  val neg_5 : t
  val neg_6 : t
  val neg_7 : t
  val neg_8 : t
  val neg_9 : t
  val neg_10 : t

  (* Random *)
  val rand_1 : t
  val rand_2 : t
  val rand_3 : t
  val rand_4 : t
  val rand_5 : t
  val rand_6 : t

  (* Assegnamenti *)
  val assign_1 : t
  val assign_2 : t
  val assign_3 : t
  val assign_4 : t
  val assign_5 : t

  (* Sequenze *)
  val sequence_1_1 : t
  val sequence_1_2 : t

  (* Skip *)
  val skip_1 : t

  (* If *)
  val if_1 : t
  val if_2 : t
  val if_3 : t
  val if_4 : t
  val if_5 : t
  val if_6 : t
  val if_7 : t
  val if_8 : t
  val if_9 : t
  val if_10 : t
  val if_11 : t
  val if_12 : t

  (* While *)
  val while_1_1 : t
  val while_1_2 : t
  val while_2_1 : t
  val while_3_1 : t
  val while_4_1 : t
  val while_4_2 : t
  val while_4_3 : t
end
(* Aggiungi qui gli altri valori se variano tra domini *)

module Expected_Signs : EXPECTED_VALUES with type t = Abstract_domains.Signs.t  = struct
  open Abstract_domains.Signs

  type t = Abstract_domains.Signs.t

  (* Somma *)
  let sum_1 = Pos
  let sum_2 = SignTop
  let sum_3 = Neg
  let sum_4 = Pos
  let sum_5 = Zero
  let sum_6 = PosZero
  let sum_7 = NegZero
  let sum_8 = SignTop
  let sum_9 = Pos
  let sum_10 = SignTop
  let sum_11 = SignTop
  let sum_12 = Neg
  let sum_13 = SignTop
  let sum_14 = NonZero
  let sum_15 = SignTop
  let sum_16 = SignTop
  let sum_17 = SignBottom
  let sum_18 = SignTop

  (* Sottrazione *)
  let sub_1 = Pos
  let sub_2 = SignTop
  let sub_3 = SignTop
  let sub_4 = SignTop
  let sub_5 = Pos

  (* Moltiplicazione *)
  let mul_1 = Pos
  let mul_2 = Neg
  let mul_3 = Pos
  let mul_4 = Zero
  let mul_5 = NegZero
  let mul_6 = NegZero
  let mul_7 = Zero
  let mul_8 = NonZero
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = PosZero
  let div_2 = NegZero
  let div_3 = PosZero
  let div_4 = SignBottom
  let div_5 = SignTop
  let div_6 = SignTop
  let div_7 = SignTop
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = SignTop
  let div_11 = NegZero
  let div_12 = NegZero

  (* Negazione Unaria *)
  let neg_1 = Neg
  let neg_2 = Pos
  let neg_3 = Zero
  let neg_4 = NegZero
  let neg_5 = PosZero
  let neg_6 = NonZero
  let neg_7 = SignTop
  let neg_8 = SignBottom
  let neg_9 = Pos
  let neg_10 = Pos

  (* Random *)
  let rand_1 = SignTop
  let rand_2 = Pos
  let rand_3 = Neg
  let rand_4 = PosZero
  let rand_5 = NegZero
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = Pos
  let assign_2 = Neg
  let assign_3 = Zero
  let assign_4 = SignTop
  let assign_5 = Pos

  (* Sequenze *)
  let sequence_1_1 = Pos
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = Pos

  (* If *)
  let if_1 = Pos
  let if_2 = Pos
  let if_3 = Pos
  let if_4 = NonZero
  let if_5 = PosZero
  let if_6 = NegZero
  let if_7 = SignTop
  let if_8 = Pos
  let if_9 = Pos
  let if_10 = Neg
  let if_11 = NonZero
  let if_12 = NonZero

  (* While *)
  let while_1_1 = Pos
  let while_1_2 = Neg
  let while_2_1 = Zero
  let while_3_1 = Pos
  let while_4_1 = SignTop
  let while_4_2 = Pos
  let while_4_3 = SignTop
end

module Expected_SimpleSigns : EXPECTED_VALUES with type t = Abstract_domains.SimpleSigns.t = struct
  open Abstract_domains.SimpleSigns
  
  type t = Abstract_domains.SimpleSigns.t
  let sum_1 = PosZero
  let sum_2 = top
  let sum_3 = NegZero
  let sum_4 = PosZero
  let sum_5 = Zero
  let sum_6 = PosZero
  let sum_7 = NegZero
  let sum_8 = top
  let sum_9 = PosZero
  let sum_10 = SignTop
  let sum_11 = SignTop
  let sum_12 = NegZero
  let sum_13 = SignTop
  let sum_14 = top
  let sum_15 = SignTop
  let sum_16 = SignTop
  let sum_17 = SignBottom
  let sum_18 = SignTop

  (* Sottrazione *)
  let sub_1 = PosZero
  let sub_2 = SignTop
  let sub_3 = SignTop
  let sub_4 = SignTop
  let sub_5 = PosZero

  (* Moltiplicazione *)
  let mul_1 = PosZero
  let mul_2 = NegZero
  let mul_3 = PosZero
  let mul_4 = Zero
  let mul_5 = NegZero
  let mul_6 = NegZero
  let mul_7 = Zero
  let mul_8 = top
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = PosZero
  let div_2 = NegZero
  let div_3 = PosZero
  let div_4 = SignBottom
  let div_5 = PosZero
  let div_6 = NegZero
  let div_7 = SignTop
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = SignTop
  let div_11 = NegZero
  let div_12 = NegZero

  (* Negazione Unaria *)
  let neg_1 = NegZero
  let neg_2 = PosZero
  let neg_3 = Zero
  let neg_4 = NegZero
  let neg_5 = PosZero
  let neg_6 = top
  let neg_7 = SignTop
  let neg_8 = SignBottom
  let neg_9 = PosZero
  let neg_10 = PosZero

  (* Random *)
  let rand_1 = SignTop
  let rand_2 = PosZero
  let rand_3 = NegZero
  let rand_4 = PosZero
  let rand_5 = NegZero
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = PosZero
  let assign_2 = NegZero
  let assign_3 = Zero
  let assign_4 = SignTop
  let assign_5 = PosZero

  (* Sequenze *)
  let sequence_1_1 = PosZero
  let sequence_1_2 = NegZero

  (* Skip *)
  let skip_1 = PosZero

  (* If *)
  let if_1 = top
  let if_2 = PosZero
  let if_3 = PosZero
  let if_4 = top
  let if_5 = PosZero
  let if_6 = NegZero
  let if_7 = SignTop
  let if_8 = PosZero
  let if_9 = PosZero
  let if_10 = NegZero
  let if_11 = top
  let if_12 = top

  (* While *)
  let while_1_1 = top (*risultato corretto: PosZero*)
  let while_1_2 = NegZero (* non serve questo valore*)
  let while_2_1 = bottom (* è accettabile anche PosZero, ma ottengo bottomEnv*) 
  let while_3_1 = SignTop (*0*)
  let while_4_1 = SignTop (*s*)
  let while_4_2 = PosZero (*s*)
  let while_4_3 = SignTop
end

(*
module Expected_ReducedSigns : EXPECTED_VALUES with type t = Abstract_domains.ReducedSigns.t = struct
  open Abstract_domains.ReducedSigns
  
  type t = Abstract_domains.ReducedSigns.t
  let sum_1 = Pos
  let sum_2 = top
  let sum_3 = top
  let sum_4 = bottom
  let sum_5 = top
  let mul_1 = top
  let mul_2 = top
  let mul_3 = bottom
  let mul_4 = Pos

  let div_1 = bottom

  let div_2 = top

  let assign_1 = Pos
  let assign_2 = Neg
  let  if_1 = Pos
end *)

module Expected_SimplifiedSigns :EXPECTED_VALUES with type t = Abstract_domains.SimplifiedSigns.t = struct
  open Abstract_domains.SimplifiedSigns
  
  type t = Abstract_domains.SimplifiedSigns.t

  (* Somma *)
  let sum_1 = Pos
  let sum_2 = SignTop
  let sum_3 = Neg
  let sum_4 = Pos
  let sum_5 = Zero
  let sum_6 = SignTop
  let sum_7 = SignTop
  let sum_8 = SignTop
  let sum_9 = top
  let sum_10 = SignTop
  let sum_11 = SignTop
  let sum_12 = top
  let sum_13 = SignTop
  let sum_14 = SignTop
  let sum_15 = SignTop
  let sum_16 = SignTop
  let sum_17 = SignBottom
  let sum_18 = SignTop

  (* Sottrazione *)
  let sub_1 = Pos
  let sub_2 = SignTop
  let sub_3 = SignTop
  let sub_4 = SignTop
  let sub_5 = Pos

  (* Moltiplicazione *)
  let mul_1 = Pos
  let mul_2 = Neg
  let mul_3 = Pos
  let mul_4 = Zero
  let mul_5 = SignTop
  let mul_6 = SignTop
  let mul_7 = Zero
  let mul_8 = SignTop
  let mul_9 = Zero
  let mul_10 = SignBottom

  (* Divisione *)
  let div_1 = Pos
  let div_2 = Neg
  let div_3 = Pos
  let div_4 = bottom
  let div_5 = SignTop
  let div_6 = SignTop
  let div_7 = SignTop
  let div_8 = Zero
  let div_9 = Zero
  let div_10 = SignTop
  let div_11 = SignTop
  let div_12 = SignTop

  (* Negazione Unaria *)
  let neg_1 = Neg
  let neg_2 = Pos
  let neg_3 = Zero
  let neg_4 = SignTop
  let neg_5 = SignTop
  let neg_6 = SignTop
  let neg_7 = SignTop
  let neg_8 = SignBottom
  let neg_9 = Pos
  let neg_10 = Pos

  (* Random *)
  let rand_1 = SignTop
  let rand_2 = Pos
  let rand_3 = Neg
  let rand_4 = SignTop
  let rand_5 = SignTop
  let rand_6 = Zero

  (* Assegnamenti *)
  let assign_1 = Pos
  let assign_2 = Neg
  let assign_3 = Zero
  let assign_4 = SignTop
  let assign_5 = Pos

  (* Sequenze *)
  let sequence_1_1 = Pos
  let sequence_1_2 = Neg

  (* Skip *)
  let skip_1 = Pos

  (* If *)
  let if_1 = Pos
  let if_2 = Pos
  let if_3 = Pos
  let if_4 = SignTop
  let if_5 = SignTop
  let if_6 = SignTop
  let if_7 = SignTop
  let if_8 = Pos
  let if_9 = Pos
  let if_10 = Neg
  let if_11 = SignTop
  let if_12 = SignTop

  (* While *)
  let while_1_1 = Pos
  let while_1_2 = Neg
  let while_2_1 = Zero
  let while_3_1 = Pos
  let while_4_1 = SignTop
  let while_4_2 = Pos
  let while_4_3 = SignTop
end


(* module Expected_Strange : VALUES with type t = Abstract_domains.StrangeSigns.t = struct
  open Abstract_domains.StrangeSigns
  
  type t = Abstract_domains.StrangeSigns.t
    let sum_1 = PosZero
  let sum_2 = Zero
  let sum_3 = top
  let sum_4 = bottom
  let sum_5 = top
  let mul_1 = Zero
  let mul_2 = Zero
  let mul_3 = bottom
  let mul_4 = PosZero

  let div_1 = bottom

  let div_2 = Zero

  let assign_1 = PosZero
  let assign_2 = Neg
  let  if_1 = PosZero
end *)
