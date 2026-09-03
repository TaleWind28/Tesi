module type EXPECTED_VALUES = sig
  type t

  val expected_sum_1 : t
  val expected_sum_2 : t
  val expected_sum_3 : t
  val expected_sum_4 : t
  val expected_sum_5 : t
  val expected_mul_1 : t
  val expected_mul_2 : t
  val expected_mul_3 : t
  val expected_mul_4 : t

  val expected_div_1 : t

  val expected_div_2 : t

  val expected_assign_1 : t
  val expected_assign_2 : t
  
  
  (* Valori attesi specifici per ogni test di controllo di flusso o espressione *)
  val expected_if_1 : t
  (* Aggiungi qui gli altri valori se variano tra domini *)
end

module Expected_Signs : EXPECTED_VALUES with type t = Abstract_domains.Signs.t  = struct
  open Abstract_domains.Signs

  type t = Abstract_domains.Signs.t
  let expected_sum_1 = Pos
  let expected_sum_2 = Zero
  let expected_sum_3 = top
  let expected_sum_4 = bottom
  let expected_sum_5 = top
  let expected_mul_1 = Zero
  let expected_mul_2 = Zero
  let expected_mul_3 = bottom
  let expected_mul_4 = Pos

  let expected_div_1 = bottom

  let expected_div_2 = Zero

  let expected_assign_1 = Pos
  let expected_assign_2 = Neg
  let  expected_if_1 = Pos

end

module Expected_SimpleSigns : EXPECTED_VALUES with type t = Abstract_domains.SimpleSigns.t = struct
  open Abstract_domains.SimpleSigns
  
  type t = Abstract_domains.SimpleSigns.t
  let expected_sum_1 = PosZero
  let expected_sum_2 = Zero
  let expected_sum_3 = top
  let expected_sum_4 = bottom
  let expected_sum_5 = top
  let expected_mul_1 = Zero
  let expected_mul_2 = Zero
  let expected_mul_3 = bottom
  let expected_mul_4 = PosZero

  let expected_div_1 = bottom

  let expected_div_2 = Zero

  let expected_assign_1 = PosZero
  let expected_assign_2 = NegZero
  let  expected_if_1 = top
end

module Expected_ReducedSigns : EXPECTED_VALUES with type t = Abstract_domains.ReducedSigns.t = struct
  open Abstract_domains.ReducedSigns
  
  type t = Abstract_domains.ReducedSigns.t
  let expected_sum_1 = Pos
  let expected_sum_2 = top
  let expected_sum_3 = top
  let expected_sum_4 = bottom
  let expected_sum_5 = top
  let expected_mul_1 = top
  let expected_mul_2 = top
  let expected_mul_3 = bottom
  let expected_mul_4 = Pos

  let expected_div_1 = bottom

  let expected_div_2 = top

  let expected_assign_1 = Pos
  let expected_assign_2 = Neg
  let  expected_if_1 = Pos
end

module Expected_SimplifiedSigns : EXPECTED_VALUES with type t = Abstract_domains.SimplifiedSigns.t = struct
  open Abstract_domains.SimplifiedSigns
  
  type t = Abstract_domains.SimplifiedSigns.t
    let expected_sum_1 = Pos
  let expected_sum_2 = Zero
  let expected_sum_3 = top
  let expected_sum_4 = bottom
  let expected_sum_5 = top
  let expected_mul_1 = Zero
  let expected_mul_2 = Zero
  let expected_mul_3 = bottom
  let expected_mul_4 = Pos

  let expected_div_1 = bottom

  let expected_div_2 = Zero

  let expected_assign_1 = Pos
  let expected_assign_2 = Neg
  let  expected_if_1 = Pos
end

module Expected_Strange : EXPECTED_VALUES with type t = Abstract_domains.StrangeSigns.t = struct
  open Abstract_domains.StrangeSigns
  
  type t = Abstract_domains.StrangeSigns.t
    let expected_sum_1 = PosZero
  let expected_sum_2 = Zero
  let expected_sum_3 = top
  let expected_sum_4 = bottom
  let expected_sum_5 = top
  let expected_mul_1 = Zero
  let expected_mul_2 = Zero
  let expected_mul_3 = bottom
  let expected_mul_4 = PosZero

  let expected_div_1 = bottom

  let expected_div_2 = Zero

  let expected_assign_1 = PosZero
  let expected_assign_2 = Neg
  let  expected_if_1 = PosZero
end