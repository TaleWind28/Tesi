open Abstract_domains.Signs

let sign_to_string = function
    | SignTop    -> "Top"
    | Pos        -> "Pos"
    | Neg        -> "Neg"
    | Zero       -> "Zero"
    | SignBottom -> "Bottom"
    | PosZero    -> "PosZero"
    | NegZero    -> "NegZero"
    | NonZero    -> "NonZero"
  
  let bound_to_string = function
    | NegInf -> "-inf"
    | PosInf -> "+inf"
    | Int n -> string_of_int n

  let interval_to_string = function
    | Bottom -> "Bottom"
    | Interval (a, b) ->
        Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)


(* Creiamo il "Testable" per Alcotest *)
let sign_testable =
  let pp fmt s = Format.fprintf fmt "%s" (sign_to_string s) in
  Alcotest.testable pp ( = )

let interval_testanble = 
  let pp fmt s = Format.fprintf fmt "%s" (interval_to_string s) in Alcotest.testable pp ( = )