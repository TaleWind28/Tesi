open Abstract_domains.Intervals

(* ------------------------------------------------------------------ *)
(* 1. Setup dei Testable e Helper per la stampa                      *)
(* ------------------------------------------------------------------ *)

let bound_to_string = function
  | NegInf -> "-inf"
  | PosInf -> "+inf"
  | Int n  -> string_of_int n

let interval_to_string = function
  | Bottom -> "Bottom"
  | Interval (a, b) ->
      Printf.sprintf "[%s, %s]" (bound_to_string a) (bound_to_string b)

(* Testable per Alcotest sugli intervalli *)
let interval_testable =
  let pp fmt i = Format.fprintf fmt "%s" (interval_to_string i) in
  Alcotest.testable pp ( = )

(* Helper per i test booleani (es. leq) *)
let make_bool_case (desc, op, a, b, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a b in
      Alcotest.(check bool) desc expected res )

(* Helper per i test unari sugli intervalli (es. negate) *)
let make_unary_case (desc, op, a, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a in
      Alcotest.(check interval_testable) desc expected res )

(* Helper per i test binari sugli intervalli (es. sum, mul, div, lub, glb) *)
let make_binary_case (desc, op, a, b, expected) =
  ( desc,
    `Quick,
    fun () ->
      let res = op a b in
      Alcotest.(check interval_testable) desc expected res )

(* ------------------------------------------------------------------ *)
(* 2. Liste di Test riutilizzate dal tuo codice                       *)
(* ------------------------------------------------------------------ *)

let leq_tests = List.map make_bool_case [
  ("Bottom, [1, 2]",               leq, Bottom, abstract_range 1 2, true);
  ("[1, 2], Bottom",               leq, abstract_range 1 2, Bottom, false);
  ("[2, 3] in [1, 5]",             leq, abstract_range 2 3, abstract_range 1 5, true);
  ("[1, 5] in [2, 3]",             leq, abstract_range 1 5, abstract_range 2 3, false);
  ("[1, 2] in [1, 2]",             leq, abstract_range 1 2, abstract_range 1 2, true);
  ("[0, 10] in Top",               leq, abstract_range 0 10, top, true);
  ("Top in [0, 10]",               leq, top, abstract_range 0 10, false);
]

let lub_tests = List.map make_binary_case [
  ("[1, 3], [5, 7]",               lub, abstract_range 1 3, abstract_range 5 7, abstract_range 1 7);
  ("[2, 5], [1, 3]",               lub, abstract_range 2 5, abstract_range 1 3, abstract_range 1 5);
  ("Bottom, [1, 2]",               lub, Bottom, abstract_range 1 2, abstract_range 1 2);
  ("[1, 2], Top",                  lub, abstract_range 1 2, top, top);
]

let glb_tests = List.map make_binary_case [
  ("[1, 5] e [3, 8]",              glb, abstract_range 1 5, abstract_range 3 8, abstract_range 3 5);
  ("[1, 3] e [5, 8] (disgiunti)",  glb, abstract_range 1 3, abstract_range 5 8, Bottom);
  ("[1, 5] e Bottom",              glb, abstract_range 1 5, Bottom, Bottom);
  ("[1, 5] e Top",                 glb, abstract_range 1 5, top, abstract_range 1 5);
]

let sum_tests = List.map make_binary_case [
  ("[1, 2] + [3, 4]",              sum, abstract_range 1 2, abstract_range 3 4, abstract_range 4 6);
  ("[-2, 5] + [10, 20]",           sum, abstract_range (-2) 5, abstract_range 10 20, abstract_range 8 25);
  ("[1, 2] + Bottom",              sum, abstract_range 1 2, Bottom, Bottom);
  ("[1, 2] + Top",                 sum, abstract_range 1 2, top, top);
]

let negate_tests = List.map make_unary_case [
  ("[1, 5]",                       negate, abstract_range 1 5, abstract_range (-5) (-1));
  ("[-3, 2]",                      negate, abstract_range (-3) 2, abstract_range (-2) 3);
  ("Bottom",                       negate, Bottom, Bottom);
]

let mul_tests = List.map make_binary_case [
  ("[2, 3] * [4, 5]",              mul, abstract_range 2 3, abstract_range 4 5, abstract_range 8 15);
  ("[-2, 3] * [-4, 5]",            mul, abstract_range (-2) 3, abstract_range (-4) 5, abstract_range (-12) 15);
  ("[-5, -2] * [-4, -1]",          mul, abstract_range (-5) (-2), abstract_range (-4) (-1), abstract_range 2 20);
  ("[-5, 2] * [4, 1]",             mul, abstract_range (-5) 2, abstract_range 4 1, abstract_range (-20) 8);
  ("[0, 5] * Bottom",              mul, abstract_range 0 5, Bottom, Bottom);
]

let div_tests = List.map make_binary_case [
  ("[10, 20] / [2, 5]",            div, abstract_range 10 20, abstract_range 2 5, abstract_range 2 10);
  ("[10, 20] / [-5, -2]",          div, abstract_range 10 20, abstract_range (-5) (-2), abstract_range (-10) (-2));
  ("[10, 20] / [0, 0]",            div, abstract_range 10 20, abstract_range 0 0, Bottom);
]

(* Lista globale dei test per gli Intervalli *)
let all_interval_cases =
  leq_tests @ lub_tests @ glb_tests @ sum_tests @ negate_tests @ mul_tests @ div_tests

(* Oppure raggruppati per operazione *)
let tests = [
  "Intervals - leq", leq_tests;
  "Intervals - lub", lub_tests;
  "Intervals - glb", glb_tests;
  "Intervals - sum", sum_tests;
  "Intervals - negate", negate_tests;
  "Intervals - mul", mul_tests;
  "Intervals - div", div_tests;
]