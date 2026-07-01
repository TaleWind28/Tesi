(* Operatori binari *)
type bop = Add | Mul | Div | Sub

(* Operatori unari *)
type uop = Negation

(* Espressioni *)
type exp =
  | Const of int
  | Var of string
  | BinaryOperation of exp * bop * exp
  | UnaryOperation of uop * exp
  | Random of int * int