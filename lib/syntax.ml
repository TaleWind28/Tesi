(* Operatori binari *)
type bop = Add | Mul | Div | Sub

(* Operatori unari *)
type uop = Negation

type ide = string

type comparator = Equals | Bigger | Smaller | BiggerEquals | SmallerEquals | NotEquals

(* Espressioni *)
type exp =
  | Const of int
  | Var of string
  | BinaryOperation of exp * bop * exp
  | UnaryOperation of uop * exp
  | Random of int * int

type cond = 
  | Comparison of exp * comparator * exp
  | Boolean of bool
  | Not of cond
  | And of cond * cond
  | Or of cond * cond

type cmd = 
  | Assign of ide * exp 
  | Sequence of cmd * cmd
  | Filter of cond * cmd
  | Skip