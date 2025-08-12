(* Same as ex_1 but with the Of bool case *)

type a_exp = AVal of int
  | Plus of a_exp * a_exp
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp
  | OfBool of b_exp
and b_exp = BVal of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp
  | Equal of a_exp * a_exp

let rec eval_aexp = function
  | AVal x -> x
  | Plus (a1, a2) -> (eval_aexp a1) + (eval_aexp a2)
  | Minus (a1, a2) -> (eval_aexp a1) - (eval_aexp a2)
  | Times (a1, a2) -> (eval_aexp a1) * (eval_aexp a2)
  | OfBool b1 -> let b: bool = (eval_bexp b1) in 
    if b = true then 1 else 0
and eval_bexp = function
  | BVal x -> x
  | And (b1, b2) -> (eval_bexp b1) && (eval_bexp b2)
  | Or (b1, b2) -> (eval_bexp b1) || (eval_bexp b2)
  | Not b1 -> not (eval_bexp b1)
  | Minor (a1, a2) -> (eval_aexp a1) < (eval_aexp a2)
  | Equal (a1, a2) -> (eval_aexp a1) = (eval_aexp a2)