(*
Write a pair of functions for evaluating arithmetical
and boolean expressions with the Of bool case.
Test their functionality using utop
*)

type a_exp =
  Aval of int
  | Plus of a_exp * a_exp
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp
  | Of_Bool of b_exp
and b_exp =
  Bval of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp

let rec aexpOps (a : a_exp) : int =
  match a with
  | Aval x -> x
  | Plus (x,y) -> aexpOps x + aexpOps y
  | Minus (x,y) -> aexpOps x - aexpOps y
  | Times (x,y) -> aexpOps x * aexpOps y
  | Of_Bool x -> let b = bexpOps x in
    match b with
    | true -> 1
    | false -> 0
and bexpOps (b : b_exp) : bool =
  match b with
  | Bval x -> x
  | And (x, y) ->  bexpOps x && bexpOps y
  | Or (x, y) -> bexpOps x || bexpOps y
  | Not x -> not(bexpOps x)
  | Minor (x, y) -> aexpOps x < aexpOps y
