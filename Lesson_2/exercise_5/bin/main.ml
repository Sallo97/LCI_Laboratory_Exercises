(*
Write a function that given two functions f and g
returns a function over pairs defined as f on the first element and g
on the second element.
*)

let func_over_pairs f g pair =
  match pair with
  |(x, y) -> (f x, g y)

(* example *)
let sum x = x + 1
let sub x = x - 1
let test = func_over_pairs sum sub ;;
test (5,5)
