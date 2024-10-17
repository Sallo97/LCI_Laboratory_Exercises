(*
Write a function that given a list of integers l1 returns
a list l2 of the same length such that each element of l2 in position i
is the sum of all the elements in l1 with position less or equal to i
*)

(* let strange_func lst =
  let rec helper ls n = ...
  in helpes ls 0*)

let rec strange_func acc l =
  match l with
  | [] -> []
  | h :: t -> let new_acc = h + acc in
    new_acc :: strange_func new_acc t

let base_case = strange_func 0
