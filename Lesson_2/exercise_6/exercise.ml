(* Write a function that given a list of integers l1 returns
a list l2 of the same length such that each element of l2 in position i
is the sum of all the elements in l1 with position less or equal to i. *)

let rec aux_sum_previous (l : int list) (acc_sum: int) (acc_list : int list) : int list = 
  match l with
  | [] -> List.rev acc_list
  | x :: xs -> let acc_sum = acc_sum + x in
    aux_sum_previous xs acc_sum (acc_sum :: acc_list)

let sum_previous (l: int list) : int list = 
  aux_sum_previous l 0 []