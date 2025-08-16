(* Write a tail recursive function for computing the sum of the elements in the 
leaves of a binary tree of integers. *)

type tree = 
  | Leaf of int
  | Node of tree * tree

let tl_sum_leaves (t: tree) : int = 
  let rec aux acc stack = 
    match stack with
    | [] -> acc
    | (Leaf v) :: rest -> aux (acc + v) rest
    | (Node (l, r)) :: rest -> aux acc (l :: r :: rest)
  in aux 0 [t]