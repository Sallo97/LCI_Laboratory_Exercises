(*
Write a type for general polymorphic trees (i.e. with
any number of children)
*)

type 'a gtree =
  Leaf of 'a
  | Node of 'a * 'a gtree list

let tree =
  Node ( 5, [ Node (6, [Leaf 7])])
