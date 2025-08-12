(* Write a type for general polymorphic trees (i.e. with any number of children) *)

type p_node = Leaf
  | Parent of p_node list

  