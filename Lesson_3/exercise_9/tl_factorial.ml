(* Basic implementation of a tail-recursive factorial 
Recall: n! = n * n-1 * n-2 * ... * 1 *)

let rec tl_factorial ?(acc: int = 1) (n:int) : int = match n with
  | 0 | 1 -> acc
  | _ -> tl_factorial ~acc:(acc * n) (n-1) 


  


