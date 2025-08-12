(* Write a function that packs consecutive duplicates of the input list elements into sublists. *)

let rec create_elem (acc : int list) (l : int list) : int list * int list =
  let common = List.hd acc in
  match l with
  | [] -> (acc, [])
  | x :: xs -> if x = common then create_elem (x::acc) xs else (acc, x::xs)

let rec aux_pack_cons (l : int list) (acc: int list list) : int list list =
  match l with 
  | [] -> List.rev acc
  | x :: xs -> let (elem, l') = (create_elem [x] xs) in
    aux_pack_cons l' (elem :: acc)
    
let pack_cons (l: int list) : int list list =
  aux_pack_cons l []
