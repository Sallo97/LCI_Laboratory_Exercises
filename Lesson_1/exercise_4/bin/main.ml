(*
Write a function that packs consecutive duplicates of
the input list elements into sublists.
*)



let rec real_pack (h: 'a) (t: 'a list) : 'a list list =
  match t with
  | [] ->  [h] :: []
  | a :: b ->
    if h = a then real_pack
    else [h] :: (pack_items a b)

let rec fake_pack l =
  match l with
  | [] -> []
  | h :: t -> real_pack h t
