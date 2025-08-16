(* A basic tail recursive version of Fibonacci *)

let rec tl_fibonacci ?(acc:int * int = (0,1)) ?(curr:int = 1) (n:int) : int =
  if n < curr then match n with
    | 0 -> 0
    | _ -> fst acc
  else  
    let fst' = fst acc + snd acc in
    let acc' = (fst', fst acc) in
    tl_fibonacci ~acc:acc' ~curr:(curr+1) n
    
