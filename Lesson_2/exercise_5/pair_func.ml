(* Write a function that given two functions f and g returns a function over 
pairs defined as f on the first element and g on the second element. *)

let pair_func (f: 'a -> 'b) (g: 'c -> 'd) : ('a * 'c )-> ('b * 'd) = 
  fun x -> (f (fst x), g (snd x)) 
