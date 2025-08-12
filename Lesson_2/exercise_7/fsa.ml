type nodeT = Internal | Start | Final | Error
type node = {transitions: transition list; behaviour: nodeT}
and transition = 
  | Recursive of char
  | Forward of node * char
type fsa = node list

let error_node = {transitions = []; behaviour = Error}

let next_node (x: char) (n: node) : node = 
  let valid_node = List.find_map
      (fun t -> match t with 
        | Recursive c -> if x = c then Some n else None
        | Forward (n', c) -> if x = c then Some n' else None      
        ) 
      n.transitions in
  if Option.is_some valid_node then Option.get valid_node
  else error_node
  

let rec aux_fsa_executor (x: string) (n: node) : bool = 
  if String.length x = 0 then 
    begin
    if n.behaviour = Final then true else false 
    end
  else 
    begin
    let curr_char = x.[0] in 
    let next_node = next_node curr_char n in
    if (next_node.behaviour = Error) 
      then false 
      else 
        let rest = String.sub x 1 (String.length x - 1) in
        aux_fsa_executor rest next_node
    end

let fsa_executor (x: string) (automata: fsa) : bool =
  let start_node = List.find 
    (fun (n:node) -> n.behaviour = Start) automata in
    aux_fsa_executor x start_node


let n2 = {transitions = []; behaviour = Final}
let n1 = {transitions = [Recursive 'b'; Forward (n2, 'c')]; behaviour = Internal}
let n0 = {transitions = [Forward(n1, 'a')]; behaviour = Start}

let automata = [n0; n1; n2]

let test = fsa_executor "abcc" automata