(* Define a type for Finite State Automata and a function for checking if a 
given string is inside the generated language. 
But this time use Maps and Sets! *)

(* Module type for alphabets *)
module type Alphabet = sig
  type t
  val compare: t -> t -> int
end

(* Module type for nodes *)
module type Node = sig
  type t (*identifier of the node*)
  val compare: t -> t -> int
  val make: t -> t
end

(* Defines set of nodes*)
module SetNode(N: Node) = struct
  include Set.Make(N)
end

module TransitionKey(A: Alphabet)(N: Node) = struct
  type t = {
    start_node : N.t;
    symbol: A.t
  }
  
  let compare (e1:t) (e2:t) : int =
    match N.compare e1.start_node e2.start_node with
    | 0 -> A.compare e1.symbol e2.symbol
    | v -> v 
end

module TransitionMap(A: Alphabet)(N: Node) = struct
module Key = TransitionKey(A)(N)
  module M = Map.Make(Key)
  type key = M.key
  type t = N.t M.t

  let empty : t = M.empty
  let is_empty : t -> bool = M.is_empty
  let mem : key -> t -> bool = M.mem
  let add : key -> N.t -> t -> t = M.add
  let update : key -> (N.t option -> N.t option) -> t -> t = M.update
  let singleton : key -> N.t -> t = M.singleton
  let remove : key -> t -> t = M.remove
  let merge : (key -> N.t option -> N.t option -> N.t option) -> t -> t -> t = M.merge
  let union : (key -> N.t -> N.t -> N.t option) -> t -> t -> t = M.union
  let compare : (N.t -> N.t -> int) -> t -> t -> int = M.compare
  let equal : (N.t -> N.t -> bool) -> t -> t -> bool = M.equal
  let iter : (key -> N.t -> unit) -> t -> unit = M.iter
  let fold : (key -> N.t -> 'a -> 'a) -> t -> 'a -> 'a = M.fold
  let for_all : (key -> N.t -> bool) -> t -> bool = M.for_all
  let exists : (key -> N.t -> bool) -> t -> bool = M.exists
  let filter : (key -> N.t -> bool) -> t -> t = M.filter
  let filter_map : (key -> N.t -> N.t option) -> t -> t = M.filter_map
  let partition : (key -> N.t -> bool) -> t -> t * t = M.partition
  let cardinal : t -> int = M.cardinal
  let bindings : t -> (key * N.t) list = M.bindings
  let min_binding : t -> key * N.t = M.min_binding
  let min_binding_opt : t -> (key * N.t) option = M.min_binding_opt
  let max_binding : t -> key * N.t = M.max_binding
  let max_binding_opt : t -> (key * N.t) option = M.max_binding_opt
  let choose : t -> key * N.t = M.choose
  let choose_opt : t -> (key * N.t) option = M.choose_opt
  let split : key -> t -> t * N.t option * t = M.split
  let find : key -> t -> N.t = M.find
  let find_opt : key -> t -> N.t option = M.find_opt
  let find_first : (key -> bool) -> t -> key * N.t = M.find_first
  let find_first_opt : (key -> bool) -> t -> (key * N.t) option = M.find_first_opt
  let find_last : (key -> bool) -> t -> key * N.t = M.find_last
  let find_last_opt : (key -> bool) -> t -> (key * N.t) option = M.find_last_opt
  let map : (N.t -> N.t) -> t -> t = M.map
  let mapi : (key -> N.t -> N.t) -> t -> t = M.mapi
  let to_seq : t -> (key * N.t) Seq.t = M.to_seq
  let to_seq_from : key -> t -> (key * N.t) Seq.t = M.to_seq_from
  let add_seq : (key * N.t) Seq.t -> t -> t = M.add_seq
  let of_seq : (key * N.t) Seq.t -> t = M.of_seq
end

module FSA(A:Alphabet)(N: Node) = struct
  include N
  module NodeSet = SetNode(N)
  module Transition = TransitionMap(A)(N)

  type t = {
    nodes: NodeSet.t;
    final_nodes: NodeSet.t;
    start_node : N.t;
    transition_map : Transition.t
  }

  let rec aux_accepts (fsa: t) (input: A.t list)(current_state: N.t) : bool =
    match input with
    | [] -> NodeSet.mem current_state fsa.final_nodes  
    | x :: xs -> 
      let key : Transition.key = {start_node = current_state; symbol = x} in
      match Transition.find_opt key fsa.transition_map with
      | Some new_state -> aux_accepts fsa xs new_state
      | None -> false

  let accepts (fsa: t) (input: A.t list) : bool = 
    aux_accepts fsa input fsa.start_node
end

(* Example *)
module CharAlphabet : Alphabet with type t = char = struct
  type t = char
  let compare = Char.compare
end

module IntNode : Node with type t = int = struct
  type t = int
  let compare = Int.compare
  let make (id: int) : t = id
end

module BaseFSA = FSA(CharAlphabet)(IntNode)
include BaseFSA

let n0 = IntNode.make 0
let n1 = IntNode.make 1
let n2 = IntNode.make 2

(* Build the set of all nodes *)
let nodes =
  NodeSet.of_list [n0; n1; n2]

(* Final nodes set: only n2 *)
let final_nodes =
  NodeSet.singleton n2

(* Build the transitions *)
let transitions =
  let open Transition in
  empty
  |> add { start_node = n0; symbol = 'a' } n1
  |> add { start_node = n1; symbol = 'b' } n1
  |> add { start_node = n1; symbol = 'c' } n2

(* Assemble the automaton *)
let automaton : t = {
  nodes;
  final_nodes;
  start_node = n0;
  transition_map = transitions;
}

(* Example usage *)
let test_strings = [
  ['a'; 'b'; 'c'];           (* Should be accepted: a->c *)
  ['a'; 'c'];                (* Should be rejected: a->b->c *)
  ['a'; 'b'; 'b'; 'c']; (* Should be accepted: a->b->b->c *)
  ['a'];                (* Should be rejected: ends in non-final state *)
  ['b'; 'c'];           (* Should be rejected: no transition from start *)
  []                    (* Should be rejected: empty string, start is not final *)
]

let () =
  List.iter (fun input ->
    let result = accepts automaton input in
    let input_str = String.of_seq (List.to_seq input) in
    Printf.printf "String \"%s\": %s\n" input_str (if result then "ACCEPTED" else "REJECTED")
  ) test_strings