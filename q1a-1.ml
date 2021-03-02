
(*Jack Schneiderhan*)
(*I pledge my honor that I have abided by the Stevens Honor System*)
(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <---- 3
  |     /\ |
  |    /   | 
 \/  /    \/
  2       4
*)
       
(* 
Eg. outgoing ex 3 => [1,4] 
*)
let outgoing_help1 (_,a) = a

let outgoing_help2 (a,_) = a

let rec outgoing_node g n = 
  match g with 
   | [] -> []
   | h::t -> if outgoing_help1 h = n then outgoing_help2 h :: outgoing_node t

(* 
   The list of nodes of the tree without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)


let rec nodes g =
   match g with 
   | [] -> []
   | h::t -> t :: nodes t


(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
   match n with
   | [] -> []
   | h::t -> if h = n 
                then remove g n 
             else if t = n
                then remove g n 
             else 
                h :: remove g n 
  
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,4,2,3] 
   *)

let rec reachable g n =
  failwith "implement me"
                               

