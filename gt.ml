(* 
   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:
    Name1: John Schneiderhan
    Name2:
    I pledge my honor that I have abided by the Stevens Honor System. 
*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let rec max2 l x =
  match l with
  |[] -> x
  |h::t -> if h > x then max2 t h else max2 t x

let rec height t =
  match t with
  |Node(x, []) -> 1
  |Node(x, subtree) -> max2(List.map height subtree) 0 + 1

let rec size t =
  let rec size_helper (Node(_, subtree)) =
      List.fold_left (fun n t -> n + size_helper t) 1 subtree
  in
  size_helper(t)

let rec paths_to_leaves t =
  match t with 
  |Node(x, []) -> []

let rec length_list l = 
  match l with 
  | [] -> 0
  | h::t -> 1 + length_list t

let rec perfect' l =
  match l with 
  | [] -> true
  | h::h2::t -> if h=h2 then perfect' t
                        else false
let rec is_perfect t =
  perfect' (map length_list (paths_to_leaves t)) 

let rec preorder_helper f l = 
  match l with 
  | [] -> []
  | h::t -> f h @ preorder_helper f t

let rec preorder_helper2 t =
  match t with 
  |Node(x, subtree) -> Node(x, []) ::
                      (fun l -> match l with
                                |[] -> []
                                | h::t -> preorder_helper2 h @ preorder_helper preorder_helper2 t) subtree

let rec preorder_helper3 l =
  match l with
  |[] -> []
  |h::t -> match h with 
          |Node(x, subtree) -> x :: preorder_helper3 t

let rec preorder (Node(d,ch)) =
 preorder_helper3(preorder_helper2 (Node(d, ch)))
                   
let rec mirror (Node(d,ch)) =
  match (Node(d, ch)) with
  |Node(d, []) -> Node(d, [])
  |Node(d, subtree) -> Node(d, List.map mirror ((List.rev) subtree)) 

let rec mapt f (Node(d,ch)) =
  match Node(d,ch) with
  | Node(d,ch) -> Node(f d, List.map (mapt f) ch)
  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) -> 
    match Node(d, ch) with
    | Node(d,ch) -> f d (List.map (foldt f) ch)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
foldt(fun n revr -> Node(n, (List.rev) revr)) t
