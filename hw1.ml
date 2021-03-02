
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)


(*Jack Schneiderhan*)
(*I pledge my honor that I have abided by the Stevens Honor System.*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let a3 = {states = ["q0";"q1";"q2";"q3"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1", 'b', "q1");
                ("q1",'c', "q2"); ("q0",'a', "q3")];
          final = ["q2";"q3"]}

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec next tf state symbol = 
  match tf with 
  | [] -> []
  | (state1, symb, state2)::tail ->
      if state = state1 && symbol = symb 
      then state2 :: next tail state symbol
      else next tail state symbol

let rec get_second (a, b, c) = b

let rec get_symbols tf = 
  match tf with 
  | [] -> []
  | head::tail -> get_second head :: get_symbols tail


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)
let rec apply_transition_function tf state symbol = 
  match tf with
  | [] -> None
  | (state1, symb, state2)::tail when state = state1 && symbol = symb -> Some state2
  | _::tail -> apply_transition_function tail state symbol

(*Helper function for accept. Has to be down here because it won't be able to use apply_transition_function otherwise*)
let rec accept_helper fa state input =
    if input = [] then state else
      match state with
      |Some q -> accept_helper fa (apply_transition_function fa.tf q ((List.hd) input)) ((List.tl) input)
      |None -> None

let rec accept fa input =
  if accept_helper fa (Some fa.start) input = None
  then false
  else if (Some((List.hd) fa.final)) = accept_helper fa (Some fa.start) input 
  then true
  else false

let rec deterministic fa =
  let rec deterministic_helper states symbols =
      if symbols = []
      then true
      else if ((List.length) (next fa.tf ((List.hd) fa.states) ((List.hd) symbols))) > 1
      then false
      else deterministic_helper states ((List.tl) symbols)
  in let rec deterministic_helper2 states symbols =
    if states = []
    then true
    else if (deterministic_helper states symbols)
    then deterministic_helper2 ((List.tl) states) symbols
    else false
  in 
  if (deterministic_helper2 fa.states (get_symbols fa.tf))
  then true
  else false

let rec valid_helper finals states =
  if finals = []
  then true
  else if ((List.mem) ((List.hd) finals) states) 
  then valid_helper ((List.tl) finals) states
  else false

let rec valid fa =
   if (List.mem) fa.start fa.states 
   then if valid_helper fa.final fa.states
        then if deterministic fa 
             then true
             else false
        else false
   else false

let rec diff l1 l2 =
  match l1 with
  | [] -> []
  | h::t when List.mem h l2 -> diff t l2
  | h::t -> h :: diff t l2

let rec reachable fa =
    let rec reachable_helper visited current =
      match current with 
      | [] -> visited
      | h::t -> reachable_helper (h::visited) ((diff (next fa.tf fa.start ((List.hd) (get_symbols fa.tf))) visited) @ t)
  in reachable_helper [] [fa.start]

let rec remove_dead_states fa =
    let rec remove_dead_helper states dead =
      match dead with 
      | [] -> []
      | h::t -> if ((List.hd) states) = h
      then remove_dead_helper ((List.tl) states) t
      else remove_dead_helper states t
    in remove_dead_helper fa.states (reachable fa)