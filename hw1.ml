(*helper functions*)

let rec contains e l =
	match l with
	[]->false
	| h::t -> if h=e then true
		  else (contains e t) 
let rec delete_el e l =
	match l with
	[] -> []
	| h::t -> if h=e then t
		  else h::(delete_el e t)
(*Homework 1*)

(*Question 1*)
let rec subset a b =
	match a with
	[] -> true
	| h::t -> if (contains h b) then subset t b
		  else false

(*Question 2*)
let rec equal_sets a b =
	match a with
	[] -> (match b with
	      [] -> true
	      | _ -> false)
        | h::t -> if ( contains h b ) then (equal_sets t (delete_el h b))
		  else false
	
	
(*Question 3*)
let rec set_union a b = 
	a@b

(*Question 4*)
let rec set_intersection a b =
	match a with
	[] -> []
	| h::t -> if (contains b h) then h::(set_intersection t b)
		  else (set_intersection t b)

(*Question 5*)
let rec set_diff a b =
	match a with
	[] -> []
	| h::t -> if (contains b h) then (set_diff t b)
		  else h::(set_diff t b)

(*Question 6*)
