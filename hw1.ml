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
	subset a b && subset b a
	
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
	| h::t -> if (contains h b) then (set_diff t b)
		  else h::(set_diff t b)

(*Question 6*)
let rec computed_fixed_point eq f x =
	if eq (f x) x then x
	else computed_fixed_point eq f (f x)

(*Question 7*)
let rec periodic_point_helper eq f p x =
	match p with 
		0 -> if eq (f x) x then x
		     else None
		| _ -> periodic_point_helper eq f (p-1) (f x) 
 
let rec computed_periodic_point eq f p x =
	match (periodic_point_helper eq f p x) with 
		None -> computed_periodic_point eq f p (f x) 
		|_ -> x

(*Question 8*)
let rec while_away s p x =
	if (p x) then x::(while_away s p (p x) )
	else []

(*Question 9*)
let rec rle_decode_helper n v =
	if n = 0 then []
	else v::rle_decode_helper (n-1) v

let rec rle_decode lp =
	match lp with
	[] -> []
	| (n, v)::t -> (rle_decode_helper n v)::(rle_decode t) 

(*Question 10*)
(*let rec filter_blind_alleys g = *)
	
