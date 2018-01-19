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
	| h::t -> if (contains h b) then h::(set_intersection t b)
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
let rec periodic_point_helper f p x =
	if p=0 then x
	else periodic_point_helper f (p-1) (f x) 
 
let rec computed_periodic_point eq f p x =
	if eq (periodic_point_helper f p x) x then x 
	else computed_periodic_point eq f p (f x) 

(*Question 8*)
let rec while_away s p x =
	if (p x) then x::(while_away s p (s x) )
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
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*check whether a non-terminal lhs maps to itself*)
let rec symbol_is_valid symbol lst=
	match symbol with
	T s -> true
	| N s -> (contains s lst)
(* rule = (lhs, expr) lhs = symbol; expr = [T'3; N'Num] *)
let rec add_safe_symbols lhs rule lst =
	match rule with
	[] -> lhs::lst
	| h::t -> if (symbol_is_valid h lst) then add_safe_symbols lhs t lst
		  else lst
let rec populate_list exprs lst =
	match exprs with
	[] -> lst
	| (lhs, rule)::t -> populate_list t (add_safe_symbols lhs rule lst )

let populate_list_helper (a, b) =
	(a, populate_list a b)

(* list now contains all N that terminates*)
let rec rule_is_safe rule lst =
	match rule with
	[] -> true
	| h::t -> if (symbol_is_valid h lst) then (rule_is_safe t lst)
		  else false

let rec filter exprs lst =
	match exprs with
	[] -> []
	| h::t-> if (rule_is_safe (snd h) lst) then h::(filter t lst)
		  else filter t lst 
let equals_helper (a, b) (c, d) =
	equal_sets b d

let rec filter_blind_alleys g =
	let lhs_expr  = (fst g) in
	let rhs_exprs = (snd g) in
	let safe_list = (snd (computed_fixed_point equals_helper populate_list_helper (rhs_exprs, []))) in
	let filtered = (filter rhs_exprs safe_list) in
	(lhs_expr, filtered)
