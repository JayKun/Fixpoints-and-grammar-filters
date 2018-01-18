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
let rec periodic_point_helper f p x =
	if p=0 then x
	else periodic_point_helper f (p-1) (f x) 
 
let rec computed_periodic_point eq f p x =
	if eq (periodic_point_helper f p x) x then x 
	else computed_periodic_point eq f p (f x) 

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
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*check whether a non-terminal lhs maps to itself*)
let rec check_rhs_for_lhs expr list =
	match expr with
	T expr -> true
	| N expr -> if contains expr list then true
		  else false

(* check whether symbol is a terminal or is in list of approved list*)
let rec symbol_is_safe sym list =
	match sym with 
	T s -> true
	| _ -> contains sym list

let rec rule_is_safe rule list =
	match rule with
	[]-> true
	| h::t -> if symbol_is_safe h list then rule_is_safe t list
		  else false
 
let rec blind_alley_helper rules list =
	match rules with
	[] -> []
	| h::t-> if (rule _is_safe (snd h) list ) then h::(blind_alley_helper t list)
		  else blind_alley_helper t list 

let rec populate_list rules list =
	match rules with
	[] -> list
	| h::t -> if rule_is_safe (snd h) list then populate_list t (fst h)::list

		  else populate_list t list
let rec filter_blind_alleys g =
	match g with
	(k, v) -> let expr  = k in
		  let rules = v in
	rules = populate_list rules []
	rules = blind_alley_helper rules []
	(expr, rules)
	
