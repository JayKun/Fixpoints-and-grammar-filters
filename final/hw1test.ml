(*subset*)
let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1] [])
let my_subset_test3 = subset [1;2;2;3] [3;3;2;1]
let my_subset_test4 = not (subset [7;1] [1;2;4])
let my_subset_test5 = subset [1;1;1] [1]
let my_subset_test6 = subset [1] [1;1;2]
let my_subset_test7 = subset [(2,2)] [(2,2); (1,1)]
let my_subset_test8 = not (subset [1;2;3;4] [2;3;4;5])

(*equal_sets*) 
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [1;2] [1])
let my_equal_sets_test2 = equal_sets [0;0;0;0;0;0] [0]
let my_equal_sets_test3 = equal_sets [0;1] [1;0]
let my_equal_sets_test4 = equal_sets [1;2;2] [1;2]
let my_equal_sets_test5 = equal_sets [[1;2;3];[1]] [[1];[1;2;3]]
let my_equal_sets_test6 = not (equal_sets [1;2;3] [1;2])

(*set_union*)
let set_union_test0 = equal_sets (set_union [1] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []
let set_union_test3 = not (equal_sets (set_union [] [2;2;3]) [1;2;3])
let set_union_test4 = not (equal_sets (set_union [1;1;5] [1;2;3]) [1;2;3])
let set_union_test5 = not (equal_sets (set_union [] [1]) [])

(*set_intersection*)
let my_set_intersection_test0 =
  equal_sets (set_intersection [1;2;3] [1;3;1]) [3;1]
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;1;1] [4;4;4]) []
let my_set_intersection_test2 =
  not (equal_sets (set_intersection [4;1;1;4;4;4] [4;5;6;1]) [4])
let my_set_intersection_test3 =
  not (equal_sets (set_intersection [1;2;2;3;4;3] [3;2;1;1;1;4]) [2;3;1])
let my_set_intersection_test4 =
  equal_sets (set_intersection [] [1;2;2;1]) []

(*set_diff*)
let set_diff_test0 = equal_sets (set_diff [1;3;3;1] [1;4;3;1]) []
let set_diff_test1 = equal_sets (set_diff [4;3;1;1;3;5] [1;3]) [4;5]
let set_diff_test2 = equal_sets (set_diff [4;3;1;1;3] []) [1;3;4]
let set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
let set_diff_test4 = equal_sets (set_diff [] []) []

(*computed_fixed_point*)
let computed_fixed_point_test0 =computed_fixed_point (=) (fun x -> x*x - 2*x) 2 = 0
let computed_fixed_point_test1 =computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 	(fun x -> x /. 2.) 10.) = 1.25)
let computed_fixed_point_test4 =computed_fixed_point (=) (fun x -> match x with []->[]
							| h::t -> t) [1;1;1;1;1;1;1] = []

(*computed_periodic_point*)
let computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> -1*x) 2 1 = 1

(*while_away*)
let my_while_away_test0 = while_away (( + ) 2) ((>) 5) 1 = [1;3]
let my_while_away_test1 = while_away (fun x -> 1::x) (fun x -> List.length x < 3) [1] = [[1];[1;1]]
let my_while_away_test2 = while_away (fun x -> (fst x, (snd x) + 1)) (fun x -> (snd x) < 3 ) (1,1) = [1,1;1,2]

(*rle_decode*)
let my_rle_decode_test0 = rle_decode [] = []
let my_rle_decode_test1 = rle_decode [1,1; 0,1; 3,2] = [1;2;2;2]
let my_rle_decode_test2 = rle_decode [0,0;0,0] = []
let my_rle_decode_test3 = rle_decode [1,1] = [1]
let my_rle_decode_test4 = rle_decode [1,'t';1,'i';1,'r';1,'e';1,'d'] = ['t';'i';'r';'e';'d']

(*filter_blind_alley*)
type awksub_nonterminals =
| Why | Am | I | So | Tired

let test_rules =
   [Why, [T"("; N Why; T")"];
    Why, [N Am];
    Why, [N Why; N I; N So];
    Why, [N I];
    Why, [N I; N Tired];
    Am, [T"$"; N Tired];
    I, [N I];
    I, [N Am];
    So, [T"+"];
    Tired, [T"0"];
  ]
let test_rules1 =
   [Why, [T"("; N Why; T")"];
    Why, [N Am];
    Why, [N Why; N I; N So];
    Why, [N I];
    Why, [N I; N Tired];
    Am, [T"$"; N Am];
    I, [N I];
    I, [N Am];
    So, [T"+"];
    Tired, [T"0"];
  ]


let g = Why, test_rules
let g1 = Why, test_rules1
let my_filter_blind_alleys_test0 = filter_blind_alleys g = g
let my_filter_blind_alleys_test1 = filter_blind_alleys g1 = (Why, [(So, [T "+"]); (Tired, [T "0"])])
