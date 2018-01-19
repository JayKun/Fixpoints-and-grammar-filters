let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1]
let my_subset_test2 = not (subset [1] [])
let my_subset_test3 = subset [1;2;3] [3;2;1]
let my_subset_test4 = subset [1;1;1] [1]
let my_subset_test5 = subset [1] [1;1;2]
let my_subset_test6 = subset [(1, "a")] [(2, "b"); (1, "a")]
let my_subset_test7 = not (subset [1;2;3;4] [2;3;4;5])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2;3] [2;3;1]
let my_equal_sets_test2 = equal_sets [1;1;2] [1;2]
let my_equal_sets_test3 = equal_sets [[1;2;3];[1]] [[1];[1;2;3]]
let my_equal_sets_test4 = not (equal_sets [1;2;3] [1;2])
let my_equal_sets_test5 = not (equal_sets [] [1])

let my_set_union_test0 = equal_sets (set_union [1;2;3] [2;4;3]) [4;3;2;1]
let my_set_union_test1 = equal_sets (set_union [] [1;2]) [1;2]
let my_set_union_test2 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let my_set_union_test3 =
  equal_sets (set_union [[1;2];[3]] [[2;4];[1;2]]) [[1;2];[3];[2;4]]
let my_set_union_test4 = equal_sets (set_union [] []) []

let my_set_intersection_test0 =
  equal_sets (set_intersection [1;2;3] [2;3;4]) [2;3]
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;2;3] [4;5;6]) []
let my_set_intersection_test2 =
  equal_sets (set_intersection [4;1;2] [4;5;6]) [4]
let my_set_intersection_test3 =
  equal_sets (set_intersection [1;2;3] [3;2;1]) [2;3;1]
let my_set_intersection_test4 =
  equal_sets (set_intersection [] [3;2;1]) []

let my_set_deff_test0 = equal_sets (set_diff [] []) []
let my_set_deff_test1 = equal_sets (set_diff [1;2;3;4] [2;4]) [1;3]
let my_set_deff_test2 = equal_sets (set_diff [2;4] [1;2;3;4]) []
let my_set_deff_test3 = equal_sets (set_diff [1;2;3] [2;3;4]) [1]
let my_set_deff_test4 = equal_sets (set_diff [1;2;3] []) [1;2;3]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x*x - 3*x + 4) 2 = 2
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x) 'a' = 'a'
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) (fun x -> match x with
                                     | [] -> []
                                     | h::t -> t) [1;2;3] = []

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> -x) 2 1 = 1
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> if x = 5 then 0 else x + 1) 6 (-2) = 0
let my_computed_periodic_point_test2 =
  computed_periodic_point (=) List.rev 2 [1;2;3] = [1;2;3]

let my_while_away_test0 =
  while_away (( * ) 2) ((>) 10) 1 = [1;2;4;8]
let my_while_away_test1 =
  while_away (fun x -> 0::x) (fun x -> List.length x < 3) [] = [[]; [0]; [0;0]]

let my_rle_decode_test0 =
  rle_decode [2,0; 0,1; 3,2] = [0;0;2;2;2]
let my_rle_decode_test2 =
  rle_decode [] = []
let my_rle_decode_test3 =
  rle_decode [0,4;0,5] = []
let my_rle_decode_test4 =
  rle_decode [1,'h';1,'e';2,'l';1,'o'] = ['h';'e';'l';'l';'o']

type nonterminal = | S | A | B | C
let rules =
  [S, [N A];
   S, [N A; N B];
   A, [N B];
   A, [T "a"];
   B, [T "b"]]
let bad_rules =
  [S, [N A];
   S, [N B];
   A, [N B];
   B, [N A]]

let my_filter_blind_alleys_test0 =
  filter_blind_alleys (S, rules) = (S, rules)
let my_filter_blind_alleys_test1 =
  filter_blind_alleys (S, (B, [N C])::rules) = (S, rules)
let my_filter_blind_alleys_test2 =
  filter_blind_alleys (S, (B, [N C])::rules) = (S, rules)
let my_filter_blind_alleys_test3 =
  filter_blind_alleys (S, (C, [T "c"])::rules) = (S, (C, [T "c"])::rules)
let my_filter_blind_alleys_test4 =
  filter_blind_alleys (S, (A, [])::rules) = (S, (A, [])::rules)
let my_filter_blind_alleys_test5 =
  filter_blind_alleys (S, bad_rules) = (S, [])
let my_filter_blind_alleys_test6 =
  filter_blind_alleys (S, (B, [T 0])::bad_rules) = (S, (B, [T 0])::bad_rules)
