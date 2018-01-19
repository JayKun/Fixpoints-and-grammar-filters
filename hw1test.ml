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
