(* Comparisons *)

let lessThan x y = (<) y x
(**T
  filter (lessThan 3) (1--10) = [1; 2]
**)
let lessOrEqualTo x y = (<=) y x
(**T
  filter (lessOrEqualTo 3) (1--10) = [1; 2; 3]
**)
let greaterThan x y = (>) y x
(**T
  filter (greaterThan 7) (1--10) = [8; 9; 10]
**)
let greaterOrEqualTo x y = (>=) y x
(**T
  filter (greaterOrEqualTo 7) (1--10) = [7; 8; 9; 10]
**)

let eq = (=)
let neq = (<>)
let equals = (=)
(**T
  filter (modulo 2 |>. equals 0) (1--10) = [2; 4; 6; 8; 10]
**)

let lt = lessThan
let lte = lessOrEqualTo
let gt = greaterThan
let gte = greaterOrEqualTo

let between a b x = x >= a && x <= b
