(* Exception handling combinators *)

open PreCombinators
open PreOption

let maybeE v f o = try f o with _ -> v
(**T
  maybeE 0 last [] = 0
  maybeE 0 last [1] = 1
**)
let maybeEx ex v f o = try f o with e when ex = e -> v
(**T
  maybeEx Not_found 0 last [] = 0
  maybeEx Not_found 0 last [1] = 1
  (try maybeEx Not_found 0 raise Exit with Exit -> 1) = 1
**)
let maybeExl exl v f o =
  try f o with x -> if List.exists ((=) x) exl then v else raise x
(**T
  maybeExl [Not_found] 0 last [] = 0
  maybeExl [Not_found] 0 last [1] = 1
**)
let maybeEOF v f o = maybeEx End_of_file v f o
(*
  unfoldlOpt (maybeEOF None (fun ic -> Some (readInt ic, ic)) ic) stdin;;
*)
let maybeNF v f o = maybeEx Not_found v f o
(**T
  maybeNF 0 last [] = 0
  maybeNF 0 last [1;2;3] = 3
**)


(* Exceptions to options *)

let optE f o = maybeE None (some @. f) o
let optEx ex f o = maybeEx ex None (some @. f) o
let optExl exl f o = maybeExl exl None (some @. f) o
let optEOF f o = maybeEOF None (some @. f) o
let optNF f o = maybeEx Not_found None (some @. f) o
(**T
  optNF last [] = None
  optNF last [1;2;3] = Some 3
**)


let finally finaliser f x =
  let r = try f x with e ->
    ( try finaliser x with _ -> () );
    raise e in
  finaliser x;
  r
