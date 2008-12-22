(* Function combinators *)

let (@@) f x = f x
(**T
  map (multiply 2) @@ reverse (1--3) = [6; 4; 2]
  int @@ ceil @@ sqrt @@ float 144 = 12
**)
let (@.) f g x = f (g x)
(**T
  map (join "-" @. words) ["a b"; "b c"] = ["a-b"; "b-c"]
  (int @. ceil @. sqrt @. float) 144 = 12
**)
let (@..) f g x y = f (g x y)
(**T
  (reverse @.. zip) (1--3) (4--6) = [(3, 6); (2, 5); (1, 4)]
**)
let (@...) f g x y z = f (g x y z)
(**T
  (rev @... unfoldr) (lessOrEqualTo 3) (fupler succ) 1 = [1; 2; 3]
**)
let (@....) f g w x y z = f (g w x y z)
(**T
  (rev @.... unfoldrFilter) (lte 7) even (fupler succ) 1 = [2; 4; 6]
**)
let (|>) x f = f x
(**T
  (1--3) |> reverse |> map (multiply 2) = [6; 4; 2]
  144 |> float |> sqrt = 12.0
**)
let (|>.) f g x = g (f x)
(**T
  map (words |>. join "-") ["a b"; "b c"] = ["a-b"; "b-c"]
  map (float |>. sqrt |>. ceil |>. int) [1; 4; 9; 15] = [1; 2; 3; 4]
**)
let uncurry f (a, b) = f a b
(**T
  uncurry (+) (1,2) = 3
  uncurry (@) @@ unzip @@ map (fun x -> (x, x+3)) (1--3) = (1--6)
**)
let curry f a b = f (a, b)
(**T
  curry reverseTuple 1 2 = (2,1)
**)
let flip f a b = f b a
(**T
  (flip map (65--68)) chr = ['A'; 'B'; 'C'; 'D']
**)
let dup f x = f x x
(**T
  map (dup multiply) (1--3) = [1; 4; 9]
**)
let id x = x
(**T
  maybe 0 id (Some 1) = 1
**)
let const x y = x
(**T
  map (const 1) ['a'; 'b'; 'c'] = [1; 1; 1]
**)

