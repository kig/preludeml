(*
Prelude.ml: OCaml utility functions

Copyright (C) 2007-2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*)
(* Unfolds and recursion *)

open PreOption
open PreTuple

let rec loop f x = f x; loop f x
(*
  loop (print_endline @. input_line) stdin;;
*)
let rec recurseOpt f i = match f i with None -> i | Some x -> recurseOpt f x
let rec recurseWhile p f i = if p i then recurseWhile p f (f i) else i
let rec recurseUntil p f i = if p i then i else recurseUntil p f (f i)
let rec recurseTo n f i = if n = i then i else recurseTo n f (f i)
let rec recurseN f n i = if n <= 0 then i else recurseN f (n-1) (f i)

let unfoldrOpt f init =
  let rec aux f v l =
    match f v with
      | None -> l
      | Some (a, b) -> aux f b (a::l) in
  aux f init []
(**T
  unfoldrOpt (fun x -> if x > 3 then None else Some (x, x+1)) 1 = [3; 2; 1]
  unfoldrOpt (fun i -> if i > 67 then None else Some (char i, i+1)) 65 = ['C';'B';'A']
**)
let unfoldlOpt f init = List.rev (unfoldrOpt f init)
(**T
  unfoldlOpt (fun x -> if x > 3 then None else Some (x, x+1)) 1 = [1; 2; 3]
  unfoldlOpt (fun i -> if i > 67 then None else Some (char i, i+1)) 65 = ['A';'B';'C']
**)

let unfoldr p f init = unfoldrOpt (optIf p f) init
(**T
  unfoldr (lessThan 4) (fupler succ) 1 = [3; 2; 1]
  unfoldr (lessThan 68) (fuple char succ) 65 = ['C'; 'B'; 'A']
**)
let unfoldl p f init = List.rev (unfoldr p f init)
(**T
  unfoldl (lessThan 4) (fupler succ) 1 = [1; 2; 3]
  unfoldl (lessThan 68) (fuple char succ) 65 = ['A'; 'B'; 'C']
**)
let unfoldrWhile = unfoldr
let unfoldlWhile = unfoldl

let unfoldrUntil p f init = unfoldr (fun v -> not (p v)) f init
let unfoldlUntil p f init = unfoldl (fun v -> not (p v)) f init

let unfoldrFilter p s f init =
  let rec aux p f v l =
    if not (p v) then l
    else let a,b = f v in
         aux p f b (if s v then (a::l) else l) in
  aux p f init []
(**T
  unfoldrFilter (lt 7) even (fupler succ) 1 = [6; 4; 2]
  unfoldrFilter (lt 7) even (fuple (divide 2) succ) 2 = [3; 2; 1]
**)
let unfoldlFilter p s f init = List.rev (unfoldrFilter p s f init)
(**T
  unfoldlFilter (lt 7) even (fupler succ) 1 = [2; 4; 6]
  unfoldlFilter (lt 7) even (fuple (divide 2) succ) 2 = [1; 2; 3]
**)

let unfoldlN f n i =
  unfoldlWhile (fun (_,v) -> v > 0) (fun (s,c) -> (f s, (s, pred c))) (i, n)

let forN f n = for i=0 to (n-1) do f i done

let generateOpt f init =
  unfoldlOpt (fun x -> match f x with Some a -> Some (x,a) | None -> None) init
(**T
  generateOpt (fun x -> if x > 3 then None else Some (x+1)) 1 = [1; 2; 3]
**)
let generate p f init = unfoldl p (fupler f) init
(**T
  generate (lessOrEqualTo 3) succ 1 = [1; 2; 3]
**)
let generateUntil p f init = generate (fun v -> not (p v)) f init

let generateOptR f init =
  unfoldrOpt (fun x -> match f x with Some a -> Some (x,a) | None -> None) init
(**T
  generateOptR (fun x -> if x > 3 then None else Some (x+1)) 1 = [3; 2; 1]
**)
let generateR p f init = unfoldr p (fun x -> (x, f x)) init
(**T
  generateR (lte 3) succ 1 = [3; 2; 1]
**)
let generateUntilR p f init = generateR (fun v -> not (p v)) f init

let iterate f n s =
  let rec aux f n s rv i =
    if i >= n then List.rev rv
    else aux f n (f s) (s::rv) (i+1) in
  aux f n s [] 0
(**T
  iterate succ 10 1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  iterate pred 4 1 = [1; 0; -1; -2]
**)

