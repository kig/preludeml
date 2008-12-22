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
