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
(* Option combinators *)

let some x = Some x
let none x = None

let opt_or o y = match o with Some x -> x | None -> y
(**T
  opt_or None 0 = 0
  opt_or (Some 10) 0 = 10
**)
let (|?) = opt_or
let optOr y o = match o with Some x -> x | None -> y
(**T
  optOr 0 None = 0
  optOr 0 (Some 10) = 10
**)

let optMap f o = match o with Some x -> Some (f x) | None -> None

let maybe v f o = match o with Some x -> f x | None -> v
(**T
  maybe 0 parseInt (Some "10") = 10
  maybe 0 parseInt None = 0
**)
let unmaybe b f v = if b = v then None else Some (f v)
(**T
  unmaybe 0 showInt 10 = Some "10"
  unmaybe 0 showInt 0 = None
**)
let optIf p f v = if p v then Some (f v) else None
(**T
  optIf (greaterThan 0) (add 5) 0 = None
  optIf (greaterThan 0) (add 5) 1 = Some 6
  unfoldrOpt (optIf (greaterThan 0) (fun x -> (x, x-1))) 5 = [1;2;3;4;5]
**)
