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
(* Integer operations *)

let average2 a b = (a + b) / 2
let quot_rem a b =
  let q = a / b in
  (q, a - (q*b))
let rem a b = a mod b
let even x = x mod 2 == 0
let odd x = x mod 2 == 1
let signum i = if i > 0 then 1 else if i < 0 then (-1) else 0
let succ x = x + 1
let pred x = x - 1
let add = (+)
let subtract a b = b - a
(**T
  map (subtract 10) (11--13) = [1; 2; 3]
**)
let multiply = ( * )
let divide a b = b / a
(**T
  map (divide 10) [10; 20; 30] = [1; 2; 3]
**)
let modulo a b = b mod a
(**T
  filter (modulo 2 |>. equals 0) (1--10) = [2; 4; 6; 8; 10]
**)
let negate v = (-v)

let rec gcd x y = match (abs x), (abs y) with
  | 0,0 -> invalid_arg "Prelude.gcd: gcd 0 0 is undefined"
  | x,0 -> x
  | x,y -> gcd y (rem x y)

let lcm x y = match x, y with
  | _,0 | 0,_ -> 0
  | x,y -> abs ((x / (gcd x y)) * y)
