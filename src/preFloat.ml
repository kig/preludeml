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
(* Float operations *)

let round f = truncate (if f > 0.0 then f +. 0.5 else f -. 0.5)
(**T
  round 0.5 = 1
  round 0.4 = 0
  round (-0.4) = 0
  round (-0.5) = -1
**)
let ceiling = ceil
let quot f i = (truncate f) / i
(**T
  quot 5.0 2 = 2
**)
let recip f = 1. /. f
let signumf f = if f > 0. then 1. else if f < 0. then (-.1.) else 0.
let logBase base f = log f /. log base
let root rt f = f ** (recip rt)
let absf f = (signumf f) *. f
let pi = 4. *. atan 1.
let addf = (+.)
let subtractf a b = b -. a
let multiplyf = ( *. )
let dividef a b = b /. a
let negatef v = (-.v)
let average2f a b = (a +. b) /. 2.0
