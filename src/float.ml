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
