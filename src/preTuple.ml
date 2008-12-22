(* Tuple operations *)

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let reverseTuple (a,b) = (b,a)
let trev = reverseTuple
let fuple f g a = (f a, g a)
let fuplel f a = (f a, a)
let fupler f a = (a, f a)
