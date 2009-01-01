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

include Printf

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
let uncurry3 f (a,b,c) = f a b c
let uncurry4 f (a,b,c,d) = f a b c d
let uncurry5 f (a,b,c,d,e) = f a b c d e
let uncurry6 f (a,b,c,d,e,g) = f a b c d e g
let curry f a b = f (a, b)
(**T
  curry reverseTuple 1 2 = (2,1)
**)
let curry3 f a b c = f (a,b,c)
let curry4 f a b c d = f (a,b,c,d)
let curry5 f a b c d e = f (a,b,c,d,e)
let curry6 f a b c d e g = f (a,b,c,d,e,g)
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


(* Option combinators *)

let some x = Some x
(**T
  some 10 = Some 10
**)
let none x = None
(**T
  none 10 = None
**)

let isSome o = (o <> None)
(**T
  isSome (Some 10) = true
  isSome (None) = false
**)

let opt_or o y = match o with Some x -> x | None -> y
(**T
  opt_or None 0 = 0
  opt_or (Some 10) 0 = 10
**)
let (|?) = opt_or
(**T
  Some 5 |? 0 = 5
  None |? 0 = 0
**)
let optOr y o = match o with Some x -> x | None -> y
(**T
  optOr 0 None = 0
  optOr 0 (Some 10) = 10
**)

let optMap f o = match o with Some x -> Some (f x) | None -> None
(**T
  optMap succ (Some 10) = Some 11
  optMap succ (None) = None
**)

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


(* Exception handling combinators *)

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
(**T
  optE last [] = None
  optE last [1] = Some 1
**)
let optEx ex f o = maybeEx ex None (some @. f) o
(**T
  optEx Not_found last [] = None
  optEx Not_found last [1] = Some 1
  optEx (Failure "hi") (fun _ -> failwith "hi") 0 = None
  optE (fun _ -> optEx (Failure "hi") (fun _ -> failwith "hi") 0) 0 = Some None
  optE (fun _ -> optEx (Failure "ho") (fun _ -> failwith "hi") 0) 0 = None
**)
let optExl exl f o = maybeExl exl None (some @. f) o
(**T
  optExl [Not_found] last [] = None
  optExl [Failure "hi"; Not_found] last [1] = Some 1
  optExl [Failure "hi"; Not_found] (fun _ -> failwith "hi") 0 = None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "hi") 0) 0 = Some None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "ho") 0) 0 = Some None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "ha") 0) 0 = None
**)
let optEOF f o = maybeEOF None (some @. f) o
(**T
  optEOF (fun i -> i) 0 = Some 0
  optEOF (fun i -> raise End_of_file) 0 = None
  optE (fun _ -> optEOF last []) 0 = None
**)
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
(**T
  (let i = ref 0 in finally (fun j -> j := 1) (fun j -> j := 2) i; !i = 1)
  (let i = ref 0 in finally (fun j -> j := 1) (fun j -> ()) i; !i = 1)
  (let i = ref 0 in ignore (optE (finally (fun j -> j := 1) (fun j -> failwith "")) i); !i = 1)
  optE (finally (fun j -> j := 1) (fun j -> failwith "")) (ref 0) = None
**)


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

let between a b x = (x >= a) && (x <= b)
(**T
  filter (between 2 6) (1--10) = (2--6)
**)


(* Tuples *)

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let tuple4 a b c d = (a,b,c,d)
let tuple5 a b c d e = (a,b,c,d,e)
let tuple6 a b c d e f = (a,b,c,d,e,f)
let reverseTuple (a,b) = (b,a)
let trev = reverseTuple
let fuple f g a = (f a, g a)
let fuplel f a = (f a, a)
let fupler f a = (a, f a)


(* Conversions *)

let array = Array.of_list
let list = Array.to_list
let int = int_of_float
let char = char_of_int
let parseInt = int_of_string
let parseFloat = float_of_string
let showInt = string_of_int
let showFloat f =
  Pcre.replace ~rex:(Pcre.regexp "\\.$") ~templ:".0" (string_of_float f)
let charCode = int_of_char
let ord = int_of_char
let chr = char_of_int
let string_of_char c = String.make 1 c
(**T
  string_of_char 'c' = "c"
**)
let char_of_string s =
  if String.length s <> 1 then None else Some (String.unsafe_get s 0)
(**T
  char_of_string "" = None
  char_of_string "foo" = None
  char_of_string "f" = Some 'f'
**)


(* Unfolds and recursion *)

let rec loop f x = f x; loop f x
(*
  loop (print_endline @. input_line) stdin;;
*)
let rec recurseOpt f i = match f i with None -> i | Some x -> recurseOpt f x
(**T
  recurseOpt (optIf (greaterThan 0 @. fst) (fun (i,l) -> (pred i, if even i then i::l else l))) (10, []) = (0, [2;4;6;8;10])
**)
let rec recurseWhile p f i = if p i then recurseWhile p f (f i) else i
(**T
  recurseWhile (greaterThan 0 @. fst) (fun (i,l) -> (pred i, if even i then i::l else l)) (10, []) = (0, [2;4;6;8;10])
**)
let rec recurseUntil p f i = if p i then i else recurseUntil p f (f i)
(**T
  recurseUntil (lessThan 0 @. fst) (fun (i,l) -> (pred i, if even i then i::l else l)) (10, []) = (-1, [0;2;4;6;8;10])
**)
let rec recurseTo n f i = if n = i then i else recurseTo n f (f i)
(**T
  recurseTo [] tail (1--100) = []
  recurseTo 100 succ 0 = 100
  recurseTo "foo" spopped "foobar" = "foo"
**)
let rec recurseN f n i = if n <= 0 then i else recurseN f (n-1) (f i)
(**T
  recurseN succ (-1) 5 = 5
  recurseN succ 0 5 = 5
  recurseN succ 1 5 = 6
  recurseN succ 10 5 = 15
**)

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
(**T
  unfoldrUntil (gte 4) (fupler succ) 1 = [3; 2; 1]
  unfoldrUntil (gte 68) (fuple char succ) 65 = ['C'; 'B'; 'A']
**)
let unfoldlUntil p f init = unfoldl (fun v -> not (p v)) f init
(**T
  unfoldlUntil (gte 4) (fupler succ) 1 = [1; 2; 3]
  unfoldlUntil (gte 68) (fuple char succ) 65 = ['A'; 'B'; 'C']
**)

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

let readN f n i =
  unfoldlWhile (fun (_,v) -> v > 0) (fun (s,c) -> (f s, (s, pred c))) (i, n)
(**T
  readN (fun i -> i := !i+1; !i) 10 (ref 0) = (1--10)
**)

let forN f n = for i=0 to (n-1) do f i done
(**T
  (let i = ref 0 in forN (fun j -> i := !i + j) 10; !i = sum (0--9))
**)

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
(**T
  generateUntil (gt 3) succ 1 = [1; 2; 3]
**)

let generateOptR f init =
  unfoldrOpt (fun x -> match f x with Some a -> Some (x,a) | None -> None) init
(**T
  generateOptR (fun x -> if x > 3 then None else Some (x+1)) 1 = [3; 2; 1]
**)
let generateR p f init = unfoldr p (fupler f) init
(**T
  generateR (lte 3) succ 1 = [3; 2; 1]
**)
let generateUntilR p f init = generateR (fun v -> not (p v)) f init
(**T
  generateUntilR (gt 3) succ 1 = [3; 2; 1]
**)

let iterate f n s =
  let rec aux f n s rv i =
    if i >= n then List.rev rv
    else aux f n (f s) (s::rv) (i+1) in
  aux f n s [] 0
(**T
  iterate succ 10 1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  iterate pred 4 1 = [1; 0; -1; -2]
**)


(* Low-level parallel combinators *)

let coreCount () =
  let countCores l =
    List.filter (Pcre.pmatch ~rex:(Pcre.regexp "^processor\\s*:")) l
    |> List.length in
  let withFile filename f = finally close_in f (open_in_bin filename) in
  optE (withFile "/proc/cpuinfo") (fun ic ->
    let rec readLines ic rv =
      match optEOF input_line ic with
        | None -> List.rev rv
        | Some l -> readLines ic (l::rv) in
    countCores (readLines ic [])
  )

let global_process_count = ref (coreCount () |? 4)

let invoke f x =
  flush stdout;
  let input, output = Unix.pipe() in
  match Unix.fork() with
  | -1 -> let v = f x in fun () -> v
  | 0 ->
      global_process_count := 1; (* no further implicit parallelization *)
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
      close_out output;
      exit 0
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = Marshal.from_channel input in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with `Res x -> x | `Exn e -> raise e
(**T
  invoke average (1--10) () = average (1--10)
  invoke aaveragef [|1.;2.;3.|] () = aaveragef [|1.;2.;3.|]
**)

(*
  The "par_"-functions are intended as plumbing to build other functions
  on top of. As such, their semantics are quite broken for common usage
  and you should use the "p*"-functions instead.
*)

(* lockstep iteration, unshifting new job after popped job completes
   a polling system would fare better with uneven job runtimes
*)
let par_iter ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let pop l =
    let rec aux l res =
      match l with
        | [] -> raise Not_found
        | (h::[]) -> (List.rev res, h)
        | (h::t) -> aux t (h :: res) in
    aux l [] in
  let rec aux f n procs l =
    let n,procs = if n >= process_count
    then match procs with [] -> 0,[] | lst ->
      let l, last = pop lst in
      (last (); ((n-1), l))
    else n,procs in
    match l with
      | [] -> List.iter (fun f -> f ()) procs
      | (h::t) -> aux f (n+1) ((invoke f h) :: procs) t in
  aux f 0 [] l

(* lockstep iteration, unshifting new job after popped job completes
   a polling system would fare better with uneven job runtimes
*)
let par_map ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let pop l =
    let rec aux l res =
      match l with
        | [] -> raise Not_found
        | (h::[]) -> (List.rev res, h)
        | (h::t) -> aux t (h :: res) in
    aux l [] in
  let rec aux f n procs res l =
    let n,res,procs = if n >= process_count
    then match procs with
        | [] -> 0,res,[]
        | lst -> let l,last = pop lst in
                 ((n-1), (last ())::res, l)
    else n,res,procs in
    match l with
      | [] -> (List.rev res) @ (List.rev_map (fun f -> f ()) procs)
      | (h::t) -> aux f (n+1) ((invoke f h) :: procs) res t in
  aux f 0 [] [] l
(**T
  par_map succ (1--10) = map succ (1--10)
**)

(*
  Splits n into process_count continuous segments,
  executes each in its own process.
*)
let pforN ?process_count f n =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float n /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (n - start) in
    for j = start to start+len-1 do
      f j
    done in
  par_iter ~process_count process (Array.to_list (Array.init process_count id))

let mapReduce partition distribute process combine input =
  partition input |> distribute process |> combine


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
(**T
  recip 42. = 1. /. 42.
  recip 3. = 1. /. 3.
**)
let signumf f = if f > 0. then 1. else if f < 0. then (-1.) else 0.
(**T
  signumf 42. = 1.
  signumf (-42.) = -1.
  signumf 0. = 0.
**)
let logBase base f = if base = 10. then log10 f else log f /. log base
(**T
  logBase 10. 1000. = log10 1000.
  absf (logBase 2. 64. -. 6.) < 0.0001
**)
let root rt f = f ** (recip rt)
(**T
  root 2. 4. = 2.
  root 4. 16. = 2.
**)
let absf = abs_float
(**T
  absf 0.1 = 0.1
  absf (-0.1) = 0.1
  absf (-0.0) = 0.0
  absf (0.0) = 0.0
**)
let pi = 4. *. atan 1.
(**T
  between 3.14 3.15 pi
  absf ((sin pi) -. 0.) < 0.00001
  absf ((cos pi) -. (-1.)) < 0.00001
  absf ((sin (2.*.pi)) -. 0.) < 0.00001
  absf ((cos (2.*.pi)) -. 1.) < 0.00001
**)
let addf = (+.)
(**T
  addf 1.0 2.0 = 1.0 +. 2.0
**)
let subtractf a b = b -. a
(**T
  (subtractf 1.0) 2.0 = 2.0 -. 1.0
**)
let multiplyf = ( *. )
(**T
  multiplyf 2.0 3.0 = 2.0 *. 3.0
**)
let dividef a b = b /. a
(**T
  (dividef 2.0) 3.0 = 3.0 /. 2.0
**)
let negatef v = (-.v)
(**T
  negatef 0. = 0.
  negatef 1. = -1.
  negatef (-1.) = 1.
**)
let average2f a b = (a +. b) /. 2.0
(**T
  average2f 2.0 3.0 = 2.5
  average2f 1.0 2.0 = 1.5
  average2f (-1.) 1. = 0.
**)


(* Integer operations *)

let average2 a b = (a + b) / 2
(**T
  average2 2 3 = 2
  average2 0 2 = 1
  average2 (-1) 1 = 0
**)
let quot_rem a b =
  let q = a / b in
  (q, a - (q*b))
(**T
  quot_rem 10 5 = (2, 0)
  quot_rem 10 3 = (3, 1)
  quot_rem (-10) 3 = (-3, -1)
  quot_rem (-10) (-3) = (3, -1)
**)
let rem a b = a mod b
(**T
  rem 10 5 = 0
  rem 10 3 = 1
  rem (-10) 3 = -1
  rem (-10) (-3) = -1
**)
let even x = x mod 2 == 0
(**T
  filter even (0--10) = map (multiply 2) (0--5)
**)
let odd x = x mod 2 == 1
(**T
  filter odd (0--10) = map (add 1 @. multiply 2) (0--4)
**)
let signum i = if i > 0 then 1 else if i < 0 then (-1) else 0
(**T
  signum 0 = 0
  signum 41 = 1
  signum (-41) = -1
**)
let succ x = x + 1
(**T
  succ 0 = 1
**)
let pred x = x - 1
(**T
  pred 0 = -1
**)
let add = (+)
(**T
  add 2 3 = 2 + 3
**)
let subtract a b = b - a
(**T
  map (subtract 10) (11--13) = [1; 2; 3]
**)
let multiply = ( * )
(**T
  multiply 2 3 = 2 * 3
**)
let divide a b = b / a
(**T
  map (divide 10) [10; 20; 30] = [1; 2; 3]
**)
let modulo a b = b mod a
(**T
  filter (modulo 2 |>. equals 0) (1--10) = [2; 4; 6; 8; 10]
**)
let negate v = (-v)
(**T
  negate 0 = 0
  negate 1 = -1
  negate (-1) = 1
**)

(* Greatest common divisor *)
let rec gcd x y = match (abs x), (abs y) with
  | 0,0 -> invalid_arg "Prelude.gcd: gcd 0 0 is undefined"
  | x,0 -> x
  | x,y -> gcd y (rem x y)
(**T
  gcd 12 4 = 4
  gcd 28 21 = 7
  gcd 21 28 = 7
  gcd 30 21 = 3
  gcd 21 30 = 3
**)

(* Least common multiple *)
let lcm x y = match x, y with
  | _,0 | 0,_ -> 0
  | x,y -> abs ((x / (gcd x y)) * y)
(**T
  lcm 12 4 = 12
  lcm 3 4 = 12
  lcm 8 70 = 280
  gcd (lcm 8 70) 8 = 8
  gcd (lcm 8 70) 70 = 70
**)

(* Time operations *)

let timeNow = Unix.gettimeofday
(**T
  timeNow () > 0.
**)
let timeZone = Netdate.localzone
(**T
  timeZone <= 60*12
  timeZone >= -60*12
**)
let formatTime ?(zone=timeZone) fmt f = Netdate.format ~fmt (Netdate.create ~zone f)
(**T
  formatTime ~zone:0 "%Y-%m-%d" 0. = "1970-01-01"
  formatTime ~zone:0 "%Y-%m-%d %H:%M:%S%z" 0. = "1970-01-01 00:00:00+0000"
  formatTime ~zone:0 "%Y-%m-%d %H:%M:%S%z" 3666. = "1970-01-01 01:01:06+0000"
  formatTime ~zone:(-480) "%Y-%m-%d %z" 0. = "1969-12-31 -0800"
**)
let showTime ?zone = formatTime ?zone "%Y-%m-%d %H:%M:%S%z"
(**T
  showTime ~zone:0 0. = "1970-01-01 00:00:00+0000"
**)
let showDate ?zone = formatTime ?zone "%Y-%m-%d"
(**T
  showDate ~zone:0 0. = "1970-01-01"
**)
let httpDate f = formatTime ~zone:0 "%a, %d %b %Y %H:%M:%S GMT" f
(**T
  httpDate 0. = "Thu, 01 Jan 1970 00:00:00 GMT"
**)

let second = 1.0
let minute = 60.0 *. second
let hour = 60.0 *. minute
let day = 24.0 *. hour

let sleep = Unix.sleep


(* User and group operations *)

let groupByName = Unix.getgrnam
let groupByGid = Unix.getgrgid

let groupGid name = (groupByName name).Unix.gr_gid
let groupName gid = (groupByGid gid).Unix.gr_name
let gidMembers gid = (groupByGid gid).Unix.gr_mem
let groupMembers name = (groupByName name).Unix.gr_mem

let userByName = Unix.getpwnam
let userByUid = Unix.getpwuid

let userUid name = (userByName name).Unix.pw_uid
let userGid name = (userByName name).Unix.pw_gid
let userGroup name = groupName (userGid name)
let userGecos name = (userByName name).Unix.pw_gecos
let userDir name = (userByName name).Unix.pw_dir
let userShell name = (userByName name).Unix.pw_shell
let userName uid = (userByUid uid).Unix.pw_name


(* List *)
module PreList =
struct
  include List

  let reverse = rev

  let nth i l = List.nth l i
  let ($$) = List.nth

  let cons x xs = x::xs
  let head = function [] -> raise Not_found | (h::_) -> h
  let tail = function [] -> raise Not_found | (_::t) -> t
  let pop l =
    let rec aux l res =
      match l with
        | [] -> raise Not_found
        | (h::[]) -> (rev res, h)
        | (h::t) -> aux t (h :: res) in
    aux l []
  (**T
    pop [1;2;3] = ([1;2], 3)
  **)
  let popped l = fst (pop l)
  (**T
    popped [1; 2; 3] = [1; 2]
  **)
  let last l = snd (pop l)
  (**T
    last [1; 2; 3] = 3
  **)
  let first = head

  let shift l = (tail l, head l)
  let unshift = cons

  let map f l = rev (rev_map f l)

  let rec assocBy f l =
    match l with
      | [] -> raise Not_found
      | (k,v)::t when f k -> v
      | _::t -> assocBy f t

  let lookup e l = optNF (assoc e) l
  let lookupBy f e l = optNF (assocBy f e) l

  let len = length

  let all = for_all
  let any = exists

  let allEqual l = match l with
    | [] -> true
    | (h::t) -> all ((=) h) t

  let includes x = exists (fun y -> x = y)
  let has = includes
  let elem = includes
  let notElem x lst = not @@ elem x lst

  let indexOf x lst =
    let rec aux x c l = match l with
      | [] -> raise Not_found
      | (h::t) when x = h -> c
      | (h::t) -> aux x (c+1) t in
    aux x 0 lst

  let filterWithIndex f s =
    let rec aux f l i res =
      match l with
        | [] -> rev res
        | (h::t) when f h i -> aux f t (succ i) (h::res)
        | (h::t) -> aux f t (succ i) res in
    aux f s 0 []
  (**T
    filterWithIndex (fun _ i -> i > 5) (1--9) = (7--9)
  **)

  (**T
    indexOf 'a' (explode "foobar") = 4
  **)
  let findIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h then c else aux p (c+1) t in
    aux p 0 lst
  (**T
    findIndex (gt 4) (0--9) = 5
  **)
  let findWithIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h then (h,c) else aux p (c+1) t in
    aux p 0 lst
  (**T
    findWithIndex (gt 4) (2--9) = (5,3)
  **)

  let null = function [] -> true | _ -> false

  let concatMap f l = concat (map f l)
  (**T
    concatMap ((--) 1) [1;2;3] = [1; 1; 2; 1; 2; 3]
  **)

  let pick indices l = map (flip nth l) indices
  (**T
    pick [2; 3] (explode "foobar") = ['o'; 'b']
  **)
  let pickWith funcs l = map ((|>) l) funcs
  (**T
    pickWith [first; last] (explode "foobar") = ['f'; 'r']
  **)

  let span f lst =
    let rec aux f res l = match l with
      | (h::t) when f h -> aux f (h::res) t
      | x -> (rev res, x) in
    aux f [] lst
  (**T
    span id [true; false; false; true] = ([true], [false; false; true])
    span (lessOrEqualTo 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
  **)
  let break p = span (not @. p)
  (**T
    break id [false; false; true; false] = ([false; false], [true; false])
    break (greaterThan 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
  **)

  let takeWhile f lst = fst @@ span f lst
  let take n lst =
    let rec aux c res l = match c, l with
        | x, (h::t) when x > 0 -> aux (c-1) (h::res) t
        | _ -> rev res in
    aux n [] lst

  let rec dropWhile f lst = match lst with
    | (h::t) when f h -> dropWhile f t
    | _ -> lst
  let rec drop n lst = match n, lst with
    | x, (h::t) when x > 0 -> drop (n-1) t
    | _ -> lst

  let rec dropWhile2 f a b = match a,b with
    | (x::xs), (y::ys) when f x y -> dropWhile2 f xs ys
    | _ -> a,b
  let rec drop2 n a b = match n,a,b with
    | c, (x::xs), (y::ys) when c > 0 -> drop2 c xs ys
    | _ -> a,b

  let splitAt n xs = (take n xs, drop n xs)
  (**T
    splitAt 3 (explode "foobar") = (['f'; 'o'; 'o'], ['b'; 'a'; 'r'])
  **)

  let sub first len lst =
    let rec f l fst ln c res = match l with
      | [] -> res
      | h::t when c >= (fst + ln) -> res
      | h::t when c >= fst -> f t fst ln (c+1) (h::res)
      | h::t -> f t fst ln (c+1) res in
    let first = if first < 0 then length lst + first else first in
    List.rev (f lst first len 0 [])
  (**T
    sub 2 3 (explode "foobar") = ['o'; 'b'; 'a']
    sub (-3) 2 (explode "foobar") = ['b'; 'a']
  **)
  let slice first last lst =
    let len = if first < 0 || last < 0 then length lst else 0 in
    let first = if first < 0 then len + first else first in
    let last = if last < 0 then len + last else last in
    sub first (last-first+1) lst
  (**T
    slice 2 3 (explode "foobar") = ['o'; 'b']
    slice (-3) (-1) (explode "foobar") = ['b'; 'a'; 'r']
  **)

  let interlace elem l =
    let rec aux l l2 = match l with
        | [] -> (match l2 with [] -> [] | (h::t) -> List.rev t)
        | (h::t) -> aux t (elem :: h :: l2) in
    aux l []
  (**T
    interlace 0 [1; 2; 3] = [1; 0; 2; 0; 3]
    implode @@ interlace '-' @@ explode "abcde" = "a-b-c-d-e"
  **)

  let compact l = map (function Some x -> x | _ -> failwith "compact")
                      (filter ((!=) None) l)
  (**T
    compact [None; Some 10; Some 5; None; None; Some 8] = [10; 5; 8]
    compact @@ map (optIf (greaterThan 0) id) (-3--3) = [1; 2; 3]
  **)

  let squeeze l =
    let rec aux x l1 l2 = match l1 with
      | [] -> (rev l2)
      | (h::t) when h = x -> aux x t l2
      | (h::t) -> aux h t (h::l2)
    in
    match l with [] -> [] | (h::t) -> aux h t [h]
  (**T
    squeeze [1;2;2;2;3;3;1] = [1; 2; 3; 1]
    squeeze @@ sort [1;2;2;2;3;3;1] = [1; 2; 3]
  **)

  let sort ?(cmp=compare) l = List.sort cmp l
  let sortBy ?(cmp=compare) f l =
    map (fupler f) l |> sort ~cmp:(fun (_,a) (_,b) -> cmp a b) |> map fst
  let uniq ?cmp l = squeeze (sort ?cmp l)
  (**T
    uniq [3;1;2;2;2;3;3;1] = [1; 2; 3]
  **)

  let reject f l = filter (not @. f) l
  (**T
    reject (gt 4) (1--5) = (1--4)
  **)

  let without x l = filter ((<>) x) l
  (**T
    without 4 [1; 2; 4; 1; 2; 4] = [1; 2; 1; 2]
  **)

  let rec neighbours item items = match items with
    | (p::i::n::t) when i == item -> (Some p, Some n)
    | (i::n::t) when i == item -> (None, Some n)
    | (p::i::[]) when i == item -> (Some p, None)
    | (h::t) -> neighbours item t
    | [] -> (None, None)
  (**T
    neighbours 2 (1--10) = (Some 1, Some 3)
    neighbours 10 (1--10) = (Some 9, None)
    neighbours 1 (1--10) = (None, Some 2)
    neighbours 0 (1--10) = (None, None)
    neighbours 11 (1--10) = (None, None)
  **)

  let neighbourLists item n items =
    let rec aux prev lst =
      match lst with
        | [] -> ([], [])
        | (i::[]) when i = item -> (prev, [])
        | (i::t) when i = item -> (prev, take n t)
        | (h::t) -> aux (take n (h::prev)) t
    in
    aux [] items
  (**T
    neighbourLists 5 2 (1--10) = ([4; 3], [6; 7])
    neighbourLists 7 3 (1--10) = ([6; 5; 4], [8; 9; 10])
    neighbourLists 2 5 (1--10) = ([1], [3; 4; 5; 6; 7])
    neighbourLists 9 3 (1--10) = ([8; 7; 6], [10])
    neighbourLists 0 4 (1--10) = ([], [])
  **)

  let mapWindow f n l =
    let rec aux f wnd lst res =
      match lst with
        | [] -> rev res
        | (h::t) ->
          let wnd = tail wnd @ [h] in
          aux f wnd t ((f wnd) :: res) in
    let wnd, t = splitAt n l in
    aux f wnd t [f wnd]
  (**T
    mapWindow sum 1 (1--4) = (1--4)
    mapWindow sum 2 (1--4) = [3; 5; 7]
    mapWindow sum 3 (1--4) = [6; 9]
  **)

  let foldl = fold_left
  (**T
    foldl (+) 0 (1--10) = 55
    foldl (fun s b -> s ^ (string_of_int b)) "--" (1--3) = "--123"
  **)
  let foldl1 f l = foldl f (head l) (tail l)
  (**T
    foldl1 (+) (1--10) = 55
    foldl1 (fun s i -> s ^ i) ["foo"; "bar"; "baz"] = "foobarbaz"
  **)

  let foldr f s l = fold_right f l s
  (**T
    foldr (+) 0 (1--10) = 55
    foldr (fun a s -> s ^ (string_of_int a)) "--" (1--3) = "--321"
  **)
  let foldr1 f l = let l,i = pop l in foldr f i l
  (**T
    foldr1 (+) (1--10) = 55
    foldr1 (fun a s -> s ^ a) ["foo"; "bar"; "baz"] = "bazbarfoo"
  **)

  let scanl f init lst = rev @@ snd @@
    foldl (fun (s,l) i -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanl multiply 1 (2--5) = [1; 2; 6; 24; 120]
  **)
  let scanl1 f l = scanl f (head l) (tail l)
  (**T
    scanl1 multiply (1--5) = [1; 2; 6; 24; 120]
  **)

  let scanr f init lst = snd @@
    foldr (fun i (s,l) -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanr multiply 1 @@ [5;4;3;2] = [120; 24; 6; 2; 1]
  **)
  let scanr1 f l = let l,i = pop l in scanr f i l
  (**T
    scanr1 multiply @@ [5;4;3;2;1] = [120; 24; 6; 2; 1]
  **)


  let zipWith f a b =
    let rec aux f a b l = match a,b with
        | (x::xs), (y::ys) -> aux f xs ys ((f x y)::l)
        | _ -> l in
    rev @@ aux f a b []
  let zip a b = zipWith tuple a b
  let unzip = split

  let rec zipWith3 f a b c = match a,b,c with
    | (h1::t1), (h2::t2), (h3::t3) -> (f h1 h2 h3) :: (zipWith3 f t1 t2 t3)
    | _ -> []
  let zip3 a b c = zipWith3 tuple3 a b c
  let unzip3 l =
    foldr (fun (a,b,c) (t1,t2,t3) -> (a::t1, b::t2, c::t3)) ([],[],[]) l

  let iterWithIndex f l = ignore (foldl (fun j i -> f i j; j+1) 0 l)
  let each = iter
  let eachWithIndex = iterWithIndex
  let mapWithIndex f l =
    rev (snd (foldl (fun (j,r) i -> (j+1, (f i j)::r)) (0, []) l))

  let diffSorted a b =
    let rec aux a b l =
      match b with
        | [] -> (rev l) @ a
        | (x::xs) -> begin
          match a with
            | [] -> rev l
            | (y::ys) ->
              if y = x then aux ys xs l
              else if y > x then aux a xs l
              else aux ys b (y::l)
        end in
    aux a b []
  (**T
    diffSorted (1--10) (5--15) = [1; 2; 3; 4]
    diffSorted (5--15) (1--10) = [11; 12; 13; 14; 15]
    diffSorted [3;2;1] [1;0] = [3; 2; 1]
  **)

  let diff a b =
    let rec aux a b l =
      match b with
        | [] -> (rev l) @ a
        | (x::xs) -> begin
          match a with
            | [] -> rev l
            | ((y,i)::ys) ->
              if y = x then aux ys xs l
              else if y > x then aux a xs l
              else aux ys b ((y,i)::l)
        end in
    let diffs =
      aux (List.sort (fun (y,_) (y',_) -> compare y y') (mapWithIndex tuple a))
          (sort b) [] in
    map fst (List.sort (fun (_,i) (_,i') -> compare i i') diffs)
  (**T
    diff (1--10) (5--15) = [1; 2; 3; 4]
    diff (5--15) (1--10) = [11; 12; 13; 14; 15]
    diff [3;2;1] [1;0] = [3; 2]
  **)

  let product lst = foldl ( * ) 1 lst
  let productf lst = foldl ( *. ) 1. lst
  let sum lst = foldl (+) 0 lst
  let sumf lst = foldl (+.) 0. lst
  let average lst = (sum lst) / (length lst)
  let averagef lst = (sumf lst) /. (float (length lst))

  let cycle n l =
    let rec aux c lst res =
      if c == 0 then res
      else match lst with
            | [] -> aux c l res
            | (h::t) -> aux (c-1) t (h::res) in
    match l with
      | [] -> invalid_arg "cycle"
      | _ -> reverse @@ aux n l []
  (**T
    cycle 5 (1--3) = [1; 2; 3; 1; 2]
    cycle 3 (1--10) = [1; 2; 3]
  **)

  let range s e =
    if s <= e
    then generateR (greaterOrEqualTo s) pred e
    else generateR (lessOrEqualTo s) succ e
  (**T
    range 1 3 = [1; 2; 3]
    range 1 1 = [1]
    range 1 0 = [1; 0]
  **)
  let init f n =
    let rec aux f n res =
      if n < 0 then res
      else aux f (n-1) ((f n) :: res) in
    aux f (n-1) []
  (**T
    init succ 10 = (1--10)
    init pred 10 = ((-1)--8)
  **)
  let step d s e =
    if d == 0 then failwith "Prelude.step: zero step" else
    if s == e then [s] else
    if s < e
    then (if d < 0 then [] else generate (lte e) (add d) s)
    else (if d > 0 then [] else generate (gte e) (add d) s)
  (**T
    step 2 0 5 = [0; 2; 4]
    step 2 1 5 = [1; 3; 5]
    step (-2) 5 1 = [5; 3; 1]
    step (-2) 5 0 = [5; 3; 1]
  **)
  let (--) = range
  (**T
    (1--3) = [1; 2; 3]
    (1--1) = [1]
    (1--0) = [1; 0]
  **)

  let replicate n v = init (const v) n
  (**T
    replicate 5 '-' = ['-'; '-'; '-'; '-'; '-']
    replicate 0 '-' = []
    replicate (-1) '-' = []
  **)
  let times n l = concat (replicate n l)
  (**T
    times 3 [1; 2; 3] = [1; 2; 3; 1; 2; 3; 1; 2; 3]
  **)

  let maximum lst = foldl1 max lst
  (**T
    maximum [1;2;3;0;1;4;3;1] = 4
  **)
  let maxBy f a b = if (f a) >= (f b) then a else b
  let maximumBy f lst = foldl1 (maxBy f) lst
  let maximumByWith f lst = maximumBy snd (map (fupler f) lst)
  let minimum lst = foldl1 min lst
  (**T
    minimum [1;2;3;0;1;4;3;1] = 0
  **)
  let minBy f a b = if (f a) <= (f b) then a else b
  let minimumBy f lst = foldl1 (minBy f) lst
  let minimumByWith f lst = minimumBy snd (map (fupler f) lst)

  let groupsOf n l = if n <= 1 then [l]
    else unfoldlUntil null (splitAt n) l
  (**T
    groupsOf 3 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
  **)
  let splitInto n l = if n <= 1 then [l]
    else groupsOf (int (ceil (float (len l) /. float n))) l
  (**T
    splitInto 4 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
  **)
  let groupBy p l =
    let rec aux p v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when p v h -> aux p v t (h::rl) res
      | (h::t) -> aux p h t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev (aux p h t [h] [])
  (**T
    groupBy (fun x y -> x*x = y*y) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
  **)
  let groupAs f l =
    let rec aux f v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when (f h) = v -> aux f v t (h::rl) res
      | (h::t) -> aux f (f h) t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev @@ aux f (f h) t [h] []
  (**T
    groupAs (fun x -> x*x) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
  **)
  let group l = groupAs id l
  (**T
    group [1;1;2;2;3;1] = [[1;1]; [2;2]; [3]; [1]]
  **)
  let count p l =
    let rec aux c p l = match l with
      | [] -> c
      | (h::t) -> aux (c + (if p h then 1 else 0)) p t in
    aux 0 p l
  (**T
    count (gt 5) (0--10) = 5
  **)
  let rotate n l =
    let len = length l in
    let n = (-n) mod len in
    let n = if n >= 0 then n else len + n in
    uncurry (@) (reverseTuple (splitAt n l))
  (**T
    rotate 1 [1;2;3] = [3;1;2]
    rotate 2 [1;2;3] = [2;3;1]
    rotate 3 [1;2;3] = [1;2;3]
    rotate (-1) [1;2;3] = [2;3;1]
    rotate (-2) [1;2;3] = [3;1;2]
    rotate (-3) [1;2;3] = [1;2;3]
  **)



  (* Parallel iterators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce concat (map f)
  let pfilter f = pmapReduce concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let pfoldl1SeqN ?process_count n f l =
    pfoldlSeqN ?process_count n f f (first l) (tail l)

  let piterSeqN ?process_count n r f l =
    iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)


  let pinit ?process_count f l =
    let process_count = process_count |? !global_process_count in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    concat (par_map ~process_count (uncurry (zipWith f)) (zip aspl bspl))

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l
      |> mapWithIndex tuple
      |> par_map  ~process_count process |> combine

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process

  let pmapWithInit init f =
    pmapReduceWithIndex concat (fun (sublist, idx) -> map f (init sublist idx))
end

include PreList

let (--) = range
let (@*) l n = times n l
(**T
  [1; 2; 3] @* 3 = [1; 2; 3; 1; 2; 3; 1; 2; 3]
**)




module PreArray =
struct
  include Array

  let len = length
  (**T
    alen (1--|10) = 10
    alen [||] = 0
  **)

  (* Basic operations *)

  let init f l = init l f
  (**T
    ainit succ 10 = (1--|10)
    ainit pred 10 = (-1--|8)
  **)

  let range s e =
    if s > e
    then init ((-) s) (s-e+1)
    else init ((+) s) (e-s+1)
  (**T
    arange 0 1 = [|0; 1|]
    arange 2 4 = [|2; 3; 4|]
    arange 2 0 = [|2; 1; 0|]
  **)

  (**T
    (1--|10) = array (1--10)
    (10--|1) = array (10--1)
  **)

  let reverse (s : 'a array) =
    let len = length s in
    if len = 0 then s else
    let s2 = make len (unsafe_get s 0) in
    let mlen = len - 1 in
    for i=0 to mlen do
      unsafe_set s2 (mlen-i) (unsafe_get s i)
    done;
    s2
  let rev = reverse
  (**T
    arev (1--|10) = (10--|1)
    arev [|1|] = [|1|]
    arev (aexplode "foobar") = aexplode "raboof"
    arev [||] = [||]
  **)
  let normalizeIndex i s = if i < 0 then (len s) + i else i
  (**T
    PreArray.normalizeIndex 0 [||] = 0
    PreArray.normalizeIndex 0 (1--|10) = 0
    PreArray.normalizeIndex 2 (1--|10) = 2
    PreArray.normalizeIndex (-1) (1--|10) = 9
    PreArray.normalizeIndex (-2) (1--|10) = 8
    PreArray.normalizeIndex (-1) (1--|2) = 1
    PreArray.normalizeIndex (-2) (1--|2) = 0
  **)

  let times n a = PreList.replicate n a |> concat
  (**T
    atimes 3 (1--|3) = [|1;2;3;1;2;3;1;2;3|]
    atimes 0 (1--|3) = [||]
    atimes 1 (1--|3) = (1--|3)
  **)

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)

  (* Searching *)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let filterWithIndex f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c i then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let findWithIndex f s =
    let rec aux f s i len =
      if i >= len then raise Not_found
      else
        let v = unsafe_get s i in
        if f v i then (v, i)
        else aux f s (i+1) len in
    aux f s 0 (len s)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)

  let indexOf v s = findIndex ((=) v) s

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3

  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a

  let maximum a = foldl1 max a
  let minimum a = foldl1 min a

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)

  (* Subsequences *)

  let sub i len s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun x -> unsafe_get s (i+x)) (j-i+1)

  let slice_to_sub i j s =
    let i = normalizeIndex i s
    and j = normalizeIndex j s + (if j < 0 then 1 else 0) in
    let len = j - i in
    i, len

  let slice i j s =
    let i, len = slice_to_sub i j s in
    sub i len s

  let subStride stride i len a =
    let i = normalizeIndex i a in
    if i + (len-1) * stride >= length a
    then invalid_arg "subStride: index out of bounds";
    init (fun j -> unsafe_get a (i + j*stride)) len


  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  let tail a = slice 1 (-1) a

  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  let popped a = slice 0 (-2) a

  let pop a = (popped a, last a)
  let push v a = append a [|v|]

  let shift a = (tail a, first a)
  let unshift v a = append [|v|] a

  let take n s = sub 0 n s
  let takeWhile f s = sub 0 (findIndex (fun v -> not (f v)) s + 1) s

  let drop n s = sub (-n) n s
  let dropWhile f s = sub (findIndex (fun v -> not (f v)) s) (len s) s

  let splitAt n xs = (take n xs, drop n xs)

  let break f s = splitAt (findIndex f s) s
  let span f s = break (fun v -> not (f v)) s

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (2 * len s - 1)

  let reject f s = filter (fun v -> not (f v)) s
  let without v s = filter ((<>) v) s

  let groupsOf n a =
    let count, rem = quot_rem (len a) n in
    unfoldrWhile (gte 0) (fun i -> sub (i*n) n a, i-1) (count-1) @
    if rem = 0 then [] else [sub (-rem) rem a]

  let splitInto n range =
    let len = len range in
    let plen = int (ceil (float len /. float n)) in
    groupsOf plen range


  (* Subsequence iterators *)

  let iterSub i len f s =
    let i = normalizeIndex i s in
    for j=i to i+len-1 do f (unsafe_get s j) done

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s

  let mapSub i len f s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun j -> f (unsafe_get s (i+j))) (j-i)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldl1Sub i len f s =
    let i = normalizeIndex i s in
    if i < 0 || i >= length s then raise Not_found;
    foldlSub (i+1) (len-1) f (unsafe_get s i) s

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldr1Sub i len f s =
    let i = normalizeIndex i s in
    let j = i + len - 1 in
    if j < 0 || j >= length s then raise Not_found;
    foldrSub i (len-1) f (unsafe_get s j) s


  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s

  let sum a = foldl (+) 0 a
  let sumf a = foldl (+.) 0. a
  let product a = foldl ( * ) 1 a
  let productf a = foldl ( *. ) 1. a
  let average a = sum a / len a
  let averagef a = sumf a /. float (len a)

  let sumSub i len a = foldlSub i len (+) 0 a
  let sumSubf i len a = foldlSub i len (+.) 0. a

  let sumSlice i j a = foldlSlice i j (+) 0 a
  let sumSlicef i j a = foldlSlice i j (+.) 0. a

  let productSub i len a = foldlSub i len ( * ) 1 a
  let productSubf i len a = foldlSub i len ( *. ) 1. a

  let productSlice i j a = foldlSlice i j ( * ) 1 a
  let productSlicef i j a = foldlSlice i j ( *. ) 1. a

  let averageSub i len a = sumSub i len a / len
  let averageSubf i len a = sumSubf i len a /. float len

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (gte l) indices then invalid_arg "pick: Index out of bounds";
    PreList.map (fun i -> unsafe_get s i) indices

  let pickWith funcs s = PreList.map (fun f -> f s) funcs


  (* Parallel operations *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce concat (map f)
  let pfilter f = pmapReduce concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)

  let pinit ?process_count f l =
    let process_count = process_count |? !global_process_count in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process

  let pmapWithInit init f =
    pmapReduceWithIndex concat (fun (sublist, idx) -> map f (init sublist idx))
end




module PreString =
struct
  include String

  let init f l =
    let s = create l in
    for i=0 to l-1 do unsafe_set s i (f i) done;
    s

  let reverse s =
    let len = length s in
    let s2 = create len in
    let mlen = len - 1 in
    for i=0 to mlen do
      unsafe_set s2 (mlen-i) (unsafe_get s i)
    done;
    s2
  let rev = reverse
  let len = length
  let normalizeIndex i s = if i < 0 then (len s) + i else i

  let times n a = PreList.replicate n a |> concat ""

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)

  let mapToList f s = PreList.init (fun i -> f (unsafe_get s i)) (len s)
  let mapToArray f s = PreArray.init (fun i -> f (unsafe_get s i)) (len s)

  (* Conversions *)

  let to_array s = PreArray.init (unsafe_get s) (len s)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)

  let to_list s = PreList.init (unsafe_get s) (len s)
  let of_list l = of_array (Array.of_list l)

  let to_byte_array s = PreArray.init (fun i -> ord (unsafe_get s i)) (len s)
  let of_byte_array a = init (fun i -> chr (Array.unsafe_get a i)) (Array.length a)

  (* Searching *)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let filterWithIndex f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c i then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let findWithIndex f s =
    let rec aux f s i len =
      if i >= len then raise Not_found
      else
        let v = unsafe_get s i in
        if f v i then (v, i)
        else aux f s (i+1) len in
    aux f s 0 (len s)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)

  let indexOf v s = findIndex ((=) v) s

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3

  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a

  let maximum = foldl1 max
  let minimum = foldl1 min

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)

  (* Subsequences *)

  let sub i len s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun x -> unsafe_get s (i+x)) (j-i+1)

  let slice_to_sub i j s =
    let i = normalizeIndex i s
    and j = normalizeIndex j s + (if j < 0 then 1 else 0) in
    let len = j - i in
    i, len

  let slice i j s =
    let i, len = slice_to_sub i j s in
    sub i len s

  let subStride stride i len a =
    let i = normalizeIndex i a in
    if i + (len-1) * stride >= length a
    then invalid_arg "subStride: index out of bounds";
    init (fun j -> unsafe_get a (i + j*stride)) len


  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  let tail = slice 1 (-1)

  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  let popped = slice 0 (-2)

  let append = (^)

  let pop a = (popped a, last a)
  let push v a = append a (string_of_char v)

  let shift a = (tail a, first a)
  let unshift v a = append (string_of_char v) a

  let take n s = sub 0 n s
  let takeWhile f s = sub 0 (findIndex (fun v -> not (f v)) s + 1) s

  let drop n s = sub (-n) n s
  let dropWhile f s = sub (findIndex (fun v -> not (f v)) s) (len s) s

  let splitAt n xs = (take n xs, drop n xs)

  let break f s = splitAt (findIndex f s) s
  let span f s = break (fun v -> not (f v)) s

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (2 * len s - 1)

  let reject f s = filter (fun v -> not (f v)) s
  let without v s = filter ((<>) v) s

  let groupsOf n a =
    let count, rem = quot_rem (len a) n in
    unfoldrWhile (gte 0) (fun i -> sub (i*n) n a, i-1) (count-1) @
    if rem = 0 then [] else [sub (-rem) rem a]

  let splitInto n range =
    let len = len range in
    let plen = int (ceil (float len /. float n)) in
    groupsOf plen range


  (* Subsequence iterators *)

  let iterSub i len f s =
    let i = normalizeIndex i s in
    for j=i to i+len-1 do f (unsafe_get s j) done

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s

  let mapSub i len f s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun j -> f (unsafe_get s (i+j))) (j-i)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldl1Sub i len f s =
    let i = normalizeIndex i s in
    if i < 0 || i >= length s then raise Not_found;
    foldlSub (i+1) (len-1) f (unsafe_get s i) s

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldr1Sub i len f s =
    let i = normalizeIndex i s in
    let j = i + len - 1 in
    if j < 0 || j >= length s then raise Not_found;
    foldrSub i (len-1) f (unsafe_get s j) s


  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s

  let add_int i c = i + ord c
  let add_float i c = i +. float (ord c)
  let mul_int i c = i * ord c
  let mul_float i c = i *. float (ord c)

  let sum a = foldl add_int 0 a
  let sumf a = foldl add_float 0. a
  let product a = foldl mul_int 1 a
  let productf a = foldl mul_float 1. a
  let average a = sum a / len a
  let averagef a = sumf a /. float (len a)

  let sumSub i len a = foldlSub i len add_int 0 a
  let sumSubf i len a = foldlSub i len add_float 0. a

  let sumSlice i j a = foldlSlice i j add_int 0 a
  let sumSlicef i j a = foldlSlice i j add_float 0. a

  let productSub i len a = foldlSub i len mul_int 1 a
  let productSubf i len a = foldlSub i len mul_float 1. a

  let productSlice i j a = foldlSlice i j mul_int 1 a
  let productSlicef i j a = foldlSlice i j mul_float 1. a

  let averageSub i len a = sumSub i len a / len
  let averageSubf i len a = sumSubf i len a /. float len

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (gte l) indices then invalid_arg "pick: Index out of bounds";
    PreList.map (fun i -> unsafe_get s i) indices

  let pickWith funcs s = PreList.map (fun f -> f s) funcs


  (* String specific *)

  let strip = Pcre.replace ~rex:(Pcre.regexp "^\\s+|\\s+$") ~templ:""

  let split ?n sep s = Pcre.split ?max:n ~pat:sep s
  let rsplit ?n sep s = PreList.rev (PreList.map rev (split ?n sep (rev s)))
  let nsplit sep n s = split ~n sep s
  let nrsplit sep n s = rsplit ~n sep s

  let rx = Pcre.regexp
  let rex = Pcre.regexp
  let escape_rex = Pcre.quote

  let rexsplit ?n rex s =
    PreList.map (function Pcre.Text s -> s | _ -> "") @@
    PreList.filter (function Pcre.Text _ -> true | _ -> false) @@
    Pcre.full_split ?max:n ~rex s
  let rexrsplit ?n rex s = PreList.rev (PreList.map rev (rexsplit ?n rex (rev s)))
  let xsplit ?n rexs s = rexsplit ?n (rx rexs) s
  let xrsplit ?n rexs s = rexrsplit ?n (rx rexs) s
  let xnsplit rexs n s = xsplit ~n rexs s
  let xnrsplit rexs n s = xrsplit ~n rexs s

  let rexscan rex s =
    try PreArray.to_list (Array.map PreArray.to_list (Pcre.extract_all ~rex s))
    with _ -> []
  let scan rexs s = rexscan (rx rexs) s

  let rexscan_nth rex n s =
    try
      let arr = Pcre.extract_all ~rex s in
      list (Array.map (fun a ->
        if PreArray.length a <= n
        then invalid_arg "Prelude.rexscan_nth: index out of bounds";
        a.(n)
      ) arr)
    with _ -> []
  let scan_nth rexs n s = rexscan_nth (rx rexs) n s

  let xfind x s = PreList.first (scan_nth x 0 s)
  let xfindOpt x s = optNF PreList.first (scan_nth x 0 s)

  let smatch pat = Pcre.pmatch ~pat
  let rexmatch rex = Pcre.pmatch ~rex
  let xmatch s = rexmatch (rx s)

  let xstartsWith prefix = rexmatch (rx ("^" ^ prefix))
  let startsWith prefix = xstartsWith (escape_rex prefix)

  let xendsWith suffix = rexmatch (rx (suffix ^ "$"))
  let endsWith suffix = xendsWith (escape_rex suffix)

  let replace pat templ = Pcre.replace ~pat ~templ
  let rexreplace rex templ = Pcre.replace ~rex ~templ
  let xreplace s = rexreplace (rx s)

  let frexreplace f rex s =
    let split = Pcre.full_split ~rex s in
    let processed = PreList.map (function
      | Pcre.Text s -> s
      | Pcre.Delim s -> f s
      | _ -> "") split in
    String.concat "" processed
  let fxreplace f s = frexreplace f (rx s)

  let quote l r s = l ^ s ^ r

  let join = String.concat
  let join_array s a = join s (Array.to_list a)

  let xreplaceMulti x_rep s =
    let pat = x_rep |> PreList.map (quote "(" ")" @. fst) |> join "|" in
    frexreplace (fun p -> PreList.assocBy (fun x -> xmatch x p) x_rep) (rex pat) s
  (**
    xreplaceMulti ["f.o","bar"; "b.r","foo"] "foobar" = "barfoo"
    xreplaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "barfoo"
  **)

  let replaceMulti pat_rep s =
    let pat = pat_rep |> PreList.map fst |> PreList.map escape_rex |> join "|" in
    frexreplace (flip PreList.assoc pat_rep) (rex pat) s
  (**
    String.replaceMulti ["foo","bar"; "bar","foo"] "foobar" = "barfoo"
    String.replaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "foofoo"
  **)

  let words s = rexsplit (rx "\\s+") s
  let unwords a = join " " a

  let lines s = split "\n" s
  let unlines a = join "\n" a ^ "\n"

  let rexsplitPartition rex s =
    let rec aux splits l = match splits with
      | [] -> (PreList.rev l, None)
      | (a::[]) -> (PreList.rev l, Some a)
      | (a::b::t) -> aux t ((a,b)::l) in
    let cleaned_split =
      Pcre.full_split ~rex s |>
      PreList.filter (function Pcre.Text _ | Pcre.Delim _ -> true | _ -> false) in
    let padded_split = match cleaned_split with
      | (Pcre.Delim _ :: t) -> (Pcre.Text "") :: cleaned_split
      | _ -> cleaned_split in
    let string_split =
      PreList.map (function Pcre.Text s | Pcre.Delim s -> s | _ -> "") padded_split in
    aux string_split []
  let xsplitPartition x s = rexsplitPartition (rex x) s


  (* Parallel operations *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce (concat "") (map f)
  let pfilter f = pmapReduce (concat "") (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)

  let pinit ?process_count f l =
    let process_count = process_count |? !global_process_count in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat "" (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process

  let pmapWithInit init f =
    pmapReduceWithIndex (concat "") (fun (sublist, idx) -> map f (init sublist idx))
end




module Bytestring =
struct
  include PreString

  let unsafe_get s i = ord (String.unsafe_get s i)
  let unsafe_set s i c = String.unsafe_set s i (chr c)

  let make l i = make l (chr i)

  let init f l =
    let s = create l in
    for i=0 to l-1 do unsafe_set s i (f i) done;
    s

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)

  (* Conversions *)

  let to_array s = PreArray.init (unsafe_get s) (len s)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)

  let to_list s = PreList.init (unsafe_get s) (len s)
  let of_list l = of_array (Array.of_list l)

  (* Searching *)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let filterWithIndex f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c i then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []

  let findWithIndex f s =
    let rec aux f s i len =
      if i >= len then raise Not_found
      else
        let v = unsafe_get s i in
        if f v i then (v, i)
        else aux f s (i+1) len in
    aux f s 0 (len s)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)

  let indexOf v s = findIndex ((=) v) s

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3

  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a

  let maximum = foldl1 max
  let minimum = foldl1 min

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)

  (* Subsequences *)

  let sub i len s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun x -> unsafe_get s (i+x)) (j-i+1)

  let slice_to_sub i j s =
    let i = normalizeIndex i s
    and j = normalizeIndex j s + (if j < 0 then 1 else 0) in
    let len = j - i in
    i, len

  let slice i j s =
    let i, len = slice_to_sub i j s in
    sub i len s

  let subStride stride i len a =
    let i = normalizeIndex i a in
    if i + (len-1) * stride >= length a
    then invalid_arg "subStride: index out of bounds";
    init (fun j -> unsafe_get a (i + j*stride)) len


  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  let tail = slice 1 (-1)

  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  let popped = slice 0 (-2)

  let pop a = (popped a, last a)
  let push v a = append a (string_of_char (chr v))

  let shift a = (tail a, first a)
  let unshift v a = append (string_of_char (chr v)) a

  let take n s = sub 0 n s
  let takeWhile f s = sub 0 (findIndex (fun v -> not (f v)) s + 1) s

  let drop n s = sub (-n) n s
  let dropWhile f s = sub (findIndex (fun v -> not (f v)) s) (len s) s

  let splitAt n xs = (take n xs, drop n xs)

  let break f s = splitAt (findIndex f s) s
  let span f s = break (fun v -> not (f v)) s

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (2 * len s - 1)

  let reject f s = filter (fun v -> not (f v)) s
  let without v s = filter ((<>) v) s

  let groupsOf n a =
    let count, rem = quot_rem (len a) n in
    unfoldrWhile (gte 0) (fun i -> sub (i*n) n a, i-1) (count-1) @
    if rem = 0 then [] else [sub (-rem) rem a]

  let splitInto n range =
    let len = len range in
    let plen = int (ceil (float len /. float n)) in
    groupsOf plen range


  (* Subsequence iterators *)

  let iterSub i len f s =
    let i = normalizeIndex i s in
    for j=i to i+len-1 do f (unsafe_get s j) done

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s

  let mapSub i len f s =
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    init (fun j -> f (unsafe_get s (i+j))) (j-i)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldl1Sub i len f s =
    let i = normalizeIndex i s in
    if i < 0 || i >= length s then raise Not_found;
    foldlSub (i+1) (len-1) f (unsafe_get s i) s

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = min (max 0 (i+len-1)) (slen-1) in
    aux f s init i j

  let foldr1Sub i len f s =
    let i = normalizeIndex i s in
    let j = i + len - 1 in
    if j < 0 || j >= length s then raise Not_found;
    foldrSub i (len-1) f (unsafe_get s j) s


  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s

  let add_int = (+)
  let add_float i c = i +. float c
  let mul_int = ( * )
  let mul_float i c = i *. float c

  let sum a = foldl add_int 0 a
  let sumf a = foldl add_float 0. a
  let product a = foldl mul_int 1 a
  let productf a = foldl mul_float 1. a
  let average a = sum a / len a
  let averagef a = sumf a /. float (len a)

  let sumSub i len a = foldlSub i len add_int 0 a
  let sumSubf i len a = foldlSub i len add_float 0. a

  let sumSlice i j a = foldlSlice i j add_int 0 a
  let sumSlicef i j a = foldlSlice i j add_float 0. a

  let productSub i len a = foldlSub i len mul_int 1 a
  let productSubf i len a = foldlSub i len mul_float 1. a

  let productSlice i j a = foldlSlice i j mul_int 1 a
  let productSlicef i j a = foldlSlice i j mul_float 1. a

  let averageSub i len a = sumSub i len a / len
  let averageSubf i len a = sumSubf i len a /. float len

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (gte l) indices then invalid_arg "pick: Index out of bounds";
    PreList.map (fun i -> unsafe_get s i) indices

  let pickWith funcs s = PreList.map (fun f -> f s) funcs

  let concat = String.concat ""


  (* Parallel combinators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce concat (map f)
  let pfilter f = pmapReduce concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)

  let pinit ?process_count f l =
    let process_count = process_count |? !global_process_count in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process

  let pmapWithInit init f =
    pmapReduceWithIndex concat (fun (sublist, idx) -> map f (init sublist idx))
end



module Range =
struct
  open Array

  let create s e = (s,e)

  let length (s,e) = max 0 (e-s+1)
  let len = length

  let to_list = uncurry PreList.range
  let to_array = uncurry PreArray.range

  let iter f (s, e) = for i = s to e do f i done
  let map f (s,e) = PreList.init (fun i -> f (s+i)) (len (s,e))
  let zipWith f (s,e) (s2,e2) =
    let l = min (len (s,e)) (len (s2,e2)) in
    PreList.init (fun i -> f (s+i) (s2+i)) l
  (**T
    Range.zipWith (+) (1-->10) (1-->11) = zipWith (+) (1--10) (1--11)
    Range.zipWith (/) (1-->10) (2-->11) = replicate 10 0
  **)
  let map2 = zipWith

  let zipWith3 f (s,e) (s2,e2) (s3,e3) =
    let l = min (min (len (s,e)) (len (s2,e2))) (len (s3,e3)) in
    PreList.init (fun i -> f (s+i) (s2+i) (s3+i)) l
  let map3 = zipWith3

  let left_succ (s, e) = (s+1, e)
  let left_pred (s, e) = (s-1, e)

  let right_succ (s, e) = (s, e+1)
  let right_pred (s, e) = (s, e-1)

  let foldl f init (s,e) =
    fst (recurseN (fun (acc, i) -> f acc i, i+1) (len (s,e)-1) (f init s, s+1))

  let foldl1 f (s,e) =
    if len (s,e) < 1 then raise Not_found;
    foldl f s (left_succ (s,e))

  let foldr f init (s,e) =
    fst (recurseN (fun (acc, i) -> f i acc, i-1) (len (s,e)-1) (f e init, e-1))

  let foldr1 f (s,e) =
    if len (s,e) < 1 then raise Not_found;
    foldr f e (right_pred (s,e))

  let find f (s, e) =
    let rec aux f s e =
      if s > e then false
      else if f s then true
      else aux f (s+1) e in
    aux f s e

  let filter f (s, e) =
    let rec aux f s e res =
      if s > e then PreList.rev res
      else if f s then aux f (s+1) e (s::res)
      else aux f (s+1) e res in
    aux f s e []

  let groupsOf n (s,e) =
    unfoldl (lte e) (fun s -> (s, (min (s+n-1) e)), s+n) s

  let splitInto n range =
    let len = length range in
    let plen = int (ceil (float len /. float n)) in
    groupsOf plen range

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce PreList.concat (map f)
  let pfilter f = pmapReduce PreList.concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> PreList.iter r (pmap ?process_count f l)) (groupsOf n l)

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    PreList.concat (par_map ~process_count (uncurry (zipWith f)) (PreList.zip aspl bspl))

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process

  let pmapWithInit init f =
    pmapReduceWithIndex PreList.concat (fun (sublist, idx) -> map f (init sublist idx))

end

let (-->) = Range.create



(* Array operation shortcuts *)

let auget = PreArray.unsafe_get
let auset = PreArray.unsafe_set

let amake = PreArray.make
let acreate = PreArray.create
let ainit = PreArray.init
let alen = PreArray.length
let aconcat = PreArray.concat
let areverse = PreArray.reverse
let arev = areverse

let amap = PreArray.map
let amapSub = PreArray.mapSub
let amapSlice = PreArray.mapSlice
let amapWithIndex = PreArray.mapWithIndex
let aiter = PreArray.iter
let aiterSub = PreArray.iterSub
let aiterSlice = PreArray.iterSlice
let aiterWithIndex = PreArray.iterWithIndex
let afilter = PreArray.filter
let afilterWithIndex = PreArray.filterWithIndex
let afind = PreArray.find
let afindWithIndex = PreArray.findWithIndex
let afindIndex = PreArray.findIndex

let afoldl = PreArray.foldl
let afoldl1 = PreArray.foldl1
let afoldlSub = PreArray.foldlSub
let afoldl1Sub = PreArray.foldl1Sub
let afoldlSlice = PreArray.foldlSlice
let afoldl1Slice = PreArray.foldl1Slice

let afoldr = PreArray.foldr
let afoldr1 = PreArray.foldr1
let afoldrSub = PreArray.foldrSub
let afoldr1Sub = PreArray.foldr1Sub
let afoldrSlice = PreArray.foldrSlice
let afoldr1Slice = PreArray.foldr1Slice

let asum = PreArray.sum
let asumSub = PreArray.sumSub
let asumSlice = PreArray.sumSlice

let asumf = PreArray.sumf
let asumSubf = PreArray.sumSubf
let asumSlicef = PreArray.sumSlicef

let aproduct = PreArray.product
let aproductSub = PreArray.productSub
let aproductSlice = PreArray.productSlice

let aproductf = PreArray.productf
let aproductSubf = PreArray.productSubf
let aproductSlicef = PreArray.productSlicef

let aaverage = PreArray.average
let aaverageSub = PreArray.averageSub
let aaverageSlice = PreArray.averageSlice

let aaveragef = PreArray.averagef
let aaverageSubf = PreArray.averageSubf
let aaverageSlicef = PreArray.averageSlicef

let arange = PreArray.range
let azipWith = PreArray.zipWith
let amap2 = PreArray.zipWith
let azipWith3 = PreArray.zipWith3
let amap3 = PreArray.zipWith3
let asub = PreArray.sub
let aslice = PreArray.slice
let asubStride = PreArray.subStride
let agroupsOf = PreArray.groupsOf
let asplitInto = PreArray.splitInto
let atimes = PreArray.times

let afirst = PreArray.first
let ahead = PreArray.head
let atail = PreArray.tail

let alast = PreArray.last
let apopped = PreArray.popped

let apop = PreArray.pop
let apush = PreArray.push

let ashift = PreArray.shift
let aunshift = PreArray.unshift

let atake = PreArray.take
let atakeWhile = PreArray.takeWhile

let adrop = PreArray.drop
let adropWhile = PreArray.dropWhile

let asplitAt = PreArray.splitAt

let abreak = PreArray.break
let aspan = PreArray.span

let ainterlace = PreArray.interlace

let areject = PreArray.reject
let awithout = PreArray.without

let apick = PreArray.pick
let apickWith = PreArray.pickWith

let apmap = PreArray.pmap
let apiter = PreArray.piter
let apinit = PreArray.pinit
let apzipWith = PreArray.pzipWith
let apfilter = PreArray.pfilter
let apfoldl = PreArray.pfoldl
let apfoldl1 = PreArray.pfoldl1
let apfoldr = PreArray.pfoldr
let apfoldr1 = PreArray.pfoldr1
let apfoldlSeqN = PreArray.pfoldlSeqN
let apiterSeqN = PreArray.piterSeqN

let apmapReduceWithIndex = PreArray.pmapReduceWithIndex
let apmapWithInit = PreArray.pmapWithInit

let (@|) = PreArray.append
let (@|*) = PreArray.times
let (--|) = PreArray.range


(* String operation shortcuts *)

let suget = PreString.unsafe_get
let suset = PreString.unsafe_set

let smake = PreString.make
let screate = PreString.create
let sinit = PreString.init
let slen = PreString.length
let sconcat = PreString.concat
let sreverse = PreString.reverse
let srev = areverse

let smap = PreString.map
let smapSub = PreString.mapSub
let smapSlice = PreString.mapSlice
let smapWithIndex = PreString.mapWithIndex
let siter = PreString.iter
let siterSub = PreString.iterSub
let siterSlice = PreString.iterSlice
let siterWithIndex = PreString.iterWithIndex
let sfilter = PreString.filter
let sfilterWithIndex = PreString.filterWithIndex
let sfind = PreString.find
let sfindWithIndex = PreString.findWithIndex
let sfindIndex = PreString.findIndex

let sfoldl = PreString.foldl
let sfoldl1 = PreString.foldl1
let sfoldlSub = PreString.foldlSub
let sfoldl1Sub = PreString.foldl1Sub
let sfoldlSlice = PreString.foldlSlice
let sfoldl1Slice = PreString.foldl1Slice

let sfoldr = PreString.foldr
let sfoldr1 = PreString.foldr1
let sfoldrSub = PreString.foldrSub
let sfoldr1Sub = PreString.foldr1Sub
let sfoldrSlice = PreString.foldrSlice
let sfoldr1Slice = PreString.foldr1Slice

let ssum = PreString.sum
let ssumSub = PreString.sumSub
let ssumSlice = PreString.sumSlice

let ssumf = PreString.sumf
let ssumSubf = PreString.sumSubf
let ssumSlicef = PreString.sumSlicef

let sproduct = PreString.product
let sproductSub = PreString.productSub
let sproductSlice = PreString.productSlice

let sproductf = PreString.productf
let sproductSubf = PreString.productSubf
let sproductSlicef = PreString.productSlicef

let saverage = PreString.average
let saverageSub = PreString.averageSub
let saverageSlice = PreString.averageSlice

let saveragef = PreString.averagef
let saverageSubf = PreString.averageSubf
let saverageSlicef = PreString.averageSlicef

(* let srange = PreString.range *)
let szipWith = PreString.zipWith
let smap2 = PreString.zipWith
let szipWith3 = PreString.zipWith3
let smap3 = PreString.zipWith3
let ssub = PreString.sub
let sslice = PreString.slice
let ssubStride = PreString.subStride
let sgroupsOf = PreString.groupsOf
let ssplitInto = PreString.splitInto
let stimes = PreString.times

let sfirst = PreString.first
let shead = PreString.head
let stail = PreString.tail

let slast = PreString.last
let spopped = PreString.popped

let spop = PreString.pop
let spush = PreString.push

let sshift = PreString.shift
let sunshift = PreString.unshift

let stake = PreString.take
let stakeWhile = PreString.takeWhile

let sdrop = PreString.drop
let sdropWhile = PreString.dropWhile

let ssplitAt = PreString.splitAt

let sbreak = PreString.break
let sspan = PreString.span

let sinterlace = PreString.interlace

let sreject = PreString.reject
let swithout = PreString.without

let spick = PreString.pick
let spickWith = PreString.pickWith

let spmap = PreString.pmap
let spiter = PreString.piter
let spinit = PreString.pinit
let spzipWith = PreString.pzipWith
let spfilter = PreString.pfilter
let spfoldl = PreString.pfoldl
let spfoldl1 = PreString.pfoldl1
let spfoldr = PreString.pfoldr
let spfoldr1 = PreString.pfoldr1
let spfoldlSeqN = PreString.pfoldlSeqN
let spiterSeqN = PreString.piterSeqN

let spmapReduceWithIndex = PreString.pmapReduceWithIndex
let spmapWithInit = PreString.pmapWithInit

let (^*) = PreString.times

(* String specific shortcuts *)

let strip = PreString.strip

let split = PreString.split
let rsplit = PreString.rsplit
let nsplit = PreString.nsplit
let nrsplit = PreString.nrsplit

let rx = PreString.rx
let rex = PreString.rex
let escape_rex = PreString.escape_rex

let rexsplit = PreString.rexsplit
let rexrsplit = PreString.rexrsplit
let xsplit = PreString.xsplit
let xrsplit = PreString.xrsplit
let xnsplit = PreString.xnsplit
let xnrsplit = PreString.xnrsplit

let rexscan = PreString.rexscan
let scan = PreString.scan

let rexscan_nth = PreString.rexscan_nth
let scan_nth = PreString.scan_nth

let xfind = PreString.xfind
let xfindOpt = PreString.xfindOpt

let smatch = PreString.smatch
let rexmatch = PreString.rexmatch
let xmatch = PreString.xmatch

let replace = PreString.replace
let rexreplace = PreString.rexreplace
let xreplace = PreString.xreplace

let frexreplace = PreString.frexreplace
let fxreplace = PreString.fxreplace

let quote = PreString.quote

let join = PreString.join
let join_array = PreString.join_array

let xreplaceMulti = PreString.xreplaceMulti
let replaceMulti = PreString.replaceMulti

let words = PreString.words
let unwords = PreString.unwords

let lines = PreString.lines
let unlines = PreString.unlines

let rexsplitPartition = PreString.rexsplitPartition
let xsplitPartition = PreString.xsplitPartition

let xstartsWith = PreString.xstartsWith
let startsWith = PreString.startsWith

let xendsWith = PreString.xendsWith
let endsWith = PreString.endsWith

let explode = PreString.to_list
let implode = PreString.of_list

let aexplode = PreString.to_array
let aimplode = PreString.of_array


(* Bytestring operation shortcuts *)

let buget = Bytestring.unsafe_get
let buset = Bytestring.unsafe_set

let bmake = Bytestring.make
let bcreate = Bytestring.create
let binit = Bytestring.init
let blen = Bytestring.length
let bconcat = Bytestring.concat
let breverse = Bytestring.reverse
let brev = areverse

let bmap = Bytestring.map
let bmapSub = Bytestring.mapSub
let bmapSlice = Bytestring.mapSlice
let bmapWithIndex = Bytestring.mapWithIndex
let biter = Bytestring.iter
let biterSub = Bytestring.iterSub
let biterSlice = Bytestring.iterSlice
let biterWithIndex = Bytestring.iterWithIndex
let bfilter = Bytestring.filter
let bfilterWithIndex = Bytestring.filterWithIndex
let bfind = Bytestring.find
let bfindWithIndex = Bytestring.findWithIndex
let bfindIndex = Bytestring.findIndex

let bfoldl = Bytestring.foldl
let bfoldl1 = Bytestring.foldl1
let bfoldlSub = Bytestring.foldlSub
let bfoldl1Sub = Bytestring.foldl1Sub
let bfoldlSlice = Bytestring.foldlSlice
let bfoldl1Slice = Bytestring.foldl1Slice

let bfoldr = Bytestring.foldr
let bfoldr1 = Bytestring.foldr1
let bfoldrSub = Bytestring.foldrSub
let bfoldr1Sub = Bytestring.foldr1Sub
let bfoldrSlice = Bytestring.foldrSlice
let bfoldr1Slice = Bytestring.foldr1Slice

let bsum = Bytestring.sum
let bsumSub = Bytestring.sumSub
let bsumSlice = Bytestring.sumSlice

let bsumf = Bytestring.sumf
let bsumSubf = Bytestring.sumSubf
let bsumSlicef = Bytestring.sumSlicef

let bproduct = Bytestring.product
let bproductSub = Bytestring.productSub
let bproductSlice = Bytestring.productSlice

let bproductf = Bytestring.productf
let bproductSubf = Bytestring.productSubf
let bproductSlicef = Bytestring.productSlicef

let baverage = Bytestring.average
let baverageSub = Bytestring.averageSub
let baverageSlice = Bytestring.averageSlice

let baveragef = Bytestring.averagef
let baverageSubf = Bytestring.averageSubf
let baverageSlicef = Bytestring.averageSlicef

(* let brange = Bytestring.range *)
let bzipWith = Bytestring.zipWith
let bmap2 = Bytestring.zipWith
let bzipWith3 = Bytestring.zipWith3
let bmap3 = Bytestring.zipWith3
let bsub = Bytestring.sub
let bslice = Bytestring.slice
let bsubStride = Bytestring.subStride
let bgroupsOf = Bytestring.groupsOf
let bsplitInto = Bytestring.splitInto
let btimes = Bytestring.times

let bfirst = Bytestring.first
let bhead = Bytestring.head
let btail = Bytestring.tail

let blast = Bytestring.last
let bpopped = Bytestring.popped

let bpop = Bytestring.pop
let bpush = Bytestring.push

let bshift = Bytestring.shift
let bunshift = Bytestring.unshift

let btake = Bytestring.take
let btakeWhile = Bytestring.takeWhile

let bdrop = Bytestring.drop
let bdropWhile = Bytestring.dropWhile

let bsplitAt = Bytestring.splitAt

let bbreak = Bytestring.break
let bspan = Bytestring.span

let binterlace = Bytestring.interlace

let breject = Bytestring.reject
let bwithout = Bytestring.without

let bpick = Bytestring.pick
let bpickWith = Bytestring.pickWith

let bpmap = Bytestring.pmap
let bpiter = Bytestring.piter
let bpinit = Bytestring.pinit
let bpzipWith = Bytestring.pzipWith
let bpfilter = Bytestring.pfilter
let bpfoldl = Bytestring.pfoldl
let bpfoldl1 = Bytestring.pfoldl1
let bpfoldr = Bytestring.pfoldr
let bpfoldr1 = Bytestring.pfoldr1
let bpfoldlSeqN = Bytestring.pfoldlSeqN
let bpiterSeqN = Bytestring.piterSeqN

let bpmapReduceWithIndex = Bytestring.pmapReduceWithIndex
let bpmapWithInit = Bytestring.pmapWithInit

(* Common filesystem operations *)

let rename = Sys.rename

let ls d = PreArray.to_list (Sys.readdir d)
let rm = Sys.remove
let ln_s = Unix.symlink
let ln = Unix.link
let mkdir ?(perm=0o755) s = Unix.mkdir s perm
let rmdir = Unix.rmdir

let getcwd = Sys.getcwd
let pwd = Sys.getcwd
let chdir = Unix.chdir
let cd = Unix.chdir

let chmod perm filename = Unix.chmod filename perm

let fileUid fn = (Unix.stat fn).Unix.st_uid
let fileGid fn = (Unix.stat fn).Unix.st_gid

let chownUid ?gid uid fn =
  let gid = match gid with None -> fileGid fn | Some gid -> gid in
  Unix.chown fn uid gid

let chown ?group user fn =
  let gid = optMap groupGid group in
  chownUid ?gid (userUid user) fn

let chgrpGid gid fn = chownUid ~gid (fileUid fn) fn
let chgrp group fn = chgrpGid (groupGid group) fn


(* Filesystem paths *)

(**T
(* Simple relative *)
  expandPath "foo" = (Filename.concat (Unix.getcwd ()) "foo")

(* Absolute *)
  expandPath "/foo" = "/foo"

(* /./ *)
  expandPath "/foo/./bar/./baz/./" = "/foo/bar/baz"

(* /. *)
  expandPath "/foo/bar/." = "/foo/bar"

(* /../ *)
  expandPath "/foo/../bar/../baz" = "/baz"

(* /../ 2 *)
  expandPath "/foo/../bar/../baz/../" = "/"

(* /.. *)
  expandPath "/foo/bar/.." = "/foo"

(* Mixed /./ and /../ *)
  expandPath "/foo/../bar/./baz/qux/./.." = "/bar/baz"

(* Trailing / (absolute) *)
  expandPath "/foo/" = "/foo"

(* Trailing / (relative) *)
  expandPath "foo/" = (Filename.concat (Unix.getcwd ()) "foo")

(* Root *)
  expandPath "/" = "/"

(* Current dir *)
  expandPath "" = (Unix.getcwd ())
**)
let expandPath path =
  let rec replace re tmpl s =
    let s' = Pcre.replace ~rex:(Pcre.regexp re) ~templ:tmpl s in
    if s = s' then s
              else replace re tmpl s' in
  let p1 = if not (Filename.is_relative path) then path
           else Filename.concat (Sys.getcwd ()) path in
  let p2 = replace "/\\.(/|$)" "/" p1 in
  let p3 = replace "/[^/]+/\\.\\.(/|$)" "/" p2 in
  if String.length p3 > 1
  then replace "/$" "" p3
  else p3

module Path =
struct
  type t = Path of string list

  let absolute a =
    let rec aux a lst = match a with
      | [] -> PreList.rev lst
      | (""::t) -> aux t [""]
      | (".."::t) -> aux t (maybeNF [] PreList.tail lst)
      | (h::t) -> aux t (h::lst) in
    aux a []
  let make s =
    let s = xreplace "/+" "/" s in
    let s = xreplace "/$" "" s in
    Path (split "/" s)
  let to_s (Path a) = if a = [""] then "/" else join "/" a

  let join_path (Path a) (Path b) = Path (absolute (a @ b))

  let join_list path ss = PreList.foldl join_path path (PreList.map make ss)
  let join path s = join_path path (make s)

  let join_list_to_s path ss = to_s (join_list path ss)
  let join_to_s path s = to_s (join path s)

  let expand path = make (expandPath (to_s path))
end
(**T
  Path.to_s (Path.make "/home") = "/home"
  Path.to_s (Path.make "/home/foo") = "/home/foo"
  Path.to_s (Path.make "/home/") = "/home"
  Path.to_s (Path.join (Path.make "/home/") "foo") = "/home/foo"
  Path.to_s (Path.join (Path.make "/home/") "/foo") = "/foo"
  Path.to_s (Path.join (Path.make "/home/") "..") = "/"
  Path.to_s (Path.join_list (Path.make "/home/") [".."; "tmp"]) = "/tmp"
  Path.join_to_s (Path.make "/home/") "/foo" = "/foo"
  Path.join_to_s (Path.make "/home/") ".." = "/"
  Path.join_list_to_s (Path.make "/home/") [".."; "tmp"] = "/tmp"
**)

let (^/) = Filename.concat
let lsFull d = map ((^/) (expandPath d)) (ls d)
let dirExists d = Sys.file_exists d && Sys.is_directory d
let isRoot d =
  let fileInode fn = (Unix.stat fn).Unix.st_ino in
  let fileDevice fn = (Unix.stat fn).Unix.st_dev in
  fileInode d = fileInode "/" && fileDevice d = fileDevice "/"
let parentDirs d =
  generateUntil (eq "") (nrsplit "/" 2 |>. PreList.first) (expandPath d) @ ["/"]

let dirSeparator = sslice 1 (-2) ("a" ^/ "b")
let splitPath p = match p with
  | "/" -> ["/"]
  | p ->
    begin match split dirSeparator p with
      | (""::t) -> "/"::t
      | ps -> ps
    end
let joinPath ps = foldl1 (^/) ps
(**T
  joinPath (splitPath "/foo/bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/") = "/foo"
  joinPath (splitPath "/foo") = "/foo"
  joinPath (splitPath "/") = "/"
**)
let relativePath path =
  let cp = splitPath (expandPath ".") in
  let pp = splitPath (expandPath path) in
  let cp, pp = dropWhile2 (=) cp pp in
  joinPath (replicate (len cp) ".." @ pp)

let dirname = Filename.dirname
let basename = Filename.basename

let mkdir_p ?(perm=0o755) s =
  let nex, ex = span (not @. Sys.file_exists) (parentDirs s) in
  PreList.iter (mkdir ~perm) (reverse nex)


(* File and IO operations *)

let putStr = print_string
let putStrLn = print_endline
let puts s = if rexmatch (rx "\n$") s
             then print_string s
             else print_endline s
let output_line oc line =
  output_string oc line;
  output_char oc '\n'

let readLine = input_line
let readChar = input_char
let readByte = input_byte
let readInt = readLine |>. parseInt
let readFloat = readLine |>. parseFloat

let open_append = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666
let open_append_bin = open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o666

let fileExists = Sys.file_exists

let withFile filename f = finally close_in f (open_in_bin filename)
let withFileOut filename f = finally close_out f (open_out_bin filename)
let withFileAppend filename f = finally close_out f (open_append_bin filename)

let withUnixFile ?(flags=[Unix.O_RDONLY]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileOut ?(flags=[Unix.O_WRONLY;Unix.O_TRUNC;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileAppend ?(flags=[Unix.O_APPEND;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)


let read ?buf bytes ch =
  let rec aux ch bytes c buf =
    match input ch buf c (bytes-c) with
      | 0 when c = 0 -> raise End_of_file
      | 0 -> ssub 0 c buf
      | b when c + b = bytes -> buf
      | b -> aux ch bytes (c+b) buf in
  let buf = match buf with
    | None -> screate bytes
    | Some s ->
      if slen s = bytes then s
      else invalid_arg (sprintf
                        "Prelude.read: buffer size %d differs from read size %d"
                        (slen s) bytes) in
  aux ch bytes 0 buf

let write = output_string

let readAll ch =
  let rec aux ch ret buf =
    match input ch buf 0 4096 with
      | 0 -> Buffer.contents ret
      | b -> Buffer.add_substring ret buf 0 b;
             aux ch ret buf in
  let ret = Buffer.create 4096 in
  let buf = String.create 4096 in
  aux ch ret buf

let stat = Unix.lstat

let fileSize filename = (stat filename).Unix.st_size

let fileKind fn = (stat fn).Unix.st_kind
let isKind kind fn = fileKind fn = kind
let isDir = isKind Unix.S_DIR
let isFile = isKind Unix.S_REG
let isLink = isKind Unix.S_LNK
let isFIFO = isKind Unix.S_FIFO
let isSocket = isKind Unix.S_SOCK
let isCharDev = isKind Unix.S_CHR
let isBlockDev = isKind Unix.S_BLK

let fileInode fn = (stat fn).Unix.st_ino
let filePermissions fn = (stat fn).Unix.st_perm
let fileDevice fn = (stat fn).Unix.st_dev
let fileUid fn = (stat fn).Unix.st_uid
let fileOwner fn = userName (fileUid fn)
let fileGid fn = (stat fn).Unix.st_gid
let fileGroup fn = groupName (fileGid fn)

let atime fn = (stat fn).Unix.st_atime
let mtime fn = (stat fn).Unix.st_mtime
let ctime fn = (stat fn).Unix.st_ctime

let readFile filename = withFile filename readAll
let writeFile filename str = withFileOut filename (flip output_string str)
let appendToFile filename str = withFileAppend filename (flip output_string str)

let readLines = lines @. readFile

let tokenize t ic = unfoldlOpt (maybeEOF None (fun ic -> Some (t ic, ic))) ic
let tokenizeN t n ic = readN t n ic
let tokenizeIter t f ic = maybeEOF () (loop (f @. t)) ic
let tokenizeMap t f ic = tokenize (f @. t) ic
let tokenizeFile t filename = withFile filename (tokenize t)
let tokenizeFileN t n fn = withFile fn (tokenizeN t n)

let icEachLine f ic = tokenizeIter input_line f ic
let icMapLines f ic = tokenizeMap input_line f ic
let eachLine f = flip withFile (icEachLine f)
let mapLines f = flip withFile (icMapLines f)

let output_line_flush oc s = output_line oc s; flush oc


let withTempFile suffix f =
  let tmpfilename _ =
    "/tmp" ^/ (showInt (Random.int 1000000) ^ showFloat (timeNow ()) ^ "." ^ suffix) in
  let fn = (0--1000)
    |> find (fun i -> not (fileExists (tmpfilename i)))
    |> tmpfilename in
  finally (fun fn -> if fileExists fn then Sys.remove fn else ()) f fn


let pipeWith f init i o = recurseOpt (f i o) init
let pipeChan f = pipeWith (optEOF @.. f)
let unitPipe t f = t (fun ic () -> f ic, ())
let pipeTokenizer input output f ic oc init =
  let line, acc = f (input ic) init in
  output oc line;
  acc

let linePiper = pipeTokenizer input_line output_line_flush
let blockPiper ?buf block_sz = pipeTokenizer (read ?buf block_sz) write

let pipeLines f = pipeChan (linePiper f)
let pipeBlocks block_sz f =
  let buf = String.create block_sz in
  pipeChan (blockPiper ~buf block_sz f)

let withFiles f infile outfile =
  withFile infile (fun ic -> withFileOut outfile (fun oc -> f ic oc))
let withFilesAppend f infile outfile =
  withFile infile (fun ic -> withFileAppend outfile (fun oc -> f ic oc))

let pipeFiles f init = withFiles (pipeChan f init)
let pipeFileLines f init = withFiles (pipeLines f init)
let pipeFileBlocks block_sz f init = withFiles (pipeBlocks block_sz f init)

let pipeAppend f init = withFilesAppend (pipeChan f init)
let pipeAppendLines f init = withFilesAppend (pipeLines f init)
let pipeAppendBlocks block_sz f init = withFilesAppend (pipeBlocks block_sz f init)

let interactWith f = pipeChan (unitPipe linePiper f) ()
let interact f = interactWith f stdin stdout
let interactFiles f = pipeFiles (unitPipe linePiper f) ()
let interactAppend f = pipeAppend (unitPipe linePiper f) ()

let appendFileTo oc filename =
  withFile filename (fun ic -> pipeBlocks 4096 tuple () ic oc)

let appendFileToFile dst src = pipeAppendBlocks 4096 tuple () dst src
let appendFiles dst srcs = iter (appendFileToFile dst) srcs
let concatFiles dst srcs = withFileOut dst (fun oc -> iter (appendFileTo oc) srcs)

let cp s d = pipeFileBlocks 4096 tuple () s d
let mv s d =
  try Sys.rename s d
  with Sys_error "Invalid cross-device link" -> cp s d; Sys.remove s

let prependFile filename str =
  if fileSize filename > 32000000 (* use temp file if larger than 32 megs *)
  then withTempFile filename (fun fn ->
    withFileOut fn (fun oc -> write oc str; appendFileTo oc filename);
    mv fn filename)
  else writeFile filename (str ^ readFile filename)


let shell_escape =
  let re = Pcre.regexp "(?=[^a-zA-Z0-9._+/-])" in
  Pcre.replace ~rex:re ~templ:"\\"

let escape_cmd args = String.concat " " (PreList.map shell_escape args)

exception Command_error of int * string
let command args =
  let cmd = escape_cmd args in
  let retcode = Sys.command cmd in
  if retcode <> 0 then
    raise (Command_error (retcode, (sprintf "Command failed with %d: %S" retcode cmd)))
  else
    ()


(* Shell commands *)

let runCmd = command
let cmdCode args = try command args; 0 with Command_error (rv,_) -> rv

let withRawCmd cmd f =
  let ic,oc = Unix.open_process cmd in
  finally (fun _ -> maybeE () close_out oc; maybeE () close_in ic)
          (f ic) oc
let withRawCmdStdin args f =
  withRawCmd args (fun ic oc -> maybeE () close_in ic; f oc)
let withRawCmdStdout args f =
  withRawCmd args (fun ic oc -> maybeE () close_out oc; f ic)

let withCmd args = withRawCmd (escape_cmd args)
let withCmdStdin args = withRawCmdStdin (escape_cmd args)
let withCmdStdout args = withRawCmdStdout (escape_cmd args)

let readCmd args = withCmdStdout args readAll
let readRawCmd args = withRawCmdStdout args readAll

let pipeCmd f init args = withCmd args (pipeChan f init)
let pipeCmdLines f init args = withCmd args (pipeLines f init)

let pipeRawCmd f init args = withRawCmd args (pipeChan f init)
let pipeRawCmdLines f init args = withRawCmd args (pipeLines f init)

let interactWithRawCmd f args = withRawCmd args (interactWith f)
let interactWithCmd f args = withCmd args (interactWith f)


(* Bigarray (1D) operations *)

let bacreate ?(layout=Bigarray.c_layout) kind l =
  Bigarray.Array1.create kind layout l

let bafill = Bigarray.Array1.fill
let basub = Bigarray.Array1.sub
let bablit = Bigarray.Array1.blit

let bamake ?layout kind l init =
  let ba = bacreate ?layout kind l in
  bafill ba init;
  ba

let baget = Bigarray.Array1.get
let baset = Bigarray.Array1.set

let balayout = Bigarray.Array1.layout
let bakind = Bigarray.Array1.kind
let balen = Bigarray.Array1.dim

let bainit ?layout kind f l =
  let ba = bacreate ?layout kind l in
  for i = 0 to l - 1 do
    baset ba i (f i)
  done;
  ba

let baiter f ba =
  let l = balen ba in
  for i = 0 to l - 1 do
    f (baget ba i)
  done

let of_string s = bainit Bigarray.char (String.unsafe_get s) (slen s)
let to_string ba = sinit (baget ba) (balen ba)

let bamap f ba =
  bainit ~layout:(balayout ba) (bakind ba) (fun i -> f (baget ba i)) (balen ba)

let bamapi f ba =
  bainit ~layout:(balayout ba) (bakind ba) (fun i -> f i (baget ba i)) (balen ba)

let bamapWithIndex f ba =
  bainit ~layout:(balayout ba) (bakind ba) (fun i -> f (baget ba i) i) (balen ba)

let bafoldl f init s =
  let rec aux f s len v i =
    if i >= len then v else aux f s len (f v (baget s i)) (i+1) in
  aux f s (balen s) init 0

let bafoldr f init s =
  let rec aux f s v i =
    if i < 0 then v else aux f s (f (baget s i) v) (i-1) in
  aux f s init (balen s - 1)

let barev ba =
  let l = balen ba in
  bainit ~layout:(balayout ba) (bakind ba) (fun i -> baget ba (l-i-1)) l

let baZipWith f a b =
  let len = min (balen a) (balen b) in
  bainit ~layout:(balayout a) (bakind a) (fun i -> f (baget a i) (baget b i)) len
let bamap2 = baZipWith

let bacreateMmap ?(layout=Bigarray.c_layout) ?(shared=true)
                  ?(perm=0o600) ?(flags=[Unix.O_RDWR])
                  kind l filename =
  withUnixFile ~flags ~perm filename
    (fun fd -> Bigarray.Array1.map_file fd kind layout shared l)

let bacreateShared ?layout kind l =
  bacreateMmap ?layout kind l "/dev/zero"

let par_bainit ?process_count ?layout kind f l =
  let ba = bacreateShared ?layout kind l in
  pforN ?process_count (fun i -> Bigarray.Array1.set ba i (f i)) l;
  ba
let bapinit = par_bainit


(* Hashtables *)

let string_hash_djb2 s =
  let rec aux s len v i =
    if i >= len then v
    else aux s len (((v lsl 5) + v) + (ord (suget s i))) (i+1) in
  aux s (slen s) 5381 0

let string_hash_head =
  let ws = Sys.word_size / 8 - 1 in
  let rec aux sum s i =
    if i < 0 then sum
    else aux ((sum lsl 8) lor (ord (suget s i))) s (i-1) in
  fun s -> aux 0 s ((min (slen s) ws) - 1)

(* SHash is a Hashtbl with strings as keys.
   SHash uses string_hash_djb2 as the hash function.
*)
module SHash = Hashtbl.Make(struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let hash = string_hash_djb2
end)
(* HHash is a Hashtbl with hash strings as keys.
   HHash uses string_hash_head as the hash function.
*)
module HHash = Hashtbl.Make(struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let hash = string_hash_head
end)


(* Maps *)

module SMap = Map.Make(String)
module IMap = Map.Make(struct type t = int let compare = (-) end)
