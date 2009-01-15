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
(**T
  uncurry3 (fun a b c -> a ^ b ^ c) ("a", "b", "c") = "abc"
**)
let uncurry4 f (a,b,c,d) = f a b c d
(**T
  uncurry4 (fun a b c d -> a ^ b ^ c ^ d) ("a", "b", "c", "d") = "abcd"
**)
let uncurry5 f (a,b,c,d,e) = f a b c d e
(**T
  uncurry5 (fun a b c d e -> a^b^c^d^e) ("a","b","c","d","e") = "abcde"
**)
let uncurry6 f (a,b,c,d,e,g) = f a b c d e g
(**T
  uncurry6 (fun a b c d e f -> a^b^c^d^e^f) ("a","b","c","d","e","f") = "abcdef"
**)
let curry f a b = f (a, b)
(**T
  curry reverseTuple 1 2 = (2,1)
**)
let curry3 f a b c = f (a,b,c)
(**T
  curry3 (fun (a,b,c)->a^b^c) "a" "b" "c" = "abc"
**)
let curry4 f a b c d = f (a,b,c,d)
(**T
  curry4 (fun (a,b,c,d)->a^b^c^d) "a" "b" "c" "d" = "abcd"
**)
let curry5 f a b c d e = f (a,b,c,d,e)
(**T
  curry5 (fun (a,b,c,d,e)->a^b^c^d^e) "a" "b" "c" "d" "e" = "abcde"
**)
let curry6 f a b c d e g = f (a,b,c,d,e,g)
(**T
  curry6 (fun (a,b,c,d,e,f)->a^b^c^d^e^f) "a" "b" "c" "d" "e" "f" = "abcdef"
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
  maybeExl [Exit; Not_found] 0 last [1] = 1
  optE (fun _ -> maybeExl [Exit] 0 last []) 0 = None
**)
let maybeEOF v f o = maybeEx End_of_file v f o
(**T
  maybeEOF 0 (fun _ -> raise End_of_file) [] = 0
  maybeEOF 0 (fun _ -> 1) [] = 1
  optE (fun _ -> maybeEOF 0 (fun _ -> raise Exit) []) 0 = None
  (* unfoldlOpt (maybeEOF None (fun ic -> Some (readInt ic, ic)) ic) stdin;; *)
**)
let maybeNF v f o = maybeEx Not_found v f o
(**T
  maybeNF 0 last [] = 0
  maybeNF 0 last [1;2;3] = 3
  optE (fun _ -> maybeNF 0 (fun _ -> raise Exit) []) 0 = None
**)


(* Exceptions to options *)

let optE f o = maybeE None (fun v -> Some (f v)) o
(**T
  optE last [] = None
  optE last [1] = Some 1
**)
let optEx ex f o = maybeEx ex None (fun v -> Some (f v)) o
(**T
  optEx Not_found last [] = None
  optEx Not_found last [1] = Some 1
  optEx (Failure "hi") (fun _ -> failwith "hi") 0 = None
  optE (fun _ -> optEx (Failure "hi") (fun _ -> failwith "hi") 0) 0 = Some None
  optE (fun _ -> optEx (Failure "ho") (fun _ -> failwith "hi") 0) 0 = None
**)
let optExl exl f o = maybeExl exl None (fun v -> Some (f v)) o
(**T
  optExl [Not_found] last [] = None
  optExl [Failure "hi"; Not_found] last [1] = Some 1
  optExl [Failure "hi"; Not_found] (fun _ -> failwith "hi") 0 = None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "hi") 0) 0 = Some None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "ho") 0) 0 = Some None
  optE (fun _ -> optExl [Failure "hi"; Failure "ho"] (fun _ -> failwith "ha") 0) 0 = None
**)
let optEOF f o = maybeEOF None (fun v -> Some (f v)) o
(**T
  optEOF (fun i -> i) 0 = Some 0
  optEOF (fun i -> raise End_of_file) 0 = None
  optE (fun _ -> optEOF last []) 0 = None
**)
let optNF f o = maybeEx Not_found None (fun v -> Some (f v)) o
(**T
  optNF last [] = None
  optNF last [1;2;3] = Some 3
  optE (fun _ -> optNF (fun _ -> failwith "hi") []) 0 = None
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
  between 2 2 2 = true
  between 1 2 3 = false
  between 2 1 3 = false
  between 1 3 2 = true
  between 1 2 2 = true
  between 1 2 1 = true
  filter (between 2 6) (1--10) = (2--6)
**)

let maxBy f a b = if (f a) >= (f b) then a else b
(**T
  maxBy square 2 (-3) = -3
  maxBy square 2 (-1) = 2
**)
let minBy f a b = if (f a) <= (f b) then a else b
(**T
  minBy square 2 (-3) = 2
  minBy square 2 (-1) = -1
**)


(* Tuples *)

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let tuple4 a b c d = (a,b,c,d)
let tuple5 a b c d e = (a,b,c,d,e)
let tuple6 a b c d e f = (a,b,c,d,e,f)
let reverseTuple (a,b) = (b,a)
let trev = reverseTuple
(**T
  trev (1,2) = (2,1)
**)
let fuple f g a = (f a, g a)
(**T
  fuple pred succ 0 = (-1, 1)
**)
let fuplel f a = (f a, a)
(**T
  fuplel succ 0 = (1, 0)
**)
let fupler f a = (a, f a)
(**T
  fupler succ 0 = (0, 1)
**)


(* Conversions *)

let array = Array.of_list
let list = Array.to_list
let int = int_of_float
let char = char_of_int
let parseInt = int_of_string
let parseFloat = float_of_string
let showInt = string_of_int
let showFloat f =
  match string_of_float f with
    | "inf" -> "infinity"
    | "-inf" -> "-infinity"
    | s -> Pcre.replace ~rex:(Pcre.regexp "\\.$") ~templ:".0" s
(**T
  showFloat 1. = "1.0"
  showFloat 1.0048 = "1.0048"
  showFloat (-0.2) = "-0.2"
  showFloat 1e18 = "1e+18"
  showFloat 1e-18 = "1e-18"
  showFloat infinity = "infinity"
  showFloat neg_infinity = "-infinity"
  showFloat 0.0 = "0.0"
  showFloat (-0.0) = "-0.0"
  showFloat (0.0 /. 0.0) = "nan"
  showFloat 1e-320 <> "1e-320"
**)
let charCode = int_of_char
let ord = int_of_char
let chr = char_of_int
let string_of_char c = String.make 1 c
(**T
  string_of_char 'c' = "c"
  foldl (fun s i -> s ^ string_of_char i) "" ('\000'-~'\255') = ('\000'--^'\255')
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
(**T
  (let i = ref 0 in try loop (fun i -> if !i >= 10 then raise Exit else i := succ !i) i with Exit -> !i = 10)
  (* loop (print_endline @. input_line) stdin;; *)
**)
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
  readN (fun i -> i := !i+1; !i) 0 (ref 0) = []
  readN (fun i -> i := !i+1; !i) (-1) (ref 0) = []
**)

let forN f n = for i=0 to (n-1) do f i done
(**T
  (let i = ref 0 in forN (fun j -> i := !i + j) 10; !i = sum (0--9))
  (let i = ref 0 in forN (fun j -> i := !i + j) 0; !i = 0)
  (let i = ref 0 in forN (fun j -> i := !i + j) (-1); !i = 0)
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
  iterate pred 0 1 = []
  iterate pred (-1) 1 = []
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
  invoke sum [] () = sum []
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
  let process_count = max 1 (process_count |? !global_process_count) in
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
(**T
  par_iter (ignore @. succ) (1--10) = ()
  par_iter (ignore @. succ) [1] = ()
  par_iter (ignore @. succ) [] = ()
**)

(* lockstep iteration, unshifting new job after popped job completes
   a polling system would fare better with uneven job runtimes
*)
let par_map ?process_count f l =
  let process_count = max 1 (process_count |? !global_process_count) in
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
  par_map succ [1] = map succ [1]
  par_map succ [] = map succ []
**)

(*
  Splits n into process_count continuous segments,
  executes each in its own process.
*)
let pforN ?process_count f n =
  let process_count = max 1 (process_count |? !global_process_count) in
  let plen = int (ceil (float n /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (n - start) in
    for j = start to start+len-1 do
      f j
    done in
  par_iter ~process_count process (Array.to_list (Array.init process_count id))
(**T
  pforN (ignore @. succ) 10 = ()
  pforN (ignore @. succ) 1 = ()
  pforN (ignore @. succ) 0 = ()
  pforN (ignore @. succ) (-1) = ()
**)

let mapReduce partition distribute process combine input =
  partition input |> distribute process |> combine


(* Integer operations *)

let even x = x land 1 == 0
(**T
  filter even ((-10)--10) = map (multiply 2) ((-5)--5)
**)
let odd x = x land 1 == 1
(**T
  filter odd ((-10)--10) = [-9;-7;-5;-3;-1;1;3;5;7;9]
**)

let average2 a b =
  if a >= 0 && b >= 0
  then ((a lxor b) lsr 1) + (a land b)
  else if a < 0 && b < 0
  then ((a lxor b) lsr 1) + (a land b) + ((a land 1) lxor (b land 1))
  else (a + b) / 2
(**T
  average2 2 4 = 3
  average2 4 2 = 3
  average2 2 3 = 2
  average2 1 3 = 2
  average2 0 2 = 1
  average2 (-2) (-4) = (-3)
  average2 (-1) (-3) = (-2)
  average2 (-2) (-3) = (-2)
  average2 (-1) 1 = 0
  average2 (-2) 2 = 0
  average2 (-4) 2 = -1
  average2 (-5) 2 = -1
  average2 (-5) 1 = -2
  average2 5 (-1) = 2
  average2 (-6) 2 = -2
  average2 max_int max_int = max_int
  average2 min_int min_int = min_int
  average2 max_int min_int = 0
  average2 max_int 0 = max_int / 2
  average2 min_int 0 = min_int / 2
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
let signum i = if i > 0 then 1 else if i < 0 then (-1) else 0
(**T
  signum 0 = 0
  signum 41 = 1
  signum (-41) = -1
  signum max_int = 1
  signum min_int = -1
**)
let succ x = x + 1
(**T
  succ 0 = 1
  succ max_int = min_int
**)
let pred x = x - 1
(**T
  pred 0 = -1
  pred min_int = max_int
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
let square x = x * x
(**T
  square 0 = 0
  square 2 = 4
  square (-2) = 4
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


(* Float operations *)

let round f = truncate (if f > 0.0 then f +. 0.5 else f -. 0.5)
(**T
  round 0.5 = 1
  round 0.4 = 0
  round (-0.4) = 0
  round (-0.5) = -1
**)
let ceiling = ceil
(**T
  ceiling 0. = 0.
  ceiling (-0.) = 0.
  ceiling 0.5 = 1.0
  ceiling 0.1 = 1.0
  ceiling (-0.1) = 0.0
  ceiling (-0.5) = 0.0
**)
let recip f = 1. /. f
(**T
  recip 42. = 1. /. 42.
  recip 3. = 1. /. 3.
  recip 0. = infinity
  recip (-0.) = neg_infinity
  recip (-1.) = -1.
**)
let signumf f = if f > 0. then 1. else if f < 0. then (-1.) else 0.
(**T
  signumf 42. = 1.
  signumf (-42.) = -1.
  signumf 0. = 0.
  signumf infinity = 1.
  signumf neg_infinity = -1.
  signumf nan = 0.
**)
let logBase base f = if base = 10. then log10 f else log f /. log base
(**T
  logBase 10. 1000. = log10 1000.
  absf (logBase 2. 64. -. 6.) < 0.0001
**)
let root rt f =
  if f < 0.0 then
    let d, i = modf rt in
    if d = 0.0 && odd (int i)
    then -.((-.f) ** (recip rt))
    else nan
  else f ** (recip rt)
(**T
  root 2. 4. = 2.
  root 4. 16. = 2.
  root 3. (-8.) = -2.
  isNaN (root 2. (-8.))
**)
let absf = abs_float
(**T
  absf 0.1 = 0.1
  absf (-0.1) = 0.1
  absf (-0.0) = 0.0
  absf (0.0) = 0.0
  absf neg_infinity = infinity
  absf infinity = infinity
  isNaN @@ absf nan
**)
let pi = 4. *. atan 1.
(**T
  between 3.14 3.15 pi
  absf ((sin pi) -. 0.) < 0.00001
  absf ((cos pi) -. (-1.)) < 0.00001
  absf ((sin (2.*.pi)) -. 0.) < 0.00001
  absf ((cos (2.*.pi)) -. 1.) < 0.00001
**)
let succf x = x +. 1.
(**T
  succf 0. = 1.
  succf max_float = max_float
  succf min_float = 1.
**)
let predf x = x -. 1.
(**T
  predf 0. = -1.
  predf (-.max_float) = (-.max_float)
  predf min_float = -1.
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
let squaref x = x *. x
(**T
  squaref 0. = 0.
  squaref 2. = 4.
  squaref (-2.) = 4.
**)
let isNaN f = classify_float f = FP_nan
(**T
  isNaN 1.0 = false
  isNaN 0.0 = false
  isNaN (1.0 /. 0.0) = false
  isNaN (0.0 /. 0.0) = true
  isNaN nan = true
  isNaN neg_infinity = false
  isNaN infinity = false
**)
let isSubnormal f = classify_float f = FP_subnormal
(**T
  isSubnormal 1.0 = false
  isSubnormal 1e-320 = true
  isSubnormal (-1e-320) = true
  isSubnormal 0.0 = false
  isSubnormal (1.0 /. 0.0) = false
  isSubnormal (0.0 /. 0.0) = false
  isSubnormal nan = false
  isSubnormal neg_infinity = false
  isSubnormal infinity = false
**)



(* Char operations *)

let predChar c = chr (ord c - 1)
(**T
  predChar 'b' = 'a'
  predChar '\255' = '\254'
  predChar '\001' = '\000'
  optEx (Invalid_argument "char_of_int") predChar '\000' = None
**)
let succChar c = chr (ord c + 1)
(**T
  succChar 'b' = 'c'
  succChar '\000' = '\001'
  succChar '\254' = '\255'
  optEx (Invalid_argument "char_of_int") succChar '\255' = None
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
(* These break in different interesting fashions on different platforms
  showTime ~zone:0 (-.max_float) = "0000-01-01 00:00:00+0000"
  showTime ~zone:0 (max_float) = "0000-01-01 00:00:00+0000"
  showTime ~zone:0 neg_infinity = "0000-01-01 00:00:00+0000"
  showTime ~zone:0 infinity = "0000-01-01 00:00:00+0000"
  showTime ~zone:0 nan = "0000-01-01 00:00:00+0000"
  optEx (Invalid_argument "Netdate.format_to") (showTime ~zone:0) 1e20 = None
  optEx (Invalid_argument "Netdate.format_to") (showTime ~zone:0) (-1e20) = None
*)
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
(**T
  groupByName "root" = groupByGid (groupGid "root")
**)
let groupByGid = Unix.getgrgid
(**T
  groupByGid (groupGid "root") = groupByName "root"
**)

let groupGid name = (groupByName name).Unix.gr_gid
(**T
  groupGid "root" >= 0
**)
let groupName gid = (groupByGid gid).Unix.gr_name
(**T
  groupName (groupGid "root") = "root"
**)
let gidMembers gid = list (groupByGid gid).Unix.gr_mem
(**T
  len (gidMembers (groupGid "root")) >= 0
**)
let groupMembers name = list (groupByName name).Unix.gr_mem
(**T
  len (groupMembers "root") >= 0
**)


let userByName = Unix.getpwnam
(**T
  userByName (currentUserName ()) = currentUser ()
**)
let userByUid = Unix.getpwuid
(**T
  (userByUid (currentUid ())).Unix.pw_uid = currentUid ()
**)

let userUid name = (userByName name).Unix.pw_uid
(**T
  userUid (currentUserName ()) = currentUid ()
**)
let userGid name = (userByName name).Unix.pw_gid
(**T
  userGid (currentUserName ()) >= 0
**)
let userGroup name = groupName (userGid name)
(**T
  slen (userGroup (currentUserName ())) >= 0
**)
let userGecos name = (userByName name).Unix.pw_gecos
(**T
  slen (userGecos (currentUserName ())) >= 0
**)
let userDir name = (userByName name).Unix.pw_dir
(**T
  slen (userDir (currentUserName ())) >= 0
**)
let userShell name = (userByName name).Unix.pw_shell
(**T
  slen (userShell (currentUserName ())) >= 0
**)
let userName uid = (userByUid uid).Unix.pw_name
(**T
  slen (userName (currentUid ())) >= 0
  userName (currentUid ()) = currentUserName ()
**)

let currentUid () = Unix.getuid ()
(**T
  currentUid () >= 0
**)
let currentUserName () = userName (currentUid ())
(**T
  userUid (currentUserName ()) = currentUid ()
**)
let currentUser () = userByUid (currentUid ())
(**T
  (currentUser ()).Unix.pw_uid = currentUid ()
**)



(* List *)
module PreList =
struct
  include List

  let len = length
  (**T
    len (1--10) = 10
    len [1] = 1
    len [] = 0
  **)

  let init f n =
    let rec aux f n res =
      if n < 0 then res
      else aux f (n-1) ((f n) :: res) in
    aux f (n-1) []
  (**T
    init succ 10 = (1--10)
    init pred 10 = ((-1)--8)
    init succ 0 = []
    init succ 1 = [1]
  **)

  (* Basic operations *)
  let range s e =
    if s > e
    then init ((-) s) (s-e+1)
    else init ((+) s) (e-s+1)
  (**T
    range 1 3 = [1; 2; 3]
    range 1 1 = [1]
    range 1 0 = [1; 0]
    range (max_int - 1) max_int = [max_int - 1; max_int]
    range max_int (max_int - 1) = [max_int; max_int - 1]
    range (min_int + 1) min_int = [min_int + 1; min_int]
    range min_int (min_int + 1) = [min_int; min_int + 1]
  **)
  let rangef s e =
    if s > e
    then init (fun i -> s -. (float i)) (int (ceil (s-.e+.1.)))
    else init (fun i -> s +. (float i)) (int (ceil (e-.s+.1.)))
  (**T
    rangef 1. 3. = [1.; 2.; 3.]
    rangef 1. 1. = [1.]
    rangef 1. 0. = [1.; 0.]
  **)
  let charRange sc ec =
    let s, e = ord sc, ord ec in
    if s > e
    then init (fun i -> chr (s - i)) (s-e+1)
    else init (fun i -> chr (s + i)) (e-s+1)
  (**T
    charRange 'a' 'c' = ['a'; 'b'; 'c']
    charRange 'a' 'a' = ['a']
    charRange 'b' 'a' = ['b'; 'a']
    charRange '\000' '\255' = explode ('\000'--^'\255')
    charRange '\255' '\000' = explode ('\255'--^'\000')
  **)
  let (--) = range
  (**T
    (1--3) = [1; 2; 3]
    (1--1) = [1]
    (1--0) = [1; 0]
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
    step 2 0 0 = [0]
    step 2 0 (-1) = []
    step 2 0 (-5) = []
    step (-2) 0 0 = [0]
    step (-2) 0 1 = []
    step (-2) 0 (-1) = [0]
    step (-2) 0 (-5) = [0; -2; -4]
  **)

  let reverse = rev
  (**T
    reverse (1--10) = (10--1)
    reverse [1] = [1]
    reverse [] = []
  **)

  let normalizeIndex i s = if i < 0 then (length s) + i else i
  (**T
    normalizeIndex 0 [] = 0
    normalizeIndex 2 [] = 2
    normalizeIndex (-2) [] = -2
    normalizeIndex 0 (1--10) = 0
    normalizeIndex 2 (1--10) = 2
    normalizeIndex (-1) (1--10) = 9
    normalizeIndex (-2) (1--10) = 8
    normalizeIndex (-1) (1--2) = 1
    normalizeIndex (-2) (1--2) = 0
  **)

  let replicate n v = init (const v) n
  (**T
    replicate 5 '-' = ['-'; '-'; '-'; '-'; '-']
    replicate 1 '-' = ['-']
    replicate 0 '-' = []
    replicate (-1) '-' = []
  **)

  let times n l = concat (replicate n l)
  (**T
    times 3 [1; 2; 3] = [1; 2; 3; 1; 2; 3; 1; 2; 3]
    times 0 (1--10) = []
    times 1 (1--10) = (1--10)
    times (-1) (1--10) = []
  **)

  let cycle n l =
    let rec aux c lst res =
      if c <= 0 then rev res
      else match lst with
            | [] -> aux c l res
            | (h::t) -> aux (c-1) t (h::res) in
    if l = [] then [] else aux n l []
  (**T
    cycle 5 (1--3) = [1; 2; 3; 1; 2]
    cycle 3 (1--10) = [1; 2; 3]
    cycle 3 [1] = [1;1;1]
    cycle 3 [] = []
    cycle 0 [1] = []
    cycle 1 [1] = [1]
    cycle (-1) [1] = []
  **)

  let nth i l = try List.nth l (normalizeIndex i l)
                with _ -> raise Not_found
  (**T
    nth 1 (1--10) = 2
    nth (-3) (1--10) = 8
    optNF (nth 1) [1] = None
    optNF (nth 0) [] = None
    optNF (nth (-1)) [] = None
    optNF (nth (-1)) [1] = Some 1
  **)
  let ($$) l i = nth i l
  (**T
    (1--10) $$ -1 = 10
    (1--10) $$ 3 = 4
    optNF (fun l -> l $$ 0) [] = None
  **)

  let cons x xs = x::xs
  (**T
    cons 1 [] = [1]
    cons 1 (2--10) = (1--10)
  **)
  let head = function [] -> raise Not_found | (h::_) -> h
  (**T
    optNF head [] = None
    optNF head [1] = Some 1
    optNF head (1--10) = Some 1
  **)
  let tail = function [] -> raise Not_found | (_::t) -> t
  (**T
    optNF tail [] = None
    optNF tail [1] = Some []
    optNF tail (1--10) = Some (2--10)
  **)
  let pop l =
    let rec aux l res =
      match l with
        | [] -> raise Not_found
        | (h::[]) -> (rev res, h)
        | (h::t) -> aux t (h :: res) in
    aux l []
  (**T
    pop [1;2;3] = ([1;2], 3)
    optNF pop [] = None
    optNF pop [1] = Some ([], 1)
    optNF pop [1;2] = Some ([1], 2)
  **)
  let popped l = fst (pop l)
  (**T
    popped [1; 2; 3] = [1; 2]
    optNF popped [] = None
    optNF popped [1] = Some []
  **)
  let last l = snd (pop l)
  (**T
    last [1; 2; 3] = 3
    optNF last [] = None
  **)
  let first = head
  (**T
    first (2--10) = 2
  **)

  let shift l = (tail l, head l)
  (**T
    shift (1--10) = ((2--10), 1)
    optNF shift [] = None
    optNF shift [1] = Some ([], 1)
  **)
  let unshift = cons
  (**T
    unshift 0 (1--10) = (0--10)
  **)



  (* Folds *)

  let foldl = fold_left
  (**T
    foldl (+) 0 (1--10) = 55
    foldl (fun s b -> s ^ (string_of_int b)) "--" (1--3) = "--123"
    foldl (+) 1 [] = 1
    foldl (+) 1 [1] = 2
  **)
  let foldl1 f l = match l with
    | (h::t) -> foldl f h t
    | [] -> raise Not_found
  (**T
    foldl1 (+) (1--10) = 55
    foldl1 (fun s i -> s ^ i) ["foo"; "bar"; "baz"] = "foobarbaz"
    optNF (foldl1 (+)) [] = None
    foldl1 (+) [1] = 1
  **)

  let foldr f s l = fold_right f l s
  (**T
    foldr (+) 0 (1--10) = 55
    foldr (fun a s -> s ^ (string_of_int a)) "--" (1--3) = "--321"
    foldr (+) 1 [] = 1
    foldr (+) 1 [1] = 2
  **)
  let foldr1 f l = let l,i = pop l in foldr f i l
  (**T
    foldr1 (+) (1--10) = 55
    foldr1 (fun a s -> s ^ a) ["foo"; "bar"; "baz"] = "bazbarfoo"
    optNF (foldr1 (+)) [] = None
    foldr1 (+) [1] = 1
  **)



  (* Iterators *)

  let iterWithIndex f l =
    let rec aux f l i = match l with
      | [] -> ()
      | (h::t) -> f h i; aux f t (succ i) in
    aux f l 0
  (**T
    (let i = ref 0 and j = ref 0 in iterWithIndex (fun a b -> i:=a; j:=b) (20--30); !i = 30 && !j = 10)
    iterWithIndex (ignore @.. add) [1] = ()
    iterWithIndex (ignore @.. add) [] = ()
  **)
  let map f l = rev (rev_map f l)
  (**T
    map succ (1--10) = (2--11)
    map succ [] = []
    map succ [1] = [2]
    (let i = ref 0 in ignore(map (fun j -> i := j; j) (1--10)); !i = 10)
  **)
  let mapWithIndex f l =
    rev (snd (foldl (fun (j,r) i -> (j+1, (f i j)::r)) (0, []) l))
  (**T
    mapWithIndex (+) (0--10) = map (multiply 2) (0--10)
    mapWithIndex (-) (10--20) = replicate 11 10
    mapWithIndex (+) [] = []
    mapWithIndex (+) [1] = [1]
  **)

  let concatMap f l = concat (map f l)
  (**T
    concatMap ((--) 1) [1;2;3] = [1; 1; 2; 1; 2; 3]
    concatMap ((--) 1) [2] = [1;2]
    concatMap ((--) 1) [] = []
  **)

  let mapWith ite f l =
    let lr = ref [] in
    let () = ignore (ite (fun i -> let v = f i in lr := v :: !lr) l) in
    rev !lr
  (**T
    mapWith iter succ (1--10) = (2--11)
    mapWith aiter succ (1--|10) = (2--11)
    mapWith map succ (1--10) = (2--11)
    mapWith (fun f l -> rev (map f (rev l))) succ (1--10) = (11--2)
    mapWith iter id [] = []
    mapWith iter id [1] = [1]
    mapWith map id [] = []
    mapWith map id [1] = [1]
  **)


  (* Searching *)

  (* let find = find *)
  let findIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h then c else aux p (c+1) t in
    aux p 0 lst
  (**T
    findIndex (gt 4) (0--9) = 5
    optNF (findIndex (gt 4)) (0--3) = None
    optNF (findIndex (gt 4)) [] = None
  **)
  let findWithIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h c then (h,c) else aux p (c+1) t in
    aux p 0 lst
  (**T
    findWithIndex (fun v _ -> v > 4) (2--9) = (5,3)
    findWithIndex (fun _ i -> i > 4) (2--9) = (7,5)
    optNF (findWithIndex (const (gt 4))) (0--3) = None
    optNF (findWithIndex (const (gt 4))) [] = None
  **)

  let indexOf x lst =
    let rec aux x c l = match l with
      | [] -> raise Not_found
      | (h::t) when x = h -> c
      | (h::t) -> aux x (c+1) t in
    aux x 0 lst
  (**T
    indexOf 14 (10--20) = 4
    optNF (indexOf 1) (10--20) = None
    indexOf 'a' (explode "foobar") = 4
  **)



  (* Zipping *)

  let zipWith f a b =
    let rec aux f a b l = match a,b with
        | (x::xs), (y::ys) -> aux f xs ys ((f x y)::l)
        | _ -> rev l in
    aux f a b []
  (**T
    zipWith (+) (1--10) (1--10) = map (dup (+)) (1--10)
    zipWith (-) (1--5) (3--1) = [-2; 0; 2]
    zipWith (-) (1--3) (5--1) = [-4; -2; 0]
    zipWith (+) [1] (1--10) = [2]
    zipWith (+) (1--10) [1] = [2]
    zipWith (+) [1] [1] = [2]
    zipWith (+) [] (1--10) = []
    zipWith (+) (1--10) [] = []
    zipWith (+) [] [] = []
  **)
  let zip a b = zipWith tuple a b
  (**T
    zip (1--3) (3--1) = [(1,3); (2,2); (3,1)]
    zip (1--500) (3--1) = [(1,3); (2,2); (3,1)]
    zip (1--3) (3--(-100)) = [(1,3); (2,2); (3,1)]
    zip (1--4) (3--1) = [(1,3); (2,2); (3,1)]
    zip (1--3) (3--0) = [(1,3); (2,2); (3,1)]
    zip (1--3) [3] = [(1,3)]
    zip [3] (1--3) = [(3,1)]
    zip (1--3) [] = []
    zip [] (1--3) = []
    zip [] [] = []
  **)
  let unzip = split
  (**T
    unzip (zip (1--3) (3--1)) = ((1--3), (3--1))
    unzip (zip (3--1) (1--3)) = ((3--1), (1--3))
    unzip [(1,2)] = ([1], [2])
    unzip [] = ([], [])
  **)

  let zipWith3 f a b c =
    let rec aux f a b c res =
      match a,b,c with
        | (h1::t1), (h2::t2), (h3::t3) -> aux f t1 t2 t3 ((f h1 h2 h3)::res)
        | _ -> rev res in
    aux f a b c []
  (**T
    zipWith3 (fun a b c -> a + b + c) (1--100000) (1--100000) (1--100000) = map (multiply 3) (1--100000)
    zipWith3 (fun a b c -> a^b^c) ["a"] ["b"] ["c";"d"] = ["abc"]
    zipWith3 (fun a b c -> a + b + c) [1] (1--10) (1--10) = [3]
    zipWith3 (fun a b c -> a + b + c) (1--10) [1] (1--10) = [3]
    zipWith3 (fun a b c -> a + b + c) (1--10) (1--10) [1] = [3]
    zipWith3 (fun a b c -> a + b + c) [1] (1--10) [1] = [3]
    zipWith3 (fun a b c -> a + b + c) [1] [1] (1--10) = [3]
    zipWith3 (fun a b c -> a + b + c) (1--10) [] [1] = []
    zipWith3 (fun a b c -> a + b + c) (1--10) [1] [] = []
    zipWith3 (fun a b c -> a + b + c) (1--10) [] [] = []
    zipWith3 (fun a b c -> a + b + c) [] [1] [1] = []
    zipWith3 (fun a b c -> a + b + c) [] [] [1] = []
    zipWith3 (fun a b c -> a + b + c) [] [1] [] = []
    zipWith3 (fun a b c -> a + b + c) [1] [] [] = []
    zipWith3 (fun a b c -> a + b + c) [] [] [] = []
  **)
  let zip3 a b c = zipWith3 tuple3 a b c
  (**T
    zip3 (1--500) (3--1) (2--5) = [(1,3,2); (2,2,3); (3,1,4)]
    zip3 (1--10) [2] [3] = [(1,2,3)]
    zip3 [1] (2--10) [3] = [(1,2,3)]
    zip3 [1] [2] (3--10) = [(1,2,3)]
    zip3 [1] [2] [3] = [(1,2,3)]
    zip3 [] [1] [1] = []
    zip3 [1] [] [1] = []
    zip3 [1] [1] [] = []
    zip3 [1] [] [] = []
    zip3 [] [1] [] = []
    zip3 [] [] [1] = []
    zip3 [] [] [] = []
  **)
  let unzip3 l =
    foldr (fun (a,b,c) (t1,t2,t3) -> (a::t1, b::t2, c::t3)) ([],[],[]) l
  (**T
    unzip3 (zip3 (1--3) (3--1) (2--4)) = ((1--3), (3--1), (2--4))
    unzip3 [(1,2,3)] = ([1], [2], [3])
    unzip3 [] = ([], [], [])
  **)



  (* Extra folds *)

  let scanl f init lst = rev @@ snd @@
    foldl (fun (s,l) i -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanl multiply 1 (2--5) = [1; 2; 6; 24; 120]
    scanl multiply 1 [] = [1]
    scanl multiply 1 [2] = [1; 2]
  **)
  let scanl1 f l = scanl f (head l) (tail l)
  (**T
    scanl1 multiply (1--5) = [1; 2; 6; 24; 120]
    scanl1 multiply [1] = [1]
    optNF (scanl1 multiply) [] = None
  **)

  let scanr f init lst = snd @@
    foldr (fun i (s,l) -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanr multiply 1 [5;4;3;2] = [120; 24; 6; 2; 1]
    scanr multiply 1 [] = [1]
    scanr multiply 1 [2] = [2; 1]
  **)
  let scanr1 f l = let l,i = pop l in scanr f i l
  (**T
    scanr1 multiply [5;4;3;2;1] = [120; 24; 6; 2; 1]
    scanr1 multiply [1] = [1]
    optNF (scanr1 multiply) [] = None
  **)

  let maximum lst = foldl1 max lst
  (**T
    maximum [1;2;3;0;1;4;3;1] = 4
    maximum [1] = 1
    optNF maximum [] = None
  **)
  let maximumBy f lst = foldl1 (maxBy f) lst
  (**T
    maximumBy square (-3 -- 2) = -3
    maximumBy square (-1 -- 2) = 2
    optNF (maximumBy square) [] = None
  **)
  let maximumByWith f lst = maximumBy snd (map (fupler f) lst)
  (**T
    maximumByWith square (-3 -- 2) = (-3, 9)
    maximumByWith square (-1 -- 2) = (2, 4)
    optNF (maximumByWith square) [] = None
  **)
  let minimum lst = foldl1 min lst
  (**T
    minimum [1;2;3;0;1;4;3;1] = 0
    minimum [1] = 1
    optNF minimum [] = None
  **)

  let minimumBy f lst = foldl1 (minBy f) lst
  (**T
    minimumBy square (-3 -- (-1)) = -1
    minimumBy square (-1 -- 2) = 0
    optNF (minimumBy square) [] = None
  **)
  let minimumByWith f lst = minimumBy snd (map (fupler f) lst)
  (**T
    minimumByWith square (-3 -- (-1)) = (-1, 1)
    minimumByWith square (-1 -- 2) = (0, 0)
    optNF (minimumByWith square) [] = None
  **)

  let product lst = foldl ( * ) 1 lst
  (**T
    product (1--10) = 3628800
    product [1;2] = 2
    product [1] = 1
    product [0] = 0
    product [] = 1
  **)
  let productf lst = foldl ( *. ) 1. lst
  (**T
    productf (1.--.10.) = 3628800.
    productf [1.;2.] = 2.
    productf [1.] = 1.
    productf [0.] = 0.
    productf [] = 1.
  **)
  let sum lst = foldl (+) 0 lst
  (**T
    sum (1--10) = 55
    sum [1;2] = 3
    sum [1] = 1
    sum [0] = 0
    sum [] = 0
  **)
  let sumf lst = foldl (+.) 0. lst
  (**T
    sumf (1.--.10.) = 55.
    sumf [1.;2.] = 3.
    sumf [1.] = 1.
    sumf [0.] = 0.
    sumf [] = 0.
  **)
  let average lst = (sum lst) / (length lst)
  (**T
    average (1--10) = 5
    average [1] = 1
    optEx Division_by_zero average [] = None
  **)
  let averagef lst = (sumf lst) /. (float (length lst))
  (**T
    averagef (1.--.10.) = 5.5
    averagef [1.] = 1.
    isNaN (averagef [])
  **)

  let count p l =
    let rec aux c p l = match l with
      | [] -> c
      | (h::t) -> aux (c + (if p h then 1 else 0)) p t in
    aux 0 p l
  (**T
    count (gt 5) (0--10) = 5
    count ((=) 1) (1--10) = 1
    count ((=) 1) [] = 0
    count ((=) 1) [1;2;3;1;2;3] = 2
  **)


  (* Subsequences *)

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
    sub 0 0 (1--10) = []
    sub 0 1 (1--10) = [1]
    sub 0 (-1) (1--10) = []
  **)
  let slice first last lst =
    let len = if first < 0 || last < 0 then length lst else 0 in
    let first = if first < 0 then len + first else first in
    let last = if last < 0 then len + last else last in
    sub first (last-first+1) lst
  (**T
    slice 2 3 (explode "foobar") = ['o'; 'b']
    slice (-3) (-1) (explode "foobar") = ['b'; 'a'; 'r']
    slice 0 0 (1--10) = [1]
    slice 0 1 (1--10) = [1; 2]
    slice 1 0 (1--10) = []
    slice 0 (-1) (1--10) = (1--10)
  **)



  (* Take and drop *)

  let span f lst =
    let rec aux f res l = match l with
      | (h::t) when f h -> aux f (h::res) t
      | x -> (rev res, x) in
    aux f [] lst
  (**T
    span id [true; false; false; true] = ([true], [false; false; true])
    span (lessOrEqualTo 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
    span id [] = ([], [])
    span id [true] = ([true], [])
    span id [false] = ([], [false])
    span id [false;true] = ([], [false;true])
    span id [true;false] = ([true], [false])
    span id [true;true] = ([true;true], [])
    span id [false;false] = ([], [false;false])
  **)
  let break p = span (fun i -> not (p i))
  (**T
    break id [false; false; true; false] = ([false; false], [true; false])
    break (greaterThan 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
  **)
  let splitAt n lst =
    let rec aux c res l = match c, l with
        | x, (h::t) when x > 0 -> aux (c-1) (h::res) t
        | _, t -> (rev res, t) in
    aux n [] lst
  (**T
    splitAt 3 (explode "foobar") = (['f'; 'o'; 'o'], ['b'; 'a'; 'r'])
    splitAt 1 [1] = ([1], [])
    splitAt 0 [1] = ([], [1])
    splitAt 1 [] = ([], [])
    splitAt 0 [] = ([], [])
  **)

  let takeWhile f lst = fst (span f lst)
  (**T
    takeWhile (lt 5) (1--10) = (1--4)
    takeWhile (lt 5) (6--10) = []
    takeWhile (lt 5) [] = []
    takeWhile (lt 5) (1--3) = (1--3)
  **)
  let takeUntil f lst = fst (break f lst)
  (**T
    takeUntil (gte 5) (1--10) = (1--4)
    takeUntil (gte 5) (6--10) = []
    takeUntil (gte 5) [] = []
    takeUntil (gte 5) (1--3) = (1--3)
  **)
  let take n lst = fst (splitAt n lst)
  (**T
    take 3 (1--10) = (1--3)
    take 0 (1--10) = []
    take 15 (1--10) = (1--10)
    take 3 [] = []
    take 0 [] = []
  **)

  let dropWhile f lst = snd (span f lst)
  (**T
    dropWhile (lt 5) (1--10) = (5--10)
    dropWhile (lt 5) (6--10) = (6--10)
    dropWhile (lt 5) [] = []
    dropWhile (lt 5) (1--3) = []
  **)
  let dropUntil f lst = snd (break f lst)
  (**T
    dropUntil (gte 5) (1--10) = (5--10)
    dropUntil (gte 5) (6--10) = (6--10)
    dropUntil (gte 5) [] = []
    dropUntil (gte 5) (1--3) = []
  **)
  let drop n lst = snd (splitAt n lst)
  (**T
    drop 3 (1--10) = (4--10)
    drop 0 (1--10) = (1--10)
    drop 15 (1--10) = []
    drop 3 [] = []
    drop 0 [] = []
  **)

  let span2 f a b =
    let rec aux f r1 r2 a b = match a,b with
      | (x::xs), (y::ys) when f x y -> aux f (x::r1) (y::r2) xs ys
      | xs, ys -> ((rev r1, rev r2), (xs, ys)) in
    aux f [] [] a b
  (**T
    span2 (=) (1--6) [1;2;3;5] = (((1--3), (1--3)), ((4--6), [5]))
    span2 (<>) (1--6) (7--1) = (((1--3), (7--5)), ((4--6), (4--1)))
    span2 (=) [1] [1] = (([1], [1]), ([], []))
    span2 (<>) [1] [1] = (([], []), ([1], [1]))
    span2 (=) [] [] = (([], []), ([], []))
    span2 (=) [] [1] = (([], []), ([], [1]))
    span2 (=) [1] [] = (([], []), ([1], []))
  **)
  let break2 f = span2 (fun x y -> not (f x y))
  (**T
    break2 (<>) (1--6) [1;2;3;5] = (((1--3), (1--3)), ((4--6), [5]))
    break2 (=) (1--6) (7--1) = (((1--3), (7--5)), ((4--6), (4--1)))
    break2 (<>) [1] [1] = (([1], [1]), ([], []))
    break2 (=) [1] [1] = (([], []), ([1], [1]))
    break2 (=) [] [] = (([], []), ([], []))
    break2 (=) [] [1] = (([], []), ([], [1]))
    break2 (=) [1] [] = (([], []), ([1], []))
  **)
  let splitAt2 n a b =
    let rec aux c r1 r2 a b = match c, a, b with
        | c, (x::xs), (y::ys) when c > 0 -> aux (c-1) (x::r1) (y::r2) xs ys
        | _, a,b -> ((rev r1, rev r2), (a, b)) in
    aux n [] [] a b
  (**T
    splitAt2 3 (1--6) [1;2;3;5] = (((1--3), (1--3)), ((4--6), [5]))
    splitAt2 3 (1--6) (7--1) = (((1--3), (7--5)), ((4--6), (4--1)))
    splitAt2 1 [1] [1] = (([1], [1]), ([], []))
    splitAt2 0 [1] [1] = (([], []), ([1], [1]))
    splitAt2 0 [] [] = (([], []), ([], []))
    splitAt2 0 [] [1] = (([], []), ([], [1]))
    splitAt2 0 [1] [] = (([], []), ([1], []))
  **)

  let takeWhile2 f a b = fst (span2 f a b)
  (**T
    takeWhile2 (=) (1--6) [1;2;3;5] = ((1--3), (1--3))
    takeWhile2 (<>) (1--6) (7--1) = ((1--3), (7--5))
  **)
  let takeUntil2 f a b = fst (break2 f a b)
  (**T
    takeUntil2 (<>) (1--6) [1;2;3;5] = ((1--3), (1--3))
    takeUntil2 (=) (1--6) (7--1) = ((1--3), (7--5))
  **)
  let take2 n a b = fst (splitAt2 n a b)
  (**T
    take2 3 (1--6) [1;2;3;5] = ((1--3), (1--3))
    take2 3 (1--6) (7--1) = ((1--3), (7--5))
  **)
  let dropWhile2 f a b = snd (span2 f a b)
  (**T
    dropWhile2 (=) (1--6) [1;2;3;5] = ((4--6), [5])
    dropWhile2 (<>) (1--6) (7--1) = ((4--6), (4--1))
  **)
  let dropUntil2 f a b = snd (break2 f a b)
  (**T
    dropUntil2 (<>) (1--6) [1;2;3;5] = ((4--6), [5])
    dropUntil2 (=) (1--6) (7--1) = ((4--6), (4--1))
  **)
  let drop2 n a b = snd (splitAt2 n a b)
  (**T
    drop2 3 (1--6) [1;2;3;5] = ((4--6), [5])
    drop2 3 (1--6) (7--1) = ((4--6), (4--1))
  **)



  (* Map with window *)

  let mapWindow f n l =
    let rec aux f wnd lst res =
      match lst with
        | [] -> rev res
        | (h::t) ->
          let wnd = tail wnd @ [h] in
          aux f wnd t ((f wnd) :: res) in
    let wnd, t = splitAt n l in
    if wnd = [] then []
    else aux f wnd t [f wnd]
  (**T
    mapWindow sum 1 (1--4) = (1--4)
    mapWindow sum 2 (1--4) = [3; 5; 7]
    mapWindow sum 3 (1--4) = [6; 9]
    mapWindow sum 3 [] = []
    mapWindow sum 3 [1] = [1]
    mapWindow sum 3 (1--3) = [6]
  **)



  (* Interlace *)

  let interlace elem l =
    let rec aux l l2 = match l with
        | [] -> (match l2 with [] -> [] | (h::t) -> List.rev t)
        | (h::t) -> aux t (elem :: h :: l2) in
    aux l []
  (**T
    interlace 0 [1; 2; 3] = [1; 0; 2; 0; 3]
    implode @@ interlace '-' @@ explode "abcde" = "a-b-c-d-e"
    interlace 0 [] = []
    interlace 0 [1] = [1]
    interlace 0 [1;2] = [1; 0; 2]
  **)



  (* Predicates *)

  let all = for_all
  (**T
    all (gt 5) (1--10) = false
    all (lt 11) (1--10) = true
    all (gt 4) [] = true
  **)
  let any = exists
  (**T
    any (gt 5) (1--10) = true
    any (gt 11) (1--10) = false
    any (gt 4) [] = false
  **)

  let allEqual l = match l with
    | [] -> true
    | (h::t) -> all ((=) h) t
  (**T
    allEqual (1--10) = false
    allEqual [] = true
    allEqual (replicate 10 'a') = true
  **)

  let includes x = exists (fun y -> x = y)
  (**T
    includes 4 (1--10) = true
    includes 0 (1--10) = false
    includes 5 [] = false
  **)
  let has = includes
  (**T
    has 'a' ('a'-~'g') = true
    has 0 (1--10) = false
  **)
  let elem = includes
  (**T
    elem 'b' ['a'; 'c'] = false
    elem "foo" ["bar"; "baz"; "foo"] = true
  **)
  let notElem x lst = not @@ elem x lst
  (**T
    notElem 4 (1--10) = false
    notElem 0 (1--10) = true
    notElem 5 [] = true
  **)

  let null = function [] -> true | _ -> false
  (**T
    null [] = true
    null (1--10) = false
  **)



  (* Associations *)

  let rec assocBy f l =
    match l with
      | [] -> raise Not_found
      | (k,v)::t when f k -> v
      | _::t -> assocBy f t
  (**T
    assocBy (gt 5) (zip (1--10) ('a'-~'z')) = 'f'
    optNF (assocBy (gt 10)) (zip (1--10) ('a'-~'z')) = None
  **)

  let lookup e l = optNF (assoc e) l
  (**T
    lookup 4 (zip (1--10) ('a'-~'z')) = Some 'd'
    lookup 11 (zip (1--10) ('a'-~'z')) = None
  **)
  let lookupBy f l = optNF (assocBy f) l
  (**T
    lookupBy (gt 5) (zip (1--10) ('a'-~'z')) = Some 'f'
    lookupBy (gt 10) (zip (1--10) ('a'-~'z')) = None
  **)



  (* Sorting *)

  let sort ?(cmp=compare) l = List.sort cmp l
  (**T
    sort (10--1) = (1--10)
    sort [] = []
    sort [1] = [1]
    sort ~cmp:subtract (1--10) = (10--1)
    sort ~cmp:subtract [] = []
    sort ['a'; 'c'; 'b'] = ('a'-~'c')
  **)
  let sortBy ?(cmp=compare) f l =
    map (fupler f) l |> sort ~cmp:(fun (_,a) (_,b) -> cmp a b) |> map fst
  (**T
    sortBy square [-3; 2; 1] = [1; 2; -3]
    sortBy square [] = []
    sortBy square [1] = [1]
    sortBy ~cmp:subtract square [-3; 2; 1] = [-3; 2; 1]
    sortBy ~cmp:subtract square [] = []
    sortBy ~cmp:subtract square [1] = [1]
  **)



  (* Filtering *)

  (* let filter = filter *)
  (**T
    filter even (1--10) = [2;4;6;8;10]
    filter odd (1--10) = [1;3;5;7;9]
    filter even [1] = []
    filter odd [1] = [1]
    filter even [] = []
  **)

  let filterWithIndex f s =
    let rec aux f l i res =
      match l with
        | [] -> rev res
        | (h::t) when f h i -> aux f t (succ i) (h::res)
        | (h::t) -> aux f t (succ i) res in
    aux f s 0 []
  (**T
    filterWithIndex (fun _ i -> i > 5) (1--9) = (7--9)
    filterWithIndex (fun _ i -> i > 10) (1--9) = []
    filterWithIndex (fun _ i -> i > 10) [] = []
  **)

  let reject f l = filter (fun i -> not (f i)) l
  (**T
    reject (gt 4) (1--5) = (1--4)
    reject (gt 4) [] = []
    reject (gt 0) (1--5) = []
    reject (gt 5) (1--5) = (1--5)
    reject (gt 3) (5--1) = (3--1)
  **)

  let without x l = filter ((<>) x) l
  (**T
    without 4 [1; 2; 4; 1; 2; 4] = [1; 2; 1; 2]
    without 4 [] = []
    without 4 [4] = []
    without 4 [1] = [1]
  **)

  let compact l = map (function Some x -> x | _ -> failwith "compact")
                      (filter ((!=) None) l)
  (**T
    compact [None; Some 10; Some 5; None; None; Some 8] = [10; 5; 8]
    compact @@ map (optIf (greaterThan 0) id) (-3--3) = [1; 2; 3]
    compact [] = []
    compact [None] = []
    compact [Some 'a'] = ['a']
    compact [Some 'a'; None] = ['a']
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
    squeeze [] = []
    squeeze [1] = [1]
    squeeze [1;1] = [1]
    squeeze [1;1;2] = [1;2]
    squeeze [2;1;1] = [2;1]
  **)
  let uniq ?cmp l = squeeze (sort ?cmp l)
  (**T
    uniq [3;1;2;2;2;3;3;1] = [1; 2; 3]
    uniq ~cmp:subtract [3;1;2;2;2;3;3;1] = [3;2;1]
    uniq [] = []
    uniq [1] = [1]
    uniq [1;1] = [1]
    uniq [1;1;2] = [1;2]
    uniq [2;1;1] = [1;2]
  **)

  let pick indices l = map (flip nth l) indices
  (**T
    pick [2; 3] (explode "foobar") = ['o'; 'b']
    pick [] [] = []
    pick [] (1--10) = []
    pick [0; -1] (1--10) = [1; 10]
    optNF (pick [2;3]) [1;2;3] = None
    optNF (pick [2;3]) [] = None
  **)
  let pickWith funcs l = map ((|>) l) funcs
  (**T
    pickWith [first; last] (explode "foobar") = ['f'; 'r']
  **)



  (* Neighbours *)

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
    neighbourLists 7 3 [] = ([], [])
  **)


  (* Set operations *)

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
    diffSorted [1;2] [] = [1;2]
    diffSorted [] [1;2] = []
    diffSorted [1] [1] = []
    diffSorted [1] [] = [1]
    diffSorted [] [1] = []
    diffSorted [] [] = []
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
    diff [1;2] [] = [1;2]
    diff [] [1;2] = []
    diff [1] [1] = []
    diff [1] [] = [1]
    diff [] [1] = []
    diff [] [] = []
  **)



  (* Grouping *)

  let groupsOf n l =
    if l = [] then [l] else unfoldlUntil null (splitAt (max 1 n)) l
  (**T
    groupsOf 3 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
    groupsOf 3 [] = [[]]
    groupsOf 3 [1] = [[1]]
    groupsOf 3 (1--3) = [(1--3)]
    groupsOf 5 (1--3) = [(1--3)]
    groupsOf 3 (1--4) = [(1--3); [4]]
    groupsOf 1 (1--3) = [[1]; [2]; [3]]
    groupsOf 0 (1--3) = [[1]; [2]; [3]]
    groupsOf (-1) (1--3) = [[1]; [2]; [3]]
  **)
  let splitInto n l = groupsOf (int (ceil (float (len l) /. float (max 1 n)))) l
  (**T
    splitInto 4 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
    splitInto 3 (1--3) = [[1]; [2]; [3]]
    splitInto 1 (1--3) = [(1--3)]
    splitInto 0 (1--3) = [(1--3)]
    splitInto (-1) (1--3) = [(1--3)]
    splitInto 2 [] = [[]]
    splitInto 1 [] = [[]]
  **)
  let groupWith p l =
    let rec aux p v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when p v h -> aux p v t (h::rl) res
      | (h::t) -> aux p h t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev (aux p h t [h] [])
  (**T
    groupWith (fun x y -> x*x = y*y) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
    groupWith (=) (1--3) = [[1]; [2]; [3]]
    groupWith (=) [1;1;2;2;2;3;3;1] = [[1;1]; [2;2;2]; [3;3]; [1]]
    groupWith (=) [1] = [[1]]
    groupWith (=) [] = []
  **)
  let groupBy f l =
    let rec aux f v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when (f h) = v -> aux f v t (h::rl) res
      | (h::t) -> aux f (f h) t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev @@ aux f (f h) t [h] []
  (**T
    groupBy (fun x -> x*x) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
    groupBy (fun x -> x) (1--3) = [[1]; [2]; [3]]
    groupBy (fun x -> signum x) (-3--3) = [(-3--(-1)); [0]; (1--3)]
    groupBy id [1] = [[1]]
    groupBy id [] = []
  **)
  let group l = groupBy id l
  (**T
    group [1;1;2;2;3;1] = [[1;1]; [2;2]; [3]; [1]]
    group [] = []
    group [1] = [[1]]
    group [1;2] = [[1]; [2]]
    group [1;1;2] = [[1;1]; [2]]
    group [1;2;2] = [[1]; [2;2]]
  **)


  (* Rotation *)

  let rotate n l =
    if l = [] then l
    else
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
    rotate 0 (1--10) = (1--10)
    rotate 10 (1--10) = (1--10)
    rotate (-10) (1--10) = (1--10)
    rotate 0 [] = []
    rotate 1 [] = []
    rotate (-1) [] = []
  **)



  (* Parallel iterators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l |> par_map ~process_count process |> combine
  (**T
    par_mapReduce ~combine:concat ~process:(map succ) (1--10) = map succ (1--10)
    par_mapReduce ~process_count:2 ~combine:concat ~process:(map succ) (1--10) = map succ (1--10)
    par_mapReduce ~process_count:2 ~combine:(concat @. reverse) ~process:(map succ) (1--10) = map succ ((6--10)@(1--5))
    par_mapReduce ~process_count:2 ~combine:(concat @. reverse) ~process:(map succ) [] = []
    par_mapReduce ~process_count:2 ~combine:(concat @. reverse) ~process:(map succ) [1] = [2]
  **)

  let pmapReduce combine process = par_mapReduce ~combine ~process
  (**T
    pmapReduce concat (map succ) (1--10) = map succ (1--10)
    pmapReduce ~process_count:2 concat (map succ) (1--10) = map succ (1--10)
    pmapReduce ~process_count:2 (concat @. reverse) (map succ) (1--10) = map succ ((6--10)@(1--5))
    pmapReduce ~process_count:2 (concat @. reverse) (map succ) [] = []
    pmapReduce ~process_count:2 (concat @. reverse) (map succ) [1] = [2]
  **)

  let pfoldl r f init = pmapReduce (foldl1 r) (foldl f init)
  (**T
    pfoldl (+) (+) 0 (1--10) = sum (1--10)
    pfoldl ~process_count:2 (+) (+) 0 (1--10) = sum (1--10)
    pfoldl ~process_count:2 (+) (+) 0 [1] = sum [1]
    pfoldl ~process_count:1 (+) (+) 0 (1--10) = sum (1--10)
    pfoldl ~process_count:1 (+) (+) 0 [1] = sum [1]
    pfoldl ~process_count:0 (+) (+) 0 (1--10) = sum (1--10)
    pfoldl ~process_count:0 (+) (+) 0 [1] = sum [1]
    pfoldl ~process_count:3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldl ~process_count:3 (+) (+) 0 [1] = sum [1]
    pfoldl ~process_count:2 (multiply) (multiply) 1 (1--10) = product (1--10)
    pfoldl ~process_count:2 (multiply) (multiply) 1 [1] = product [1]
    optNF (pfoldl ~process_count:2 (+) (+) 0) [] = Some 0
  **)
  let pfoldl1 f = pmapReduce (foldl1 f) (foldl1 f)
  (**T
    pfoldl1 (+) (1--10) = sum (1--10)
    pfoldl1 ~process_count:3 (+) (1--10) = sum (1--10)
    pfoldl1 ~process_count:2 (+) [1] = sum [1]
    pfoldl1 ~process_count:1 (multiply) (1--10) = product (1--10)
    pfoldl1 ~process_count:0 (multiply) [1] = product [1]
    optNF (pfoldl1 ~process_count:2 (+)) [] = None
  **)
  let pfoldr r f init = pmapReduce (foldr1 r) (foldr f init)
  (**T
    pfoldr ~process_count:2 (+) (+) 0 (1--10) = sum (1--10)
    pfoldr ~process_count:2 (+) (+) 0 [1] = sum [1]
    pfoldr (+) (+) 0 [1] = sum [1]
    pfoldr ~process_count:1 (+) (+) 0 (1--10) = sum (1--10)
    pfoldr ~process_count:1 (+) (+) 0 [1] = sum [1]
    pfoldr ~process_count:0 (+) (+) 0 (1--10) = sum (1--10)
    pfoldr ~process_count:0 (+) (+) 0 [1] = sum [1]
    pfoldr ~process_count:3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldr ~process_count:3 (+) (+) 0 [1] = sum [1]
    pfoldr ~process_count:2 (multiply) (multiply) 1 (1--10) = product (1--10)
    pfoldr ~process_count:2 (multiply) (multiply) 1 [1] = product [1]
    optNF (pfoldr ~process_count:2 (+) (+) 0) [] = Some 0
  **)
  let pfoldr1 f = pmapReduce (foldr1 f) (foldr1 f)
  (**T
    pfoldr1 ~process_count:3 (+) (1--10) = sum (1--10)
    pfoldr1 (+) (1--10) = sum (1--10)
    pfoldr1 ~process_count:2 (+) [1] = sum [1]
    pfoldr1 ~process_count:1 (multiply) (1--10) = product (1--10)
    pfoldr1 ~process_count:0 (multiply) [1] = product [1]
    optNF (pfoldr1 ~process_count:2 (+)) [] = None
  **)

  let piter f = pmapReduce ignore (iter f)
  (**T
    piter ~process_count:3 (ignore @. succ) (1--10) = ()
    piter ~process_count:2 (ignore @. succ) (1--10) = ()
    piter ~process_count:1 (ignore @. succ) (1--10) = ()
    piter ~process_count:0 (ignore @. succ) (1--10) = ()
    piter ~process_count:3 (ignore @. succ) [1] = ()
    piter ~process_count:2 (ignore @. succ) [1] = ()
    piter ~process_count:1 (ignore @. succ) [1] = ()
    piter ~process_count:0 (ignore @. succ) [1] = ()
    piter ~process_count:3 (ignore @. succ) [] = ()
    piter ~process_count:2 (ignore @. succ) [] = ()
    piter ~process_count:1 (ignore @. succ) [] = ()
    piter ~process_count:0 (ignore @. succ) [] = ()
    piter (ignore @. succ) [] = ()
    piter (ignore @. succ) (1--10) = ()
    piter (ignore @. succ) [1] = ()
  **)
  let pmap f = pmapReduce concat (map f)
  (**T
    pmap ~process_count:3 succ (1--10) = map succ (1--10)
    pmap ~process_count:2 succ (1--10) = map succ (1--10)
    pmap ~process_count:1 succ (1--10) = map succ (1--10)
    pmap ~process_count:0 succ (1--10) = map succ (1--10)
    pmap ~process_count:3 succ [1] = map succ [1]
    pmap ~process_count:2 succ [1] = map succ [1]
    pmap ~process_count:1 succ [1] = map succ [1]
    pmap ~process_count:0 succ [1] = map succ [1]
    pmap ~process_count:3 succ [] = map succ []
    pmap ~process_count:2 succ [] = map succ []
    pmap ~process_count:1 succ [] = map succ []
    pmap ~process_count:0 succ [] = map succ []
    pmap succ (1--10) = map succ (1--10)
    pmap succ [] = map succ []
    pmap succ [1] = map succ [1]
  **)
  let pfilter f = pmapReduce concat (filter f)
  (**T
    pfilter even (1--10) = [2;4;6;8;10]
    pfilter odd (1--10) = [1;3;5;7;9]
    pfilter even [1] = []
    pfilter odd [1] = [1]
    pfilter even [] = []
  **)

  let pfoldlSeqN ?process_count n r f init l =
    foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)
  (**T
    pfoldlSeqN 3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldlSeqN ~process_count:2 3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldlSeqN ~process_count:2 3 (+) (+) 0 [1] = sum [1]
    pfoldlSeqN ~process_count:1 3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldlSeqN ~process_count:1 3 (+) (+) 0 [1] = sum [1]
    pfoldlSeqN ~process_count:0 3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldlSeqN ~process_count:0 3 (+) (+) 0 [1] = sum [1]
    pfoldlSeqN ~process_count:3 3 (+) (+) 0 (1--10) = sum (1--10)
    pfoldlSeqN ~process_count:3 3 (+) (+) 0 [1] = sum [1]
    pfoldlSeqN ~process_count:2 3 (multiply) (multiply) 1 (1--10) = product (1--10)
    pfoldlSeqN ~process_count:2 3 (multiply) (multiply) 1 [1] = product [1]
    optNF (pfoldlSeqN ~process_count:2 3 (+) (+) 0) [] = Some 0
  **)

  let piterSeqN ?process_count n r f l =
    iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)
  (**T
    piterSeqN ~process_count:3 1 ignore succ (1--10) = ()
    piterSeqN ~process_count:2 2 ignore succ (1--10) = ()
    piterSeqN ~process_count:1 1 ignore succ (1--10) = ()
    piterSeqN ~process_count:0 4 ignore succ (1--10) = ()
    piterSeqN ~process_count:3 1 ignore succ [1] = ()
    piterSeqN ~process_count:2 6 ignore succ [1] = ()
    piterSeqN ~process_count:1 1 ignore succ [1] = ()
    piterSeqN ~process_count:0 1 ignore succ [1] = ()
    piterSeqN ~process_count:3 2 ignore succ [] = ()
    piterSeqN ~process_count:2 1 ignore succ [] = ()
    piterSeqN ~process_count:1 3 ignore succ [] = ()
    piterSeqN ~process_count:0 1 ignore succ [] = ()
    piterSeqN 0 ignore succ [] = ()
    piterSeqN 1 ignore succ (1--10) = ()
    piterSeqN 1 ignore succ [1] = ()
  **)

  let pinit ?process_count f l =
    let process_count = max 1 (process_count |? !global_process_count) in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))
  (**T
    pinit succ 10 = (1--10)
    pinit pred 10 = ((-1)--8)
    pinit succ 0 = []
    pinit succ 1 = [1]
    pinit ~process_count:4 succ 10 = (1--10)
    pinit ~process_count:3 pred 10 = ((-1)--8)
    pinit ~process_count:2 succ 0 = []
    pinit ~process_count:1 pred 10 = ((-1)--8)
    pinit ~process_count:1 succ 1 = [1]
    pinit ~process_count:0 succ 1 = [1]
  **)

  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    concat (par_map ~process_count (uncurry (zipWith f)) (zip aspl bspl))
  (**T
    pzipWith (+) (1--10) (1--10) = map (dup (+)) (1--10)
    pzipWith (-) (1--5) (3--1) = [-2; 0; 2]
    pzipWith (-) (1--3) (5--1) = [-4; -2; 0]
    pzipWith (+) [1] (1--10) = [2]
    pzipWith (+) (1--10) [1] = [2]
    pzipWith (+) [1] [1] = [2]
    pzipWith (+) [] (1--10) = []
    pzipWith (+) (1--10) [] = []
    pzipWith (+) [] [] = []
    pzipWith (+) ~process_count:3 (1--10) (1--10) = map (dup (+)) (1--10)
    pzipWith (-) ~process_count:2 (1--5) (3--1) = [-2; 0; 2]
    pzipWith (-) ~process_count:1 (1--3) (5--1) = [-4; -2; 0]
    pzipWith (+) ~process_count:0 [1] (1--10) = [2]
  **)

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l
      |> mapWithIndex tuple
      |> par_map  ~process_count process |> combine
  (**T
    par_mapReduceWithIndex ~process_count:5 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) (0--8) = [9;6;5;2;1]
    par_mapReduceWithIndex ~process_count:5 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) [] = []
    par_mapReduceWithIndex ~process_count:5 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) [0] = [1]
    par_mapReduceWithIndex ~process_count:0 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) [0] = [1]
    par_mapReduceWithIndex ~process_count:0 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) [] = []
    par_mapReduceWithIndex ~process_count:0 ~combine:(reverse @. concat) ~process:(fun (l, idx) -> map succ (if odd idx then [] else l)) (0--9) = (10--1)
  **)

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process
  (**T
    pmapReduceWithIndex ~process_count:5 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) (0--8) = [9;6;5;2;1]
    pmapReduceWithIndex ~process_count:5 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) [] = []
    pmapReduceWithIndex ~process_count:5 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) [0] = [1]
    pmapReduceWithIndex ~process_count:0 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) [0] = [1]
    pmapReduceWithIndex ~process_count:0 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) [] = []
    pmapReduceWithIndex ~process_count:0 (reverse @. concat) (fun (l, idx) -> map succ (if odd idx then [] else l)) (0--9) = (10--1)
  **)

  let pmapWithInit init f =
    pmapReduceWithIndex concat (fun (sublist, idx) -> map f (init sublist idx))
  (**T
    pmapWithInit ~process_count:2 (fun l i -> if odd i then reverse l else l) succ (0--9) = (1--5) @ (10--6)
    pmapWithInit ~process_count:2 (fun l i -> if odd i then reverse l else l) succ [0] = [1]
    pmapWithInit ~process_count:2 (fun l i -> if odd i then reverse l else l) succ [] = []
    pmapWithInit ~process_count:1 (fun l i -> if odd i then reverse l else l) succ [0] = [1]
    pmapWithInit ~process_count:1 (fun l i -> if odd i then reverse l else l) succ [] = []
    pmapWithInit ~process_count:(-1) (fun l i -> if odd i then reverse l else l) succ [0] = [1]
    pmapWithInit ~process_count:0 (fun l i -> if odd i then reverse l else l) succ [] = []
  **)
end

include PreList

let (--.) = rangef
(**T
  (1.--.3.) = [1.; 2.; 3.]
  (1.--.1.) = [1.]
  (1.--.0.) = [1.; 0.]
**)
let (@*) l n = times n l
(**T
  [1; 2; 3] @* 3 = [1; 2; 3; 1; 2; 3; 1; 2; 3]
  [1; 2; 3] @* 1 = [1;2;3]
  [1; 2; 3] @* 0 = []
  [1; 2; 3] @* (-1) = []
**)

let (-~) = charRange
(**T
  ('a'-~'c') = ['a'; 'b'; 'c']
  ('a'-~'a') = ['a']
  ('c'-~'a') = ['c'; 'b'; 'a']
  implode ('\000'-~'\255') = ('\000'--^'\255')
  implode ('\255'-~'\000') = ('\255'--^'\000')
**)




module PreArray =
struct
  include Array

  (* Basic operations *)

  let len = length
  (**T
    alen (1--|10) = 10
    alen [|1|] = 1
    alen [||] = 0
  **)

  let init f l = init l f
  (**T
    ainit succ 10 = (1--|10)
    ainit pred 10 = (-1--|8)
    ainit succ 0 = [||]
    ainit succ 1 = [|1|]
  **)

  let range s e =
    if s > e
    then init ((-) s) (s-e+1)
    else init ((+) s) (e-s+1)
  (**T
    arange 0 1 = [|0; 1|]
    arange 0 0 = [|0|]
    arange 2 4 = [|2; 3; 4|]
    arange 2 0 = [|2; 1; 0|]
    arange (max_int - 1) max_int = [|max_int - 1; max_int|]
    arange max_int (max_int - 1) = [|max_int; max_int - 1|]
    arange (min_int + 1) min_int = [|min_int + 1; min_int|]
    arange min_int (min_int + 1) = [|min_int; min_int + 1|]
  **)
  let rangef s e =
    if s > e
    then init (fun i -> s -. (float i)) (int (ceil (s-.e+.1.)))
    else init (fun i -> s +. (float i)) (int (ceil (e-.s+.1.)))
  (**T
    arangef 1. 3. = [|1.; 2.; 3.|]
    arangef 1. 1. = [|1.|]
    arangef 1. 0. = [|1.; 0.|]
  **)
  let charRange sc ec =
    let s, e = ord sc, ord ec in
    if s > e
    then init (fun i -> chr (s - i)) (s-e+1)
    else init (fun i -> chr (s + i)) (e-s+1)
  (**T
    acharRange 'a' 'c' = [|'a'; 'b'; 'c'|]
    acharRange 'a' 'a' = [|'a'|]
    acharRange 'b' 'a' = [|'b'; 'a'|]
    acharRange '\000' '\255' = aexplode ('\000'--^'\255')
    acharRange '\255' '\000' = aexplode ('\255'--^'\000')
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
    PreArray.normalizeIndex 2 [||] = 2
    PreArray.normalizeIndex (-2) [||] = -2
    PreArray.normalizeIndex 0 (1--|10) = 0
    PreArray.normalizeIndex 2 (1--|10) = 2
    PreArray.normalizeIndex (-1) (1--|10) = 9
    PreArray.normalizeIndex (-2) (1--|10) = 8
    anormalizeIndex (-1) (1--|2) = 1
    PreArray.normalizeIndex (-2) (1--|2) = 0
  **)

  let replicate n v = make (max 0 n) v
  (**T
    PreArray.replicate 5 '-' = [|'-'; '-'; '-'; '-'; '-'|]
    PreArray.replicate 1 '-' = [|'-'|]
    areplicate 0 '-' = [||]
    PreArray.replicate (-1) '-' = [||]
  **)

  let times n a = concat (PreList.replicate n a)
  (**T
    atimes 3 (1--|3) = [|1;2;3;1;2;3;1;2;3|]
    atimes 0 (1--|3) = [||]
    atimes 1 (1--|3) = (1--|3)
    atimes (-1) (1--|3) = [||]
    atimes 4 [||] = [||]
    atimes 3 [|1|] = [|1;1;1|]
  **)

  let cycle n a =
    let l = len a in
    if l = 0 || n <= 0 then [||]
    else
      let i = ref 0 in
      init (fun _ ->
        if !i >= l then i := 0;
        let v = unsafe_get a !i in
        i := !i + 1;
        v
      ) n
  (**T
    acycle 5 (1--|3) = [|1; 2; 3; 1; 2|]
    acycle 3 (1--|10) = [|1; 2; 3|]
    acycle 3 [|1|] = [|1;1;1|]
    acycle 3 [||] = [||]
    acycle 0 [|1|] = [||]
    acycle 1 [|1|] = [|1|]
    acycle (-1) [|1|] = [||]
  **)

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  (**T
    mapWith aiter id (1--|100000) = (1--100000)
    mapWith aiter id (1--|10) = (1--10)
    mapWith aiter succ (1--|10) = (2--11)
    mapWith aiter succ (1--|1) = [2]
    mapWith aiter succ [||] = []
  **)

  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done
  (**T
    mapWith (fun f -> aiterWithIndex (fun s i -> f (s,i))) (uncurry (+)) (0--|10) = map (multiply 2) (0--10)
    (let i = ref 0 and j = ref 0 in aiterWithIndex (fun a b -> i:=a; j:=b) (20--|30); !i = 30 && !j = 10)
    aiterWithIndex (ignore @.. add) [|1|] = ()
    aiterWithIndex (ignore @.. add) [||] = ()
  **)

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    amap succ (1--|10) = (2--|11)
    amap succ [||] = [||]
    amap succ [|1|] = [|2|]
    mapWith amap id (1--|10) = (1--10)
  **)

  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)
  (**T
    amapWithIndex (+) (0--|10) = amap (multiply 2) (0--|10)
    amapWithIndex (-) (10--|20) = areplicate 11 10
    amapWithIndex (+) [||] = [||]
    amapWithIndex (+) [|1|] = [|1|]
  **)

  (* Searching *)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []
  (**T
    afilter even (1--|10) = [|2;4;6;8;10|]
    afilter odd (1--|10) = [|1;3;5;7;9|]
    afilter even [|1|] = [||]
    afilter odd [|1|] = [|1|]
    afilter even [||] = [||]
  **)

  let filterWithIndex f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c i then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []
  (**T
    afilterWithIndex (fun e i -> e > 5) (1--|9) = (6--|9)
    afilterWithIndex (fun _ i -> i > 5) (1--|9) = (7--|9)
    afilterWithIndex (fun _ i -> i > 10) (1--|9) = [||]
    afilterWithIndex (fun _ i -> i > 10) [||] = [||]
  **)

  let findWithIndex f s =
    let rec aux f s i len =
      if i >= len then raise Not_found
      else
        let v = unsafe_get s i in
        if f v i then (v, i)
        else aux f s (i+1) len in
    aux f s 0 (len s)
  (**T
    afindWithIndex (fun v _ -> v > 4) (2--|9) = (5,3)
    afindWithIndex (fun _ i -> i > 4) (2--|9) = (7,5)
    optNF (afindWithIndex (const (gt 4))) (0--|3) = None
    optNF (afindWithIndex (const (gt 4))) [||] = None
  **)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  (**T
    afind (gt 5) (1--|9) = 6
    optNF (afind (gt 4)) (0--|3) = None
    optNF (afind (gt 4)) [||] = None
  **)
  let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)
  (**T
    afindIndex (gt 5) (1--|9) = 5
    optNF (afindIndex (gt 4)) (0--|3) = None
    optNF (afindIndex (gt 4)) [||] = None
  **)

  let indexOf v s = findIndex ((=) v) s
  (**T
    aindexOf 14 (10--|20) = 4
    optNF (aindexOf 1) (10--|20) = None
    aindexOf 'a' (aexplode "foobar") = 4
  **)

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith
  (**T
    azipWith (+) (1--|10) (1--|10) = amap (dup (+)) (1--|10)
    azipWith (-) (1--|5) (3--|1) = [|-2; 0; 2|]
    azipWith (-) (1--|3) (5--|1) = [|-4; -2; 0|]
    azipWith (+) [|1|] (1--|10) = [|2|]
    azipWith (+) (1--|10) [|1|] = [|2|]
    azipWith (+) [|1|] [|1|] = [|2|]
    azipWith (+) [||] (1--|10) = [||]
    azipWith (+) (1--|10) [||] = [||]
    azipWith (+) [||] [||] = [||]
  **)

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3
  (**T
    azipWith3 (fun a b c -> a + b + c) (1--|100000) (1--|100000) (1--|100000) = amap (multiply 3) (1--|100000)
    azipWith3 (fun a b c -> a^b^c) [|"a"|] [|"b"|] [|"c";"d"|] = [|"abc"|]
    azipWith3 (fun a b c -> a + b + c) [|1|] (1--|10) (1--|10) = [|3|]
    azipWith3 (fun a b c -> a + b + c) (1--|10) [|1|] (1--|10) = [|3|]
    azipWith3 (fun a b c -> a + b + c) (1--|10) (1--|10) [|1|] = [|3|]
    azipWith3 (fun a b c -> a + b + c) [|1|] (1--|10) [|1|] = [|3|]
    azipWith3 (fun a b c -> a + b + c) [|1|] [|1|] (1--|10) = [|3|]
    azipWith3 (fun a b c -> a + b + c) (1--|10) [||] [|1|] = [||]
    azipWith3 (fun a b c -> a + b + c) (1--|10) [|1|] [||] = [||]
    azipWith3 (fun a b c -> a + b + c) (1--|10) [||] [||] = [||]
    azipWith3 (fun a b c -> a + b + c) [||] [|1|] [|1|] = [||]
    azipWith3 (fun a b c -> a + b + c) [||] [||] [|1|] = [||]
    azipWith3 (fun a b c -> a + b + c) [||] [|1|] [||] = [||]
    azipWith3 (fun a b c -> a + b + c) [|1|] [||] [||] = [||]
    azipWith3 (fun a b c -> a + b + c) [||] [||] [||] = [||]
  **)

  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0
  (**T
    afoldl (+) 0 (1--|10) = 55
    afoldl (fun s b -> s ^ (string_of_int b)) "--" (1--|3) = "--123"
    afoldl (+) 1 [||] = 1
    afoldl (+) 1 [|1|] = 2
  **)

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a
  (**T
    afoldl1 (+) (1--|10) = 55
    afoldl1 (fun s i -> s ^ i) [|"foo"; "bar"; "baz"|] = "foobarbaz"
    optNF (afoldl1 (+)) [||] = None
    afoldl1 (+) [|1|] = 1
  **)

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)
  (**T
    afoldr (+) 0 (1--|10) = 55
    afoldr (fun a s -> s ^ (string_of_int a)) "--|" (1--|3) = "--|321"
    afoldr (+) 1 [||] = 1
    afoldr (+) 1 [|1|] = 2
  **)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a
  (**T
    afoldr1 (+) (1--|10) = 55
    afoldr1 (fun a s -> s ^ a) [|"foo"; "bar"; "baz"|] = "bazbarfoo"
    optNF (afoldr1 (+)) [||] = None
    afoldr1 (+) [|1|] = 1
  **)


  let maximum a = foldl1 max a
  (**T
    amaximum [|1;2;3;0;1;4;3;1|] = 4
    amaximum [|1|] = 1
    optNF amaximum [||] = None
  **)
  let minimum a = foldl1 min a
  (**T
    aminimum [|1;2;3;0;1;4;3;1|] = 0
    aminimum [|1|] = 1
    optNF aminimum [||] = None
  **)

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  (**T
    amaximumBy square (-3 --| 2) = -3
    amaximumBy square (-1 --| 2) = 2
    optNF (amaximumBy square) [||] = None
  **)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)
  (**T
    aminimumBy square (-3 --| (-1)) = -1
    aminimumBy square (-1 --| 2) = 0
    optNF (aminimumBy square) [||] = None
  **)

  let maximumByWith f lst = maximumBy snd (map (fupler f) lst)
  (**T
    amaximumByWith square (-3 --| 2) = (-3, 9)
    amaximumByWith square (-1 --| 2) = (2, 4)
    optNF (amaximumByWith square) [||] = None
  **)
  let minimumByWith f lst = minimumBy snd (map (fupler f) lst)
  (**T
    aminimumByWith square (-3 --| (-1)) = (-1, 1)
    aminimumByWith square (-1 --| 2) = (0, 0)
    optNF (aminimumByWith square) [||] = None
  **)



  (* Subsequences *)

  let sub_start_and_length i len s =
    let slen = length s in
    let ni = normalizeIndex i s in
    let len = len + min 0 ni in
    let len = len + min 0 (slen-1-ni) in
    let i = max 0 (min (slen-1) ni) in
    let j = max 0 (min slen (i + len)) - 1 in
    (i, max 0 (j-i+1))

  let sub i len s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun x -> unsafe_get s (first+x)) sub_len
  (**T
    PreArray.sub 2 3 (aexplode "foobar") = [|'o'; 'b'; 'a'|]
    PreArray.sub (-3) 2 (aexplode "foobar") = [|'b'; 'a'|]
    PreArray.sub 0 0 (1--|10) = [||]
    PreArray.sub 0 0 [||] = [||]
    PreArray.sub 0 1 [||] = [||]
    PreArray.sub 1 0 [||] = [||]
    PreArray.sub 1 1 [||] = [||]
    PreArray.sub 0 0 [|1|] = [||]
    PreArray.sub 0 (-1) [|1|] = [||]
    PreArray.sub (-10) (-1) [|1|] = [||]
    PreArray.sub (-10) 1 [|1|] = [||]
    PreArray.sub 0 1 [|1|] = [|1|]
    PreArray.sub 1 1 [|1|] = [||]
    PreArray.sub 1 0 [|1|] = [||]
    PreArray.sub 80 1 [|1|] = [||]
    PreArray.sub 15 (-5) (1--|10) = [||]
    PreArray.sub 0 1 (1--|10) = [|1|]
    asub 0 (-1) (1--|10) = [||]
  **)
  let slice_to_sub i j s =
    let si = normalizeIndex i s
    and sj = normalizeIndex j s + 1 in
    let len = sj - si in
    i, len

  let slice i j s =
    let i, len = slice_to_sub i j s in
    sub i len s
  (**T
    PreArray.slice 2 3 (aexplode "foobar") = [|'o'; 'b'|]
    PreArray.slice (-3) (-1) (aexplode "foobar") = [|'b'; 'a'; 'r'|]
    PreArray.slice 0 0 (1--|10) = [|1|]
    PreArray.slice 0 1 (1--|10) = [|1; 2|]
    PreArray.slice 1 0 (1--|10) = [||]
    aslice 0 (-1) (1--|10) = (1--|10)
  **)
  let subStride stride i len a =
    if stride < 0 then invalid_arg "PreArray.subStride: negative stride";
    let len = max 0 len in
    let first, sub_len = sub_start_and_length i (len*stride) a in
    let stride_len = (sub_len+(stride-1)) / stride in
    init (fun j -> unsafe_get a (first + j*stride)) stride_len
  (**T
    asubStride 1 2 3 (aexplode "foobar") = aexplode "oba"
    asubStride 2 1 3 (aexplode "foobar") = aexplode "obr"
    asubStride 2 0 3 (aexplode "foobar") = aexplode "foa"
    asubStride 3 0 3 (aexplode "foobar") = aexplode "fb"
    asubStride 4 0 3 (aexplode "foobar") = aexplode "fa"
    asubStride 4 1 3 (aexplode "foobar") = aexplode "or"
    asubStride 4 1 1 (aexplode "foobar") = aexplode "o"
    asubStride 8 1 3 (aexplode "foobar") = aexplode "o"
    asubStride 1 0 (-1) (aexplode "foobar") = aexplode ""
    asubStride 1 (-1) 1 (aexplode "foobar") = aexplode "r"
    asubStride 1 0 1 (aexplode "f") = aexplode "f"
    asubStride 2 0 1 (aexplode "f") = aexplode "f"
    asubStride 4 0 1 (aexplode "f") = aexplode "f"
    asubStride 4 0 0 (aexplode "f") = aexplode ""
    asubStride 4 1 0 (aexplode "f") = aexplode ""
    asubStride 4 0 1 (aexplode "") = aexplode ""
    asubStride 4 1 1 (aexplode "") = aexplode ""
    optE (asubStride (-1) 0 10) [||] = None
  **)

  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  (**T
    afirst (2--|10) = 2
    optNF ahead [||] = None
    optNF ahead [|1|] = Some 1
    optNF ahead (1--|10) = Some 1
  **)
  let tail a = if len a = 0 then raise Not_found else slice 1 (-1) a
  (**T
    optNF atail [||] = None
    optNF atail [|1|] = Some [||]
    optNF atail (1--|10) = Some (2--|10)
  **)
  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  (**T
    alast [|1; 2; 3|] = 3
    alast [|1|] = 1
    optNF alast [||] = None
  **)
  let popped a = if len a = 0 then raise Not_found else slice 0 (-2) a
  (**T
    apopped [|1; 2; 3|] = [|1; 2|]
    optNF apopped [||] = None
    optNF apopped [|1|] = Some [||]
  **)
  let pop a = (popped a, last a)
  (**T
    apop [|1;2;3|] = ([|1;2|], 3)
    optNF apop [||] = None
    optNF apop [|1|] = Some ([||], 1)
    optNF apop [|1;2|] = Some ([|1|], 2)
  **)
  let push v a = append a [|v|]
  (**T
    apush 10 (1--|9) = (1--|10)
    apush 1 [|0|] = (0--|1)
    apush 0 [||] = [|0|]
  **)

  let shift a = (tail a, head a)
  (**T
    ashift (1--|10) = ((2--|10), 1)
    optNF ashift [||] = None
    optNF ashift [|1|] = Some ([||], 1)
  **)
  let unshift v a = append [|v|] a
  (**T
    aunshift 0 (1--|10) = (0--|10)
    aunshift 0 [|1|] = (0--|1)
    aunshift 0 [||] = [|0|]
  **)

  let take n s = sub 0 n s
  (**T
    PreArray.take 3 (1--|10) = (1--|3)
    PreArray.take 0 (1--|10) = [||]
    PreArray.take 15 (1--|10) = (1--|10)
    PreArray.take 3 [||] = [||]
    atake 0 [||] = [||]
  **)
  let takeWhile f s = take (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    PreArray.takeWhile (lt 5) (1--|10) = (1--|4)
    PreArray.takeWhile (lt 5) (6--|10) = [||]
    PreArray.takeWhile (lt 5) [||] = [||]
    atakeWhile (lt 5) (1--|3) = (1--|3)
  **)
  let takeUntil f s = take (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    PreArray.takeUntil (gte 5) (1--|10) = (1--|4)
    PreArray.takeUntil (gte 5) (6--|10) = [||]
    PreArray.takeUntil (gte 5) [||] = [||]
    atakeUntil (gte 5) (1--|3) = (1--|3)
  **)
  let drop n s = sub n (len s - n) s
  (**T
    PreArray.drop 3 (1--|10) = (4--|10)
    PreArray.drop 0 (1--|10) = (1--|10)
    PreArray.drop 15 (1--|10) = [||]
    PreArray.drop 3 [||] = [||]
    adrop 0 [||] = [||]
  **)
  let dropWhile f s = drop (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    PreArray.dropWhile (lt 5) (1--|10) = (5--|10)
    PreArray.dropWhile (lt 5) (6--|10) = (6--|10)
    PreArray.dropWhile (lt 5) [||] = [||]
    adropWhile (lt 5) (1--|3) = [||]
  **)
  let dropUntil f s = drop (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    PreArray.dropUntil (gte 5) (1--|10) = (5--|10)
    PreArray.dropUntil (gte 5) (6--|10) = (6--|10)
    PreArray.dropUntil (gte 5) [||] = [||]
    adropUntil (gte 5) (1--|3) = [||]
  **)
  let splitAt n xs = (take n xs, drop n xs)
  (**T
    PreArray.splitAt 3 (aexplode "foobar") = ([|'f'; 'o'; 'o'|], [|'b'; 'a'; 'r'|])
    PreArray.splitAt 1 [|1|] = ([|1|], [||])
    PreArray.splitAt 0 [|1|] = ([||], [|1|])
    PreArray.splitAt 1 [||] = ([||], [||])
    asplitAt 0 [||] = ([||], [||])
  **)
  let break f s = splitAt (maybeNF (len s) (findIndex f) s) s
  (**T
    PreArray.break id [|false; false; true; false|] = ([|false; false|], [|true; false|])
    abreak (greaterThan 5) (1--|10) = ([|1; 2; 3; 4; 5|], [|6; 7; 8; 9; 10|])
  **)
  let span f s = break (fun v -> not (f v)) s
  (**T
    PreArray.span id [|true; false; false; true|] = ([|true|], [|false; false; true|])
    PreArray.span (lessOrEqualTo 5) (1--|10) = ([|1; 2; 3; 4; 5|], [|6; 7; 8; 9; 10|])
    PreArray.span id [||] = ([||], [||])
    PreArray.span id [|true|] = ([|true|], [||])
    PreArray.span id [|false|] = ([||], [|false|])
    PreArray.span id [|false;true|] = ([||], [|false;true|])
    PreArray.span id [|true;false|] = ([|true|], [|false|])
    PreArray.span id [|true;true|] = ([|true;true|], [||])
    aspan id [|false;false|] = ([||], [|false;false|])
  **)

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (max 0 (2 * len s - 1))
  (**T
    PreArray.interlace 0 [|1; 2; 3|] = [|1; 0; 2; 0; 3|]
    aimplode @@ PreArray.interlace '-' @@ aexplode "abcde" = "a-b-c-d-e"
    PreArray.interlace 0 [||] = [||]
    PreArray.interlace 0 [|1|] = [|1|]
    ainterlace 0 [|1;2|] = [|1; 0; 2|]
  **)

  (* let filter = filter *)
  (**T
    PreArray.filter even (1--|10) = [|2;4;6;8;10|]
    PreArray.filter odd (1--|10) = [|1;3;5;7;9|]
    PreArray.filter even [|1|] = [||]
    PreArray.filter odd [|1|] = [|1|]
    afilter even [||] = [||]
  **)
  let reject f s = filter (fun v -> not (f v)) s
 (**T
    PreArray.reject (gt 4) (1--|5) = (1--|4)
    PreArray.reject (gt 4) [||] = [||]
    PreArray.reject (gt 0) (1--|5) = [||]
    PreArray.reject (gt 5) (1--|5) = (1--|5)
    areject (gt 3) (5--|1) = (3--|1)
  **)
  let without v s = filter ((<>) v) s
  (**T
    PreArray.without 4 [|1; 2; 4; 1; 2; 4|] = [|1; 2; 1; 2|]
    PreArray.without 4 [||] = [||]
    PreArray.without 4 [|4|] = [||]
    awithout 4 [|1|] = [|1|]
  **)

  let groupsOf n a =
    let l = len a in
    let n = max 1 n in
    if l = 0 then [a] else
      let count, rem = quot_rem l n in
      unfoldrWhile (gte 0) (fun i -> sub (i*n) n a, i-1) (count-1) @
      if rem = 0 then [] else [sub (-rem) rem a]
  (**T
    PreArray.groupsOf 3 (1--|10) = [[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]; [|10|]]
    PreArray.groupsOf 3 [||] = [[||]]
    PreArray.groupsOf 3 [|1|] = [[|1|]]
    PreArray.groupsOf 3 (1--|3) = [(1--|3)]
    PreArray.groupsOf 5 (1--|3) = [(1--|3)]
    PreArray.groupsOf 3 (1--|4) = [(1--|3); [|4|]]
    PreArray.groupsOf 1 (1--|3) = [[|1|]; [|2|]; [|3|]]
    PreArray.groupsOf 0 (1--|3) = [[|1|]; [|2|]; [|3|]]
    agroupsOf (-1) (1--|3) = [[|1|]; [|2|]; [|3|]]
  **)

  let splitInto n range =
    let len = len range in
    let plen = int (ceil (float len /. float (max 1 n))) in
    groupsOf plen range
  (**T
    PreArray.splitInto 4 (1--|10) = [[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]; [|10|]]
    PreArray.splitInto 3 (1--|3) = [[|1|]; [|2|]; [|3|]]
    PreArray.splitInto 1 (1--|3) = [(1--|3)]
    PreArray.splitInto 0 (1--|3) = [(1--|3)]
    PreArray.splitInto (-1) (1--|3) = [(1--|3)]
    PreArray.splitInto 2 [||] = [[||]]
    asplitInto 1 [||] = [[||]]
  **)


  (* Subsequence iterators *)

  let iterSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    for j=first to first+sub_len-1 do f (unsafe_get s j) done
  (**T
    mapWith (aiterSub 0 10) id (1--|10) = (1--10)
    mapWith (aiterSub 2 6) id (1--|10) = (3--8)
    mapWith (aiterSub (-2) 10) id (1--|10) = (9--10)
    mapWith (aiterSub (-18) 10) id (1--|10) = [1; 2]
    mapWith (aiterSub (-10) 10) id (1--|10) = (1--10)
    mapWith (aiterSub (-12) 1) id (1--|10) = []
    mapWith (aiterSub 9 10) id (1--|10) = [10]
    mapWith (aiterSub (-19) 10) id (1--|10) = [1]
    mapWith (aiterSub (-20) 10) id (1--|10) = []
    mapWith (aiterSub (-20) 20) id (1--|10) = (1--10)
    mapWith (aiterSub (-20) 50) id [||] = []
    mapWith (aiterSub (-20) 50) id [|1|] = [1]
  **)

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s
  (**T
    mapWith (aiterSlice 0 9) id (1--|10) = (1--10)
    mapWith (aiterSlice 2 6) id (1--|10) = (3--7)
    mapWith (aiterSlice 6 2) id (1--|10) = []
    mapWith (aiterSlice 0 0) id (1--|10) = [1]
    mapWith (aiterSlice 1 0) id (1--|10) = []
    mapWith (aiterSlice 9 9) id (1--|10) = [10]
    mapWith (aiterSlice 9 8) id (1--|10) = []
    mapWith (aiterSlice (-2) (-1)) id (1--|10) = [9;10]
    mapWith (aiterSlice (-12) 0) id (1--|10) = [1]
    mapWith (aiterSlice (-12) (-1)) id (1--|10) = (1--10)
    mapWith (aiterSlice (-12) (-11)) id (1--|10) = []
    mapWith (aiterSlice (-5) (-1)) id (1--|10) = (6--10)
    mapWith (aiterSlice (-20) 20) id (1--|10) = (1--10)
    mapWith (aiterSlice (-20) 50) id [||] = []
    mapWith (aiterSlice (-20) 50) id [|1|] = [1]
  **)

  let mapSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun j -> f (unsafe_get s (first+j))) sub_len
  (**T
    mapWith (amapSub 0 10) id (1--|10) = (1--10)
    amapSub 0 10 succ (1--|10) = (2--|11)
    amapSub 2 6 id (1--|10) = (3--|8)
    amapSub (-2) 10 id (1--|10) = (9--|10)
    amapSub (-18) 10 id (1--|10) = [|1; 2|]
    amapSub (-10) 10 id (1--|10) = (1--|10)
    amapSub (-12) 1 id (1--|10) = [||]
    amapSub 9 10 id (1--|10) = [|10|]
    amapSub (-19) 10 id (1--|10) = [|1|]
    amapSub (-20) 10 id (1--|10) = [||]
    amapSub (-20) 20 id (1--|10) = (1--|10)
    amapSub (-20) 50 id [||] = [||]
    amapSub (-20) 50 id [|1|] = [|1|]
  **)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s
  (**T
    mapWith (amapSlice 0 9) id (1--|10) = (1--10)
    amapSlice 0 9 id (1--|10) = (1--|10)
    amapSlice 2 6 id (1--|10) = (3--|7)
    amapSlice 6 2 id (1--|10) = [||]
    amapSlice 0 0 id (1--|10) = [|1|]
    amapSlice 1 0 id (1--|10) = [||]
    amapSlice 9 9 id (1--|10) = [|10|]
    amapSlice 9 8 id (1--|10) = [||]
    amapSlice (-2) (-1) id (1--|10) = [|9;10|]
    amapSlice (-12) 0 id (1--|10) = [|1|]
    amapSlice (-12) (-1) id (1--|10) = (1--|10)
    amapSlice (-12) (-11) id (1--|10) = [||]
    amapSlice (-5) (-1) id (1--|10) = (6--|10)
    amapSlice (-20) 20 id (1--|10) = (1--|10)
    amapSlice (-20) 50 id [||] = [||]
    amapSlice (-20) 50 id [|1|] = [|1|]
  **)

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    afoldlSub 0 10 (+) 0 (1--|10) = asum (1--|10)
    afoldlSub (-10) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldlSub (-20) 20 (+) 0 (1--|10) = asum (1--|10)
    afoldlSub 0 3 (+) 0 (1--|10) = asum (1--|3)
    afoldlSub 3 3 (+) 0 (1--|10) = asum (4--|6)
    afoldlSub (-3) 3 (+) 0 (1--|10) = asum (8--|10)
    afoldlSub (-1) 3 (+) 0 (1--|10) = asum (10--|10)
    afoldlSub (-3) 1 (+) 0 (1--|10) = asum (8--|8)
    afoldlSub 20 (-20) (+) 0 (1--|10) = asum [||]
    afoldlSub (-20) 10 (+) 0 (1--|10) = asum [||]
    afoldlSub 10 0 (+) 0 (1--|10) = asum [||]
    afoldlSub 3 (-1) (+) 0 (1--|10) = asum [||]

    afoldlSub 0 1 (+) 0 (1--|1) = asum (1--|1)
    afoldlSub 0 1 (+) 0 [||] = asum [||]
  **)

  let foldl1Sub i len f s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    afoldl1Sub 0 10 (+) (1--|10) = asum (1--|10)
    afoldl1Sub (-10) 10 (+) (1--|10) = asum (1--|10)
    afoldl1Sub (-20) 20 (+) (1--|10) = asum (1--|10)
    afoldl1Sub 0 3 (+) (1--|10) = asum (1--|3)
    afoldl1Sub 3 3 (+) (1--|10) = asum (4--|6)
    afoldl1Sub (-3) 3 (+) (1--|10) = asum (8--|10)
    afoldl1Sub (-1) 3 (+) (1--|10) = asum (10--|10)
    afoldl1Sub (-3) 1 (+) (1--|10) = asum (8--|8)
    optNF (afoldl1Sub 20 (-20) (+)) (1--|10) = None
    optNF (afoldl1Sub (-20) 10 (+)) (1--|10) = None
    optNF (afoldl1Sub 10 0 (+)) (1--|10) = None
    optNF (afoldl1Sub 3 (-1) (+)) (1--|10) = None

    afoldl1Sub 0 1 (+) (1--|1) = asum (1--|1)
    optNF (afoldl1Sub 0 1 (+)) [||] = None
  **)

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    afoldrSub 0 10 (+) 0 (1--|10) = asum (1--|10)
    afoldrSub (-10) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldrSub (-20) 20 (+) 0 (1--|10) = asum (1--|10)
    afoldrSub 0 3 (+) 0 (1--|10) = asum (1--|3)
    afoldrSub 3 3 (+) 0 (1--|10) = asum (4--|6)
    afoldrSub (-3) 3 (+) 0 (1--|10) = asum (8--|10)
    afoldrSub (-1) 3 (+) 0 (1--|10) = asum (10--|10)
    afoldrSub (-3) 1 (+) 0 (1--|10) = asum (8--|8)
    afoldrSub 20 (-20) (+) 0 (1--|10) = asum [||]
    afoldrSub (-20) 10 (+) 0 (1--|10) = asum [||]
    afoldrSub 10 0 (+) 0 (1--|10) = asum [||]
    afoldrSub 3 (-1) (+) 0 (1--|10) = asum [||]

    afoldrSub 0 1 (+) 0 (1--|1) = asum (1--|1)
    afoldrSub 0 1 (+) 0 [||] = asum [||]
  **)

  let foldr1Sub i len f s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    afoldr1Sub 0 10 (+) (1--|10) = asum (1--|10)
    afoldr1Sub (-10) 10 (+) (1--|10) = asum (1--|10)
    afoldr1Sub (-20) 20 (+) (1--|10) = asum (1--|10)
    afoldr1Sub 0 3 (+) (1--|10) = asum (1--|3)
    afoldr1Sub 3 3 (+) (1--|10) = asum (4--|6)
    afoldr1Sub (-3) 3 (+) (1--|10) = asum (8--|10)
    afoldr1Sub (-1) 3 (+) (1--|10) = asum (10--|10)
    afoldr1Sub (-3) 1 (+) (1--|10) = asum (8--|8)
    optNF (afoldr1Sub 20 (-20) (+)) (1--|10) = None
    optNF (afoldr1Sub (-20) 10 (+)) (1--|10) = None
    optNF (afoldr1Sub 10 0 (+)) (1--|10) = None
    optNF (afoldr1Sub 3 (-1) (+)) (1--|10) = None

    afoldr1Sub 0 1 (+) (1--|1) = asum (1--|1)
    optNF (afoldr1Sub 0 1 (+)) [||] = None
  **)

  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s
  (**T
    afoldlSlice 0 10 (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice 0 9 (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice 0 (-1) (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice (-10) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice (-20) 20 (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice (-20) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldlSlice 0 3 (+) 0 (1--|10) = asum (1--|4)
    afoldlSlice 3 (-1) (+) 0 (1--|10) = asum (4--|10)
    afoldlSlice 3 3 (+) 0 (1--|10) = asum (4--|4)
    afoldlSlice (-1) (-1) (+) 0 (1--|10) = asum (10--|10)
    afoldlSlice (-3) 3 (+) 0 (1--|10) = asum [||]
    afoldlSlice (-3) 1 (+) 0 (1--|10) = asum [||]
    afoldlSlice 20 (-20) (+) 0 (1--|10) = asum [||]
    afoldlSlice 10 0 (+) 0 (1--|10) = asum [||]

    afoldlSlice 0 1 (+) 0 (1--|1) = asum (1--|1)
    afoldlSlice 0 1 (+) 0 [||] = asum [||]
  **)

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s
  (**T
    afoldl1Slice 0 10 (+) (1--|10) = asum (1--|10)
    afoldl1Slice 0 9 (+) (1--|10) = asum (1--|10)
    afoldl1Slice 0 (-1) (+) (1--|10) = asum (1--|10)
    afoldl1Slice (-10) 10 (+) (1--|10) = asum (1--|10)
    afoldl1Slice (-20) 20 (+) (1--|10) = asum (1--|10)
    afoldl1Slice (-20) 10 (+) (1--|10) = asum (1--|10)
    afoldl1Slice 0 3 (+) (1--|10) = asum (1--|4)
    afoldl1Slice 3 (-1) (+) (1--|10) = asum (4--|10)
    afoldl1Slice 3 3 (+) (1--|10) = asum (4--|4)
    afoldl1Slice (-1) (-1) (+) (1--|10) = asum (10--|10)
    optNF (afoldl1Slice (-3) 3 (+)) (1--|10) = None
    optNF (afoldl1Slice (-3) 1 (+)) (1--|10) = None
    optNF (afoldl1Slice 20 (-20) (+)) (1--|10) = None
    optNF (afoldl1Slice 10 0 (+)) (1--|10) = None

    afoldl1Slice 0 1 (+) (1--|1) = asum (1--|1)
    optNF (afoldl1Slice 0 1 (+)) [||] = None
  **)

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s
  (**T
    afoldrSlice 0 10 (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice 0 9 (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice 0 (-1) (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice (-10) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice (-20) 20 (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice (-20) 10 (+) 0 (1--|10) = asum (1--|10)
    afoldrSlice 0 3 (+) 0 (1--|10) = asum (1--|4)
    afoldrSlice 3 (-1) (+) 0 (1--|10) = asum (4--|10)
    afoldrSlice 3 3 (+) 0 (1--|10) = asum (4--|4)
    afoldrSlice (-1) (-1) (+) 0 (1--|10) = asum (10--|10)
    afoldrSlice (-3) 3 (+) 0 (1--|10) = asum [||]
    afoldrSlice (-3) 1 (+) 0 (1--|10) = asum [||]
    afoldrSlice 20 (-20) (+) 0 (1--|10) = asum [||]
    afoldrSlice 10 0 (+) 0 (1--|10) = asum [||]

    afoldrSlice 0 1 (+) 0 (1--|1) = asum (1--|1)
    afoldrSlice 0 1 (+) 0 [||] = asum [||]
  **)

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s
  (**T
    afoldr1Slice 0 10 (+) (1--|10) = asum (1--|10)
    afoldr1Slice 0 9 (+) (1--|10) = asum (1--|10)
    afoldr1Slice 0 (-1) (+) (1--|10) = asum (1--|10)
    afoldr1Slice (-10) 10 (+) (1--|10) = asum (1--|10)
    afoldr1Slice (-20) 20 (+) (1--|10) = asum (1--|10)
    afoldr1Slice (-20) 10 (+) (1--|10) = asum (1--|10)
    afoldr1Slice 0 3 (+) (1--|10) = asum (1--|4)
    afoldr1Slice 3 (-1) (+) (1--|10) = asum (4--|10)
    afoldr1Slice 3 3 (+) (1--|10) = asum (4--|4)
    afoldr1Slice (-1) (-1) (+) (1--|10) = asum (10--|10)
    optNF (afoldr1Slice (-3) 3 (+)) (1--|10) = None
    optNF (afoldr1Slice (-3) 1 (+)) (1--|10) = None
    optNF (afoldr1Slice 20 (-20) (+)) (1--|10) = None
    optNF (afoldr1Slice 10 0 (+)) (1--|10) = None

    afoldr1Slice 0 1 (+) (1--|1) = asum (1--|1)
    optNF (afoldr1Slice 0 1 (+)) [||] = None
  **)

  let sum a = foldl (+) 0 a
  (**T
    asum (1--|10) = 55
    asum [|1;2|] = 3
    asum [|1|] = 1
    asum [|0|] = 0
    asum [||] = 0
  **)
  let sumf a = foldl (+.) 0. a
  (**T
    asumf (1.--.|10.) = 55.
    asumf [|1.;2.|] = 3.
    asumf [|1.|] = 1.
    asumf [|0.|] = 0.
    asumf [||] = 0.
  **)
  let product a = foldl ( * ) 1 a
  (**T
    aproduct (1--|10) = 3628800
    aproduct [|1;2|] = 2
    aproduct [|1|] = 1
    aproduct [|0|] = 0
    aproduct [||] = 1
  **)
  let productf a = foldl ( *. ) 1. a
  (**T
    aproductf (1.--.|10.) = 3628800.
    aproductf [|1.;2.|] = 2.
    aproductf [|1.|] = 1.
    aproductf [|0.|] = 0.
    aproductf [||] = 1.
  **)
  let average a = sum a / len a
  (**T
    aaverage (1--|10) = 5
    aaverage [|1|] = 1
    optEx Division_by_zero aaverage [||] = None
  **)
  let averagef a = sumf a /. float (len a)
  (**T
    aaveragef (1.--.|10.) = 5.5
    aaveragef [|1.|] = 1.
    isNaN (aaveragef [||])
  **)

  let sumSub i len a = foldlSub i len (+) 0 a
  (**T
    asumSub 0 10 (1--|10) = asum (1--|10)
    asumSub (-10) 10 (1--|10) = asum (1--|10)
    asumSub (-20) 20 (1--|10) = asum (1--|10)
    asumSub 0 3 (1--|10) = asum (1--|3)
    asumSub 3 3 (1--|10) = asum (4--|6)
    asumSub (-3) 3 (1--|10) = asum (8--|10)
    asumSub (-1) 3 (1--|10) = asum (10--|10)
    asumSub (-3) 1 (1--|10) = asum (8--|8)
    asumSub 20 (-20) (1--|10) = asum [||]
    asumSub (-20) 10 (1--|10) = asum [||]
    asumSub 10 0 (1--|10) = asum [||]
    asumSub 3 (-1) (1--|10) = asum [||]

    asumSub 0 1 (1--|1) = asum (1--|1)
    asumSub 0 1 [||] = asum [||]
  **)
  let sumSubf i len a = foldlSub i len (+.) 0. a
  (**T
    asumSubf 0 10 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSubf (-10) 10 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSubf (-20) 20 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSubf 0 3 (1.0--.|10.0) = asumf (1.--.|3.)
    asumSubf 3 3 (1.0--.|10.0) = asumf (4.--.|6.)
    asumSubf (-3) 3 (1.0--.|10.0) = asumf (8.--.|10.)
    asumSubf (-1) 3 (1.0--.|10.0) = asumf (10.--.|10.)
    asumSubf (-3) 1 (1.0--.|10.0) = asumf (8.--.|8.)
    asumSubf 20 (-20) (1.0--.|10.0) = asumf [||]
    asumSubf (-20) 10 (1.0--.|10.0) = asumf [||]
    asumSubf 10 0 (1.0--.|10.0) = asumf [||]
    asumSubf 3 (-1) (1.0--.|10.0) = asumf [||]

    asumSubf 0 1 (1.--.|1.) = asumf (1.--.|1.)
    asumSubf 0 1 [||] = asumf [||]
  **)

  let sumSlice i j a = foldlSlice i j (+) 0 a
  (**T
    asumSlice 0 10 (1--|10) = asum (1--|10)
    asumSlice 0 9 (1--|10) = asum (1--|10)
    asumSlice 0 (-1) (1--|10) = asum (1--|10)
    asumSlice (-10) 10 (1--|10) = asum (1--|10)
    asumSlice (-20) 20 (1--|10) = asum (1--|10)
    asumSlice (-20) 10 (1--|10) = asum (1--|10)
    asumSlice 0 3 (1--|10) = asum (1--|4)
    asumSlice 3 (-1) (1--|10) = asum (4--|10)
    asumSlice 3 3 (1--|10) = asum (4--|4)
    asumSlice (-1) (-1) (1--|10) = asum (10--|10)
    asumSlice (-3) 3 (1--|10) = asum [||]
    asumSlice (-3) 1 (1--|10) = asum [||]
    asumSlice 20 (-20) (1--|10) = asum [||]
    asumSlice 10 0 (1--|10) = asum [||]

    asumSlice 0 1 (1--|1) = asum (1--|1)
    asumSlice 0 1 [||] = asum [||]
  **)
  let sumSlicef i j a = foldlSlice i j (+.) 0. a
  (**T
    asumSlicef 0 10 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef 0 9 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef 0 (-1) (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef (-10) 10 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef (-20) 20 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef (-20) 10 (1.0--.|10.0) = asumf (1.0--.|10.0)
    asumSlicef 0 3 (1.0--.|10.0) = asumf (1.0--.|4.0)
    asumSlicef 3 (-1) (1.0--.|10.0) = asumf (4.0--.|10.0)
    asumSlicef 3 3 (1.0--.|10.0) = asumf (4.0--.|4.0)
    asumSlicef (-1) (-1) (1.0--.|10.0) = asumf (10.0--.|10.0)
    asumSlicef (-3) 3 (1.0--.|10.0) = asumf [||]
    asumSlicef (-3) 1 (1.0--.|10.0) = asumf [||]
    asumSlicef 20 (-20) (1.0--.|10.0) = asumf [||]
    asumSlicef 10 0 (1.0--.|10.0) = asumf [||]

    asumSlicef 0 1 (1.0--.|1.0) = asumf (1.0--.|1.0)
    asumSlicef 0 1 [||] = asumf [||]
  **)

  let productSub i len a = foldlSub i len ( * ) 1 a
  (**T
    aproductSub 0 10 (1--|10) = aproduct (1--|10)
    aproductSub (-10) 10 (1--|10) = aproduct (1--|10)
    aproductSub (-20) 20 (1--|10) = aproduct (1--|10)
    aproductSub 0 3 (1--|10) = aproduct (1--|3)
    aproductSub 3 3 (1--|10) = aproduct (4--|6)
    aproductSub (-3) 3 (1--|10) = aproduct (8--|10)
    aproductSub (-1) 3 (1--|10) = aproduct (10--|10)
    aproductSub (-3) 1 (1--|10) = aproduct (8--|8)
    aproductSub 20 (-20) (1--|10) = aproduct [||]
    aproductSub (-20) 10 (1--|10) = aproduct [||]
    aproductSub 10 0 (1--|10) = aproduct [||]
    aproductSub 3 (-1) (1--|10) = aproduct [||]

    aproductSub 0 1 (1--|1) = aproduct (1--|1)
    aproductSub 0 1 [||] = aproduct [||]
  **)
  let productSubf i len a = foldlSub i len ( *. ) 1. a
  (**T
    aproductSubf 0 10 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSubf (-10) 10 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSubf (-20) 20 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSubf 0 3 (1.0--.|10.0) = aproductf (1.0--.|3.0)
    aproductSubf 3 3 (1.0--.|10.0) = aproductf (4.0--.|6.0)
    aproductSubf (-3) 3 (1.0--.|10.0) = aproductf (8.0--.|10.0)
    aproductSubf (-1) 3 (1.0--.|10.0) = aproductf (10.0--.|10.0)
    aproductSubf (-3) 1 (1.0--.|10.0) = aproductf (8.0--.|8.0)
    aproductSubf 20 (-20) (1.0--.|10.0) = aproductf [||]
    aproductSubf (-20) 10 (1.0--.|10.0) = aproductf [||]
    aproductSubf 10 0 (1.0--.|10.0) = aproductf [||]
    aproductSubf 3 (-1) (1.0--.|10.0) = aproductf [||]

    aproductSubf 0 1 (1.0--.|1.0) = aproductf (1.0--.|1.0)
    aproductSubf 0 1 [||] = aproductf [||]
  **)

  let productSlice i j a = foldlSlice i j ( * ) 1 a
  (**T
    aproductSlice 0 10 (1--|10) = aproduct (1--|10)
    aproductSlice 0 9 (1--|10) = aproduct (1--|10)
    aproductSlice 0 (-1) (1--|10) = aproduct (1--|10)
    aproductSlice (-10) 10 (1--|10) = aproduct (1--|10)
    aproductSlice (-20) 20 (1--|10) = aproduct (1--|10)
    aproductSlice (-20) 10 (1--|10) = aproduct (1--|10)
    aproductSlice 0 3 (1--|10) = aproduct (1--|4)
    aproductSlice 3 (-1) (1--|10) = aproduct (4--|10)
    aproductSlice 3 3 (1--|10) = aproduct (4--|4)
    aproductSlice (-1) (-1) (1--|10) = aproduct (10--|10)
    aproductSlice (-3) 3 (1--|10) = aproduct [||]
    aproductSlice (-3) 1 (1--|10) = aproduct [||]
    aproductSlice 20 (-20) (1--|10) = aproduct [||]
    aproductSlice 10 0 (1--|10) = aproduct [||]

    aproductSlice 0 1 (1--|1) = aproduct (1--|1)
    aproductSlice 0 1 [||] = aproduct [||]
  **)
  let productSlicef i j a = foldlSlice i j ( *. ) 1. a
  (**T
    aproductSlicef 0 10 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef 0 9 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef 0 (-1) (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef (-10) 10 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef (-20) 20 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef (-20) 10 (1.0--.|10.0) = aproductf (1.0--.|10.0)
    aproductSlicef 0 3 (1.0--.|10.0) = aproductf (1.0--.|4.0)
    aproductSlicef 3 (-1) (1.0--.|10.0) = aproductf (4.0--.|10.0)
    aproductSlicef 3 3 (1.0--.|10.0) = aproductf (4.0--.|4.0)
    aproductSlicef (-1) (-1) (1.0--.|10.0) = aproductf (10.0--.|10.0)
    aproductSlicef (-3) 3 (1.0--.|10.0) = aproductf [||]
    aproductSlicef (-3) 1 (1.0--.|10.0) = aproductf [||]
    aproductSlicef 20 (-20) (1.0--.|10.0) = aproductf [||]
    aproductSlicef 10 0 (1.0--.|10.0) = aproductf [||]

    aproductSlicef 0 1 (1.0--.|1.0) = aproductf (1.0--.|1.0)
    aproductSlicef 0 1 [||] = aproductf [||]
  **)

  let averageSub i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSub i len a / sub_len
  (**T
    aaverageSub 0 10 (1--|10) = aaverage (1--|10)
    aaverageSub (-10) 10 (1--|10) = aaverage (1--|10)
    aaverageSub (-20) 20 (1--|10) = aaverage (1--|10)
    aaverageSub 0 3 (1--|10) = aaverage (1--|3)
    aaverageSub 3 3 (1--|10) = aaverage (4--|6)
    aaverageSub (-3) 3 (1--|10) = aaverage (8--|10)
    aaverageSub (-1) 3 (1--|10) = aaverage (10--|10)
    aaverageSub (-3) 1 (1--|10) = aaverage (8--|8)
    optEx Division_by_zero (aaverageSub 20 (-20)) (1--|10) = None
    optEx Division_by_zero (aaverageSub (-20) 10) (1--|10) = None
    optEx Division_by_zero (aaverageSub 10 0) (1--|10) = None
    optEx Division_by_zero (aaverageSub 3 (-1)) (1--|10) = None

    aaverageSub 0 1 (1--|1) = aaverage (1--|1)
    optEx Division_by_zero (aaverageSub 0 1) [||] = None
  **)

  (* FIXME *)
  (**T aaverageSub_overflows
    aaverageSub 0 10 [|max_int/3; max_int/3; max_int/3; max_int/3|] <> max_int/3
    aaverageSub 0 10 [|max_int/2; max_int/2; max_int/2|] <> max_int/2
    aaverageSub 0 10 [|max_int; max_int; max_int|] <> max_int
    aaverageSub 0 10 [|min_int; min_int; min_int|] <> min_int
    aaverageSub 0 10 [|max_int; max_int|] <> max_int
    aaverageSub 0 10 [|min_int; min_int|] <> min_int
    aaverageSub 0 10 [|max_int; min_int|] = 0
  **)
  let averageSubf i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSubf i len a /. float sub_len
  (**T
    aaverageSubf 0 10 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSubf (-10) 10 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSubf (-20) 20 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSubf 0 3 (1.0--.|10.0) = aaveragef (1.0--.|3.0)
    aaverageSubf 3 3 (1.0--.|10.0) = aaveragef (4.0--.|6.0)
    aaverageSubf (-3) 3 (1.0--.|10.0) = aaveragef (8.0--.|10.0)
    aaverageSubf (-1) 3 (1.0--.|10.0) = aaveragef (10.0--.|10.0)
    aaverageSubf (-3) 1 (1.0--.|10.0) = aaveragef (8.0--.|8.0)
    isNaN (aaverageSubf 20 (-20) (1.0--.|10.0))
    isNaN  (aaverageSubf (-20) 10 (1.0--.|10.0))
    isNaN  (aaverageSubf 10 0 (1.0--.|10.0))
    isNaN  (aaverageSubf 3 (-1) (1.0--.|10.0))

    aaverageSubf 0 1 (1.0--.|1.0) = aaveragef (1.0--.|1.0)
    isNaN  (aaverageSubf 0 1 [||])
  **)

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s
  (**T
    aaverageSlice 0 10 (1--|10) = aaverage (1--|10)
    aaverageSlice 0 9 (1--|10) = aaverage (1--|10)
    aaverageSlice 0 (-1) (1--|10) = aaverage (1--|10)
    aaverageSlice (-10) 10 (1--|10) = aaverage (1--|10)
    aaverageSlice (-20) 20 (1--|10) = aaverage (1--|10)
    aaverageSlice (-20) 10 (1--|10) = aaverage (1--|10)
    aaverageSlice 0 3 (1--|10) = aaverage (1--|4)
    aaverageSlice 3 (-1) (1--|10) = aaverage (4--|10)
    aaverageSlice 3 3 (1--|10) = aaverage (4--|4)
    aaverageSlice (-1) (-1) (1--|10) = aaverage (10--|10)
    optEx Division_by_zero (aaverageSlice (-3) 3) (1--|10) = None
    optEx Division_by_zero (aaverageSlice (-3) 1) (1--|10) = None
    optEx Division_by_zero (aaverageSlice 20 (-20)) (1--|10) = None
    optEx Division_by_zero (aaverageSlice 10 0) (1--|10) = None

    aaverageSlice 0 1 (1--|1) = aaverage (1--|1)
    optEx Division_by_zero (aaverageSlice 0 1) [||] = None
  **)

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s
  (**T
    aaverageSlicef 0 10 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef 0 9 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef 0 (-1) (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef (-10) 10 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef (-20) 20 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef (-20) 10 (1.0--.|10.0) = aaveragef (1.0--.|10.0)
    aaverageSlicef 0 3 (1.0--.|10.0) = aaveragef (1.0--.|4.0)
    aaverageSlicef 3 (-1) (1.0--.|10.0) = aaveragef (4.0--.|10.0)
    aaverageSlicef 3 3 (1.0--.|10.0) = aaveragef (4.0--.|4.0)
    aaverageSlicef (-1) (-1) (1.0--.|10.0) = aaveragef (10.0--.|10.0)
    isNaN @@ aaverageSlicef (-3) 3 (1.0--.|10.0)
    isNaN @@ aaverageSlicef (-3) 1 (1.0--.|10.0)
    isNaN @@ aaverageSlicef 20 (-20) (1.0--.|10.0)
    isNaN @@ aaverageSlicef 10 0 (1.0--.|10.0)

    aaverageSlicef 0 1 (1.0--.|1.0) = aaveragef (1.0--.|1.0)
    isNaN @@ aaverageSlicef 0 1 [||]
  **)

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (gte l) indices then raise Not_found;
    PreList.map (fun i -> unsafe_get s i) indices
  (**T
    apick [2; 3] (aexplode "foobar") = ['o'; 'b']
    apick [] [||] = []
    apick [] (1--|10) = []
    apick [0; 9] (1--|10) = [1; 10]
    optNF (apick [2;3]) [|1;2;3|] = None
    optNF (apick [2;3]) [||] = None
  **)

  let pickWith funcs s = PreList.map (fun f -> f s) funcs
  (**T
    apickWith [afirst; alast] (aexplode "foobar") = ['f'; 'r']
  **)


  (* Parallel operations *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l |> par_map ~process_count process |> combine
  (**T
    PreArray.par_mapReduce ~combine:aconcat ~process:(amap succ) (1--|10) = amap succ (1--|10)
    PreArray.par_mapReduce ~process_count:2 ~combine:aconcat ~process:(amap succ) (1--|10) = amap succ (1--|10)
    PreArray.par_mapReduce ~process_count:2 ~combine:(aconcat @. reverse) ~process:(amap succ) (1--|10) = amap succ ((6--|10)@|(1--|5))
    PreArray.par_mapReduce ~process_count:2 ~combine:(aconcat @. reverse) ~process:(amap succ) [||] = [||]
    PreArray.par_mapReduce ~process_count:2 ~combine:(aconcat @. reverse) ~process:(amap succ) [|1|] = [|2|]
  **)

  let pmapReduce combine process = par_mapReduce ~combine ~process
  (**T
    PreArray.pmapReduce aconcat (amap succ) (1--|10) = amap succ (1--|10)
    PreArray.pmapReduce ~process_count:2 aconcat (amap succ) (1--|10) = amap succ (1--|10)
    PreArray.pmapReduce ~process_count:2 (aconcat @. reverse) (amap succ) (1--|10) = amap succ ((6--|10)@|(1--|5))
    PreArray.pmapReduce ~process_count:2 (aconcat @. reverse) (amap succ) [||] = [||]
    PreArray.pmapReduce ~process_count:2 (aconcat @. reverse) (amap succ) [|1|] = [|2|]
  **)

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  (**T
    apfoldl (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldl ~process_count:2 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldl ~process_count:2 (+) (+) 0 [|1|] = asum [|1|]
    apfoldl ~process_count:1 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldl ~process_count:1 (+) (+) 0 [|1|] = asum [|1|]
    apfoldl ~process_count:0 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldl ~process_count:0 (+) (+) 0 [|1|] = asum [|1|]
    apfoldl ~process_count:3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldl ~process_count:3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldl ~process_count:2 (multiply) (multiply) 1 (1--|10) = aproduct (1--|10)
    apfoldl ~process_count:2 (multiply) (multiply) 1 [|1|] = aproduct [|1|]
    optNF (apfoldl ~process_count:2 (+) (+) 0) [||] = Some 0
  **)
  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  (**T
    apfoldl1 (+) (1--|10) = asum (1--|10)
    apfoldl1 ~process_count:3 (+) (1--|10) = asum (1--|10)
    apfoldl1 ~process_count:2 (+) [|1|] = asum [|1|]
    apfoldl1 ~process_count:1 (multiply) (1--|10) = aproduct (1--|10)
    apfoldl1 ~process_count:0 (multiply) [|1|] = aproduct [|1|]
    optNF (apfoldl1 ~process_count:2 (+)) [||] = None
  **)

  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  (**T
    apfoldr ~process_count:2 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldr ~process_count:2 (+) (+) 0 [|1|] = asum [|1|]
    apfoldr (+) (+) 0 [|1|] = asum [|1|]
    apfoldr ~process_count:1 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldr ~process_count:1 (+) (+) 0 [|1|] = asum [|1|]
    apfoldr ~process_count:0 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldr ~process_count:0 (+) (+) 0 [|1|] = asum [|1|]
    apfoldr ~process_count:3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldr ~process_count:3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldr ~process_count:2 (multiply) (multiply) 1 (1--|10) = aproduct (1--|10)
    apfoldr ~process_count:2 (multiply) (multiply) 1 [|1|] = aproduct [|1|]
    optNF (apfoldr ~process_count:2 (+) (+) 0) [||] = Some 0
  **)

  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)
  (**T
    apfoldr1 ~process_count:3 (+) (1--|10) = asum (1--|10)
    apfoldr1 (+) (1--|10) = asum (1--|10)
    apfoldr1 ~process_count:2 (+) [|1|] = asum [|1|]
    apfoldr1 ~process_count:1 (multiply) (1--|10) = aproduct (1--|10)
    apfoldr1 ~process_count:0 (multiply) [|1|] = aproduct [|1|]
    optNF (apfoldr1 ~process_count:2 (+)) [||] = None
  **)

  let piter f = pmapReduce ignore (iter f)
  (**T
    apiter ~process_count:3 (ignore @. succ) (1--|10) = ()
    apiter ~process_count:2 (ignore @. succ) (1--|10) = ()
    apiter ~process_count:1 (ignore @. succ) (1--|10) = ()
    apiter ~process_count:0 (ignore @. succ) (1--|10) = ()
    apiter ~process_count:3 (ignore @. succ) [|1|] = ()
    apiter ~process_count:2 (ignore @. succ) [|1|] = ()
    apiter ~process_count:1 (ignore @. succ) [|1|] = ()
    apiter ~process_count:0 (ignore @. succ) [|1|] = ()
    apiter ~process_count:3 (ignore @. succ) [||] = ()
    apiter ~process_count:2 (ignore @. succ) [||] = ()
    apiter ~process_count:1 (ignore @. succ) [||] = ()
    apiter ~process_count:0 (ignore @. succ) [||] = ()
    apiter (ignore @. succ) [||] = ()
    apiter (ignore @. succ) (1--|10) = ()
    apiter (ignore @. succ) [|1|] = ()
  **)

  let pmap f = pmapReduce concat (map f)
  (**T
    apmap ~process_count:3 succ (1--|10) = amap succ (1--|10)
    apmap ~process_count:2 succ (1--|10) = amap succ (1--|10)
    apmap ~process_count:1 succ (1--|10) = amap succ (1--|10)
    apmap ~process_count:0 succ (1--|10) = amap succ (1--|10)
    apmap ~process_count:3 succ [|1|] = amap succ [|1|]
    apmap ~process_count:2 succ [|1|] = amap succ [|1|]
    apmap ~process_count:1 succ [|1|] = amap succ [|1|]
    apmap ~process_count:0 succ [|1|] = amap succ [|1|]
    apmap ~process_count:3 succ [||] = amap succ [||]
    apmap ~process_count:2 succ [||] = amap succ [||]
    apmap ~process_count:1 succ [||] = amap succ [||]
    apmap ~process_count:0 succ [||] = amap succ [||]
    apmap succ (1--|10) = amap succ (1--|10)
    apmap succ [||] = amap succ [||]
    apmap succ [|1|] = amap succ [|1|]
  **)

  let pfilter f = pmapReduce concat (filter f)
  (**T
    apfilter even (1--|10) = [|2;4;6;8;10|]
    apfilter odd (1--|10) = [|1;3;5;7;9|]
    apfilter even [|1|] = [||]
    apfilter odd [|1|] = [|1|]
    apfilter even [||] = [||]
  **)


  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)
  (**T
    apfoldlSeqN 3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldlSeqN ~process_count:2 3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldlSeqN ~process_count:2 3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldlSeqN ~process_count:1 3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldlSeqN ~process_count:1 3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldlSeqN ~process_count:0 3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldlSeqN ~process_count:0 3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldlSeqN ~process_count:3 3 (+) (+) 0 (1--|10) = asum (1--|10)
    apfoldlSeqN ~process_count:3 3 (+) (+) 0 [|1|] = asum [|1|]
    apfoldlSeqN ~process_count:2 3 (multiply) (multiply) 1 (1--|10) = aproduct (1--|10)
    apfoldlSeqN ~process_count:2 3 (multiply) (multiply) 1 [|1|] = aproduct [|1|]
    optNF (apfoldlSeqN ~process_count:2 3 (+) (+) 0) [||] = Some 0
  **)


  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)
  (**T
    apiterSeqN ~process_count:3 1 ignore succ (1--|10) = ()
    apiterSeqN ~process_count:2 2 ignore succ (1--|10) = ()
    apiterSeqN ~process_count:1 1 ignore succ (1--|10) = ()
    apiterSeqN ~process_count:0 4 ignore succ (1--|10) = ()
    apiterSeqN ~process_count:3 1 ignore succ [|1|] = ()
    apiterSeqN ~process_count:2 6 ignore succ [|1|] = ()
    apiterSeqN ~process_count:1 1 ignore succ [|1|] = ()
    apiterSeqN ~process_count:0 1 ignore succ [|1|] = ()
    apiterSeqN ~process_count:3 2 ignore succ [||] = ()
    apiterSeqN ~process_count:2 1 ignore succ [||] = ()
    apiterSeqN ~process_count:1 3 ignore succ [||] = ()
    apiterSeqN ~process_count:0 1 ignore succ [||] = ()
    apiterSeqN 0 ignore succ [||] = ()
    apiterSeqN 1 ignore succ (1--|10) = ()
    apiterSeqN 1 ignore succ [|1|] = ()
  **)


  let pinit ?process_count f l =
    let process_count = max 1 (process_count |? !global_process_count) in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))
  (**T
    apinit succ 10 = (1--|10)
    apinit pred 10 = ((-1)--|8)
    apinit succ 0 = [||]
    apinit succ 1 = [|1|]
    apinit ~process_count:4 succ 10 = (1--|10)
    apinit ~process_count:3 pred 10 = ((-1)--|8)
    apinit ~process_count:2 succ 0 = [||]
    apinit ~process_count:1 pred 10 = ((-1)--|8)
    apinit ~process_count:1 succ 1 = [|1|]
    apinit ~process_count:0 succ 1 = [|1|]
  **)


  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len
  (**T
    apzipWith (+) (1--|10) (1--|10) = amap (dup (+)) (1--|10)
    apzipWith (-) (1--|5) (3--|1) = [|-2; 0; 2|]
    apzipWith (-) (1--|3) (5--|1) = [|-4; -2; 0|]
    apzipWith (+) [|1|] (1--|10) = [|2|]
    apzipWith (+) (1--|10) [|1|] = [|2|]
    apzipWith (+) [|1|] [|1|] = [|2|]
    apzipWith (+) [||] (1--|10) = [||]
    apzipWith (+) (1--|10) [||] = [||]
    apzipWith (+) [||] [||] = [||]
    apzipWith (+) ~process_count:3 (1--|10) (1--|10) = amap (dup (+)) (1--|10)
    apzipWith (-) ~process_count:2 (1--|5) (3--|1) = [|-2; 0; 2|]
    apzipWith (-) ~process_count:1 (1--|3) (5--|1) = [|-4; -2; 0|]
    apzipWith (+) ~process_count:0 [|1|] (1--|10) = [|2|]
  **)


  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine
  (**T
    PreArray.par_mapReduceWithIndex ~process_count:5 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) (0--|8) = [|9;6;5;2;1|]
    PreArray.par_mapReduceWithIndex ~process_count:5 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) [||] = [||]
    PreArray.par_mapReduceWithIndex ~process_count:5 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) [|0|] = [|1|]
    PreArray.par_mapReduceWithIndex ~process_count:0 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) [|0|] = [|1|]
    PreArray.par_mapReduceWithIndex ~process_count:0 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) [||] = [||]
    PreArray.par_mapReduceWithIndex ~process_count:0 ~combine:(areverse @. aconcat) ~process:(fun (l, idx) -> amap succ (if odd idx then [||] else l)) (0--|9) = (10--|1)
  **)


  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process
  (**T
    apmapReduceWithIndex ~process_count:5 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) (0--|8) = [|9;6;5;2;1|]
    apmapReduceWithIndex ~process_count:5 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) [||] = [||]
    apmapReduceWithIndex ~process_count:5 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) [|0|] = [|1|]
    apmapReduceWithIndex ~process_count:0 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) [|0|] = [|1|]
    apmapReduceWithIndex ~process_count:0 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) [||] = [||]
    apmapReduceWithIndex ~process_count:0 (areverse @. aconcat) (fun (l, idx) -> amap succ (if odd idx then [||] else l)) (0--|9) = (10--|1)
  **)

  let pmapWithInit init f =
    pmapReduceWithIndex concat (fun (sublist, idx) -> map f (init sublist idx))
  (**T
    apmapWithInit ~process_count:2 (fun l i -> if odd i then areverse l else l) succ (0--|9) = (1--|5) @| (10--|6)
    apmapWithInit ~process_count:2 (fun l i -> if odd i then areverse l else l) succ [|0|] = [|1|]
    apmapWithInit ~process_count:2 (fun l i -> if odd i then areverse l else l) succ [||] = [||]
    apmapWithInit ~process_count:1 (fun l i -> if odd i then areverse l else l) succ [|0|] = [|1|]
    apmapWithInit ~process_count:1 (fun l i -> if odd i then areverse l else l) succ [||] = [||]
    apmapWithInit ~process_count:(-1) (fun l i -> if odd i then areverse l else l) succ [|0|] = [|1|]
    apmapWithInit ~process_count:0 (fun l i -> if odd i then areverse l else l) succ [||] = [||]
  **)

end




module PreString =
struct
  include String

  (* Basic operations *)

  let len = length
  (**T
    slen (1--^|10) = 10
    slen "1" = 1
    slen "" = 0
  **)

  let init f l =
    let s = create l in
    for i=0 to l-1 do unsafe_set s i (f i) done;
    s
  (**T
    sinit (chr @. succ) 10 = (1--^|10)
    sinit (fun i -> chr (i+65)) 10 = "ABCDEFGHIJ"
    sinit (chr @. succ) 0 = ""
    sinit (chr @. succ) 1 = "\001"
  **)

  let range s e =
    if s > e
    then init (fun i -> chr (ord s - i)) (ord s - ord e + 1)
    else init (fun i -> chr (ord s + i)) (ord e - ord s + 1)
  (**T
    srange 'A' 'E' = "ABCDE"
    srange 'C' 'C' = "C"
    srange 'E' 'A' = "EDCBA"
  **)

  let reverse s =
    let len = length s in
    let s2 = create len in
    let mlen = len - 1 in
    for i=0 to mlen do
      unsafe_set s2 (mlen-i) (unsafe_get s i)
    done;
    s2
  let rev = reverse
  (**T
    srev (1--^|10) = (10--^|1)
    srev "1" = "1"
    srev "12" = "21"
    srev "foobar" = "raboof"
    srev "" = ""
  **)
  let normalizeIndex i s = if i < 0 then (len s) + i else i
  (**T
    PreString.normalizeIndex 0 "" = 0
    PreString.normalizeIndex 2 "" = 2
    PreString.normalizeIndex (-2) "" = -2
    PreString.normalizeIndex 0 (1--^|10) = 0
    PreString.normalizeIndex 2 (1--^|10) = 2
    PreString.normalizeIndex (-1) (1--^|10) = 9
    PreString.normalizeIndex (-2) (1--^|10) = 8
    snormalizeIndex (-1) (1--^|2) = 1
    PreString.normalizeIndex (-2) (1--^|2) = 0
  **)

  let replicate n v = make (max 0 n) v
  (**T
    PreString.replicate 5 '-' = "-----"
    PreString.replicate 1 '-' = "-"
    sreplicate 0 '-' = ""
    PreString.replicate (-1) '-' = ""
  **)

  let times n a = concat "" (PreList.replicate n a)
  (**T
    (let s = (1--^|3) in stimes 3 s = s ^ s ^ s)
    stimes 0 (1--^|3) = ""
    stimes 1 (1--^|3) = (1--^|3)
    stimes (-1) (1--^|3) = ""
    stimes 4 "" = ""
    stimes 3 "1" = "111"
  **)

  let cycle n a =
    let l = len a in
    if l = 0 || n <= 0 then ""
    else
      let i = ref 0 in
      init (fun _ ->
        if !i >= l then i := 0;
        let v = unsafe_get a !i in
        i := !i + 1;
        v
      ) n
  (**T
    scycle 5 ('1'--^'3') = "12312"
    scycle 3 ('1'--^'9') = "123"
    scycle 3 "1" = "111"
    scycle 3 "" = ""
    scycle 0 "1" = ""
    scycle 1 "1" = "1"
    scycle (-1) "1" = ""
  **)


  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  (**T
    mapWith siter id (sreplicate 100000 '.') = replicate 100000 '.'
    mapWith siter id (1--^|10) = explode (1--^|10)
    mapWith siter succChar (1--^|10) = explode (2--^|11)
    mapWith siter succChar (1--^|1) = ['\002']
    mapWith siter succChar "" = []
  **)

  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done
  (**T
    mapWith (fun f -> siterWithIndex (fun s i -> f (ord s,i))) (uncurry (+)) (0--^|10) = map (multiply 2) (0--10)
    (let i = ref '0' and j = ref 0 in siterWithIndex (fun a b -> i:=a; j:=b) (20--^|30); !i = '\030' && !j = 10)
    siterWithIndex (ignore @.. add @. ord) "1" = ()
    siterWithIndex (ignore @.. add @. ord) "" = ()
  **)

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    smap succChar (1--^|10) = (2--^|11)
    smap succChar "" = ""
    smap succChar "1" = "2"
  **)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)
  (**T
    smapWithIndex (fun s i -> chr (ord s + i)) (0--^|10) = smap (chr @. multiply 2 @. ord) (0--^|10)
    smapWithIndex (chr @.. (-) @. ord) (10--^|20) = sreplicate 11 '\010'
    smapWithIndex (chr @.. (+) @. ord) "" = ""
    smapWithIndex (chr @.. (+) @. ord) "1" = "1"
  **)

  let mapToList f s = PreList.init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    smapToList id "ABC" = explode "ABC"
    smapToList succChar "ABC" = explode "BCD"
    smapToList id "a" = ['a']
    smapToList id "" = []
  **)
  let mapToArray f s = PreArray.init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    smapToArray id "ABC" = aexplode "ABC"
    smapToArray succChar "ABC" = aexplode "BCD"
    smapToArray id "a" = [|'a'|]
    smapToArray id "" = [||]
  **)

  (* Conversions *)

  let to_array s = PreArray.init (unsafe_get s) (len s)
  (**T
    PreString.to_array "ABCDEF" = [|'A'; 'B'; 'C'; 'D'; 'E'; 'F'|]
    PreString.to_array "A" = [|'A'|]
    PreString.to_array "" = [||]
  **)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)
  (**T
    PreString.of_array [|'A'; 'B'; 'C'; 'D'; 'E'; 'F'|] = "ABCDEF"
    PreString.of_array [|'A'|] = "A"
    PreString.of_array [||] = ""
  **)

  let to_list s = PreList.init (unsafe_get s) (len s)
  (**T
    PreString.to_list "ABCDEF" = ['A'; 'B'; 'C'; 'D'; 'E'; 'F']
    PreString.to_list "A" = ['A']
    PreString.to_list "" = []
  **)
  let of_list l = of_array (Array.of_list l)
  (**T
    PreString.of_list ['A'; 'B'; 'C'; 'D'; 'E'; 'F'] = "ABCDEF"
    PreString.of_list ['A'] = "A"
    PreString.of_list [] = ""
  **)

  let to_byte_array s = PreArray.init (fun i -> ord (unsafe_get s i)) (len s)
  (**T
    PreString.to_byte_array "ABCDEF" = (65--|70)
    PreString.to_byte_array "A" = [|65|]
    PreString.to_byte_array "" = [||]
  **)
  let of_byte_array a = init (fun i -> chr (Array.unsafe_get a i)) (Array.length a)
  (**T
    PreString.of_byte_array (65--|70) = "ABCDEF"
    PreString.of_byte_array [|65|] = "A"
    PreString.of_byte_array [||] = ""
  **)

  (* Searching *)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []
  (**T
    sfilter (even @. ord) ('0'--^'9') = "02468"
    sfilter (odd @. ord) ('0'--^'9') = "13579"
    sfilter (even @. ord) "1" = ""
    sfilter (odd @. ord) "1" = "1"
    sfilter (even @. ord) "" = ""
  **)

  let filterWithIndex f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c i then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []
  (**T
    sfilterWithIndex (fun e i -> ord e > 5) (1--^|9) = (6--^|9)
    sfilterWithIndex (fun _ i -> i > 5) (1--^|9) = (7--^|9)
    sfilterWithIndex (fun _ i -> i > 10) (1--^|9) = ""
    sfilterWithIndex (fun _ i -> i > 10) "" = ""
  **)

  let findWithIndex f s =
    let rec aux f s i len =
      if i >= len then raise Not_found
      else
        let v = unsafe_get s i in
        if f v i then (v, i)
        else aux f s (i+1) len in
    aux f s 0 (len s)
  (**T
    sfindWithIndex (fun v _ -> ord v > 4) (2--^|9) = ('\005',3)
    sfindWithIndex (fun _ i -> i > 4) (2--^|9) = ('\007',5)
    optNF (sfindWithIndex (const (gt 4))) (0--^|3) = None
    optNF (sfindWithIndex (const (gt 4))) "" = None
  **)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  (**T
    sfind (gt 5 @. ord) (1--^|9) = '\006'
    optNF (sfind (gt 4 @. ord)) (0--^|3) = None
    optNF (sfind (gt 4 @. ord)) "" = None
  **)

 let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)
  (**T
    sfindIndex (gt 5 @. ord) (1--^|9) = 5
    optNF (sfindIndex (gt 4 @. ord)) (0--^|3) = None
    optNF (sfindIndex (gt 4 @. ord)) "" = None
  **)

  let indexOf v s = findIndex ((=) v) s
  (**T
    sindexOf '\014' (10--^|20) = 4
    optNF (sindexOf '\001') (10--^|20) = None
    sindexOf 'a' "foobar" = 4
  **)

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith
  (**T
    szipWith (+^) (1--^|10) (1--^|10) = smap (dup (+^)) (1--^|10)
    szipWith (-^) (3--^|7) (3--^|1) = "\000\002\004"
    szipWith (-^) (5--^|7) (5--^|1) = "\000\002\004"
    szipWith (+^) "1" (1--^|10) = "2"
    szipWith (+^) (1--^|10) "1" = "2"
    szipWith (+^) "\001" "\001" = "\002"
    szipWith (+^) "" (1--^|10) = ""
    szipWith (+^) (1--^|10) "" = ""
    szipWith (+^) "" "" = ""
  **)

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3
  (**T
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|50) (1--^|50) (1--^|50) = smap (fun c -> chr (ord c * 3)) (1--^|50)
    szipWith3 (fun a b c -> a +^ b +^ c) "1" (1--^|10) (1--^|10) = "3"
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|10) "1" (1--^|10) = "3"
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|10) (1--^|10) "1" = "3"
    szipWith3 (fun a b c -> a +^ b +^ c) "1" (1--^|10) "\001" = "3"
    szipWith3 (fun a b c -> a +^ b +^ c) "1" "\001" (1--^|10) = "3"
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|10) "" "1" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|10) "1" "" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) (1--^|10) "" "" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) "" "1" "1" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) "" "" "1" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) "" "1" "" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) "1" "" "" = ""
    szipWith3 (fun a b c -> a +^ b +^ c) "" "" "" = ""
  **)

  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0
  (**T
    sfoldl (+^) '\000' (1--^|10) = '\055'
    sfoldl (fun s b -> s ^ (string_of_char b)) "--" ('1'--^'3') = "--123"
    sfoldl (+^) '1' "" = '1'
    sfoldl (+^) '1' "\001" = '2'
  **)

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a
  (**T
    sfoldl1 (+^) (1--^|10) = chr 55
    optNF (sfoldl1 (+^)) "" = None
    sfoldl1 (+^) "1" = '1'
  **)

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)
  (**T
    sfoldr (+^) '\000' (1--^|10) = chr 55
    sfoldr (+^) '\001' "" = '\001'
    sfoldr (+^) '\001' "1" = '2'
  **)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a
  (**T
    sfoldr1 (+^) (1--^|10) = chr 55
    optNF (sfoldr1 (+^)) "" = None
    sfoldr1 (+^) "1" = '1'
  **)


  let maximum a = foldl1 max a
  (**T
    smaximum "12301431" = '4'
    smaximum "1" = '1'
    optNF smaximum "" = None
  **)
  let minimum a = foldl1 min a
  (**T
    sminimum "12301431" = '0'
    sminimum "1" = '1'
    optNF sminimum "" = None
  **)

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  (**T
    smaximumBy (square @. subtract 3 @. ord) (0 --^| 5) = chr 0
    smaximumBy (square @. subtract 2 @. ord) (0 --^| 5) = chr 5
    optNF (smaximumBy (square @. ord)) "" = None
  **)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)
  (**T
    sminimumBy (square @. subtract 3 @. ord) (0 --^| 5) = chr 3
    sminimumBy (square @. subtract 3 @. ord) (0 --^| 1) = chr 1
    optNF (sminimumBy (square @. ord)) "" = None
  **)

  let maximumByWith f lst = PreArray.maximumBy snd (mapToArray (fupler f) lst)
  (*T
    smaximumByWith square (-3 --^| 2) = (-3, 9)
    smaximumByWith square (-1 --^| 2) = (2, 4)
    optNF (smaximumByWith square) "" = None
  **)
  let minimumByWith f lst = PreArray.minimumBy snd (mapToArray (fupler f) lst)
  (*T
    sminimumByWith square (-3 --^| (-1)) = (-1, 1)
    sminimumByWith square (-1 --^| 2) = (0, 0)
    optNF (sminimumByWith square) "" = None
  **)

  (* Subsequences *)

  let sub_start_and_length i len s =
    let slen = length s in
    let ni = normalizeIndex i s in
    let len = len + min 0 ni in
    let len = len + min 0 (slen-1-ni) in
    let i = max 0 (min (slen-1) ni) in
    let j = max 0 (min slen (i + len)) - 1 in
    (i, max 0 (j-i+1))

  let sub i len s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun x -> unsafe_get s (first+x)) sub_len
  (**T
    PreString.sub 2 3 "foobar" = "oba"
    PreString.sub (-3) 2 "foobar" = "ba"
    PreString.sub 0 0 (1--^|10) = ""
    PreString.sub 0 0 "" = ""
    PreString.sub 0 1 "" = ""
    PreString.sub 1 0 "" = ""
    PreString.sub 1 1 "" = ""
    PreString.sub 0 0 "1" = ""
    PreString.sub 0 (-1) "1" = ""
    PreString.sub (-10) (-1) "1" = ""
    PreString.sub (-10) 1 "1" = ""
    PreString.sub 0 1 "1" = "1"
    PreString.sub 1 1 "1" = ""
    PreString.sub 1 0 "1" = ""
    PreString.sub 80 1 "1" = ""
    PreString.sub 15 (-5) (1--^|10) = ""
    PreString.sub 0 1 (1--^|10) = "\001"
    ssub 0 (-1) (1--^|10) = ""
  **)

  let slice_to_sub i j s =
    let si = normalizeIndex i s
    and sj = normalizeIndex j s + 1 in
    let len = sj - si in
    i, len

  let slice i j s =
    let i, len = slice_to_sub i j s in
    sub i len s
  (**T
    PreString.slice 2 3 ("foobar") = "ob"
    PreString.slice (-3) (-1) ("foobar") = "bar"
    PreString.slice 0 0 (1--^|10) = "\001"
    PreString.slice 0 1 (1--^|10) = "\001\002"
    PreString.slice 1 0 (1--^|10) = ""
    sslice 0 (-1) (1--^|10) = (1--^|10)
  **)

  let subStride stride i len a =
    if stride < 0 then invalid_arg "PreString.subStride: negative stride";
    let len = max 0 len in
    let first, sub_len = sub_start_and_length i (len*stride) a in
    let stride_len = (sub_len+(stride-1)) / stride in
    init (fun j -> unsafe_get a (first + j*stride)) stride_len
  (**T
    ssubStride 1 2 3 ("foobar") = "oba"
    ssubStride 2 1 3 ("foobar") = "obr"
    ssubStride 2 0 3 ("foobar") = "foa"
    ssubStride 3 0 3 ("foobar") = "fb"
    ssubStride 4 0 3 ("foobar") = "fa"
    ssubStride 4 1 3 ("foobar") = "or"
    ssubStride 4 1 1 ("foobar") = "o"
    ssubStride 8 1 3 ("foobar") = "o"
    ssubStride 1 0 (-1) ("foobar") = ""
    ssubStride 1 (-1) 1 ("foobar") = "r"
    ssubStride 1 0 1 ("f") = "f"
    ssubStride 2 0 1 ("f") = "f"
    ssubStride 4 0 1 ("f") = "f"
    ssubStride 4 0 0 ("f") = ""
    ssubStride 4 1 0 ("f") = ""
    ssubStride 4 0 1 ("") = ""
    ssubStride 4 1 1 ("") = ""
    optE (ssubStride (-1) 0 10) "" = None
  **)


  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  (**T
    sfirst (2--^|10) = '\002'
    optNF shead "" = None
    optNF shead "1" = Some '1'
    optNF shead "123456789" = Some '1'
  **)
  let tail a = if len a = 0 then raise Not_found else slice 1 (-1) a
  (**T
    optNF stail "" = None
    optNF stail "1" = Some ""
    optNF stail (1--^|10) = Some (2--^|10)
  **)


  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  (**T
    slast "123" = '3'
    slast "1" = '1'
    optNF slast "" = None
  **)
  let popped a = if len a = 0 then raise Not_found else slice 0 (-2) a
  (**T
    spopped "123" = "12"
    optNF spopped "" = None
    optNF spopped "1" = Some ""
  **)

  let append = (^)

  let pop a = (popped a, last a)
  (**T
    spop "123" = ("12", '3')
    optNF spop "" = None
    optNF spop "1" = Some ("", '1')
    optNF spop "12" = Some ("1", '2')
  **)
  let push v a = append a (string_of_char v)
  (**T
    spush '\010' (1--^|9) = (1--^|10)
    spush '1' "0" = ('0'--^'1')
    spush '0' "" = "0"
  **)

  let shift a = (tail a, first a)
  (**T
    sshift (1--^|10) = ((2--^|10), '\001')
    optNF sshift "" = None
    optNF sshift "1" = Some ("", '1')
  **)
  let unshift v a = append (string_of_char v) a
  (**T
    sunshift '\000' (1--^|10) = (0--^|10)
    sunshift '\000' "\001" = (0--^|1)
    sunshift '\000' "" = "\000"
  **)

  let take n s = sub 0 n s
  (**T
    PreString.take 3 (1--^|10) = (1--^|3)
    PreString.take 0 (1--^|10) = ""
    PreString.take 15 (1--^|10) = (1--^|10)
    PreString.take 3 "" = ""
    stake 0 "" = ""
  **)
  let takeWhile f s = take (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    PreString.takeWhile (lt '\005') (1--^|10) = (1--^|4)
    PreString.takeWhile (lt '\005') (6--^|10) = ""
    PreString.takeWhile (lt '\005') "" = ""
    stakeWhile (lt '\005') (1--^|3) = (1--^|3)
  **)
  let takeUntil f s = take (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    PreString.takeUntil (gte '\005') (1--^|10) = (1--^|4)
    PreString.takeUntil (gte '\005') (6--^|10) = ""
    PreString.takeUntil (gte '\005') "" = ""
    stakeUntil (gte '\005') (1--^|3) = (1--^|3)
  **)
  let drop n s = sub n (len s - n) s
  (**T
    PreString.drop 3 (1--^|10) = (4--^|10)
    PreString.drop 0 (1--^|10) = (1--^|10)
    PreString.drop 15 (1--^|10) = ""
    PreString.drop 3 "" = ""
    sdrop 0 "" = ""
  **)
  let dropWhile f s = drop (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    PreString.dropWhile (lt '\005') (1--^|10) = (5--^|10)
    PreString.dropWhile (lt '\005') (6--^|10) = (6--^|10)
    PreString.dropWhile (lt '\005') "" = ""
    sdropWhile (lt '\005') (1--^|3) = ""
  **)
  let dropUntil f s = drop (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    PreString.dropUntil (gte '\005') (1--^|10) = (5--^|10)
    PreString.dropUntil (gte '\005') (6--^|10) = (6--^|10)
    PreString.dropUntil (gte '\005') "" = ""
    sdropUntil (gte '\005') (1--^|3) = ""
  **)
  let splitAt n xs = (take n xs, drop n xs)
  (**T
    PreString.splitAt 3 ("foobar") = ("foo", "bar")
    PreString.splitAt 1 "1" = ("1", "")
    PreString.splitAt 0 "1" = ("", "1")
    PreString.splitAt 1 "" = ("", "")
    ssplitAt 0 "" = ("", "")
  **)
  let break f s = splitAt (maybeNF (len s) (findIndex f) s) s
  (**T
    PreString.break (gt '\005') "\004\003\006\002" = ("\004\003", "\006\002")
    sbreak (gt '\005') (1--^|10) = ((1--^|5), (6--^|10))
  **)
  let span f s = break (fun v -> not (f v)) s
  (**T
    PreString.span (gt '5') "6006" = ("6", "006")
    PreString.span (lessOrEqualTo '5') ('0'--^'9') = ("012345", "6789")
    PreString.span (gt '5') "" = ("", "")
    PreString.span (gt '5') "6" = ("6", "")
    PreString.span (gt '5') "0" = ("", "0")
    PreString.span (gt '5') "06" = ("", "06")
    PreString.span (gt '5') "60" = ("6", "0")
    PreString.span (gt '5') "66" = ("66", "")
    sspan (gt '5') "00" = ("", "00")
  **)

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (max 0 (2 * len s - 1))
  (**T
    PreString.interlace '0' "123" = "10203"
    PreString.interlace '-' "abcde" = "a-b-c-d-e"
    PreString.interlace '0' "" = ""
    PreString.interlace '0' "1" = "1"
    sinterlace '0' "12" = "102"
  **)


  let reject f s = filter (fun v -> not (f v)) s
 (**T
    PreString.reject (gt '\004') (1--^|5) = (1--^|4)
    PreString.reject (gt '\004') "" = ""
    PreString.reject (gt '\000') (1--^|5) = ""
    PreString.reject (gt '\005') (1--^|5) = (1--^|5)
    sreject (gt '\003') (5--^|1) = (3--^|1)
  **)
  let without v s = filter ((<>) v) s
  (**T
    PreString.without '4' "124124" = "1212"
    PreString.without '4' "" = ""
    PreString.without '4' "4" = ""
    swithout '4' "1" = "1"
  **)

  let groupsOf n a =
    let l = len a in
    let n = max 1 n in
    if l = 0 then [a] else
      let count, rem = quot_rem l n in
      unfoldrWhile (gte 0) (fun i -> sub (i*n) n a, i-1) (count-1) @
      if rem = 0 then [] else [sub (-rem) rem a]
  (**T
    PreString.groupsOf 3 ('0'--^'9') = ["012";"345";"678";"9"]
    PreString.groupsOf 3 "" = [""]
    PreString.groupsOf 3 "1" = ["1"]
    PreString.groupsOf 3 (1--^|3) = [(1--^|3)]
    PreString.groupsOf 5 (1--^|3) = [(1--^|3)]
    PreString.groupsOf 3 (1--^|4) = [(1--^|3); "\004"]
    PreString.groupsOf 1 (1--^|3) = ["\001";"\002";"\003"]
    PreString.groupsOf 0 (1--^|3) = ["\001";"\002";"\003"]
    sgroupsOf (-1) (1--^|3) = ["\001";"\002";"\003"]
  **)

  let splitInto n range =
    let len = len range in
    let plen = int (ceil (float len /. float (max 1 n))) in
    groupsOf plen range
  (**T
    PreString.splitInto 4 ('0'--^'9') = ["012";"345";"678";"9"]
    PreString.splitInto 3 (1--^|3) = ["\001";"\002";"\003"]
    PreString.splitInto 1 (1--^|3) = [(1--^|3)]
    PreString.splitInto 0 (1--^|3) = [(1--^|3)]
    PreString.splitInto (-1) (1--^|3) = [(1--^|3)]
    PreString.splitInto 2 "" = [""]
    ssplitInto 1 "" = [""]
  **)


  (* Subsequence iterators *)

  let iterSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    for j=first to first+sub_len-1 do f (unsafe_get s j) done

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s

  let mapSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun j -> f (unsafe_get s (first+j))) sub_len

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)

  let foldl1Sub i len f s =
    let i = normalizeIndex i s in
    if i < 0 || i >= length s then raise Not_found;
    foldlSub (i+1) (len-1) f (unsafe_get s i) s

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)

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
    let process_count = max 1 (process_count |? !global_process_count) in
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
    let process_count = max 1 (process_count |? !global_process_count) in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat "" (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
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

  let range s e =
    if s > e
    then init ((-) s) (s-e+1)
    else init ((+) s) (e-s+1)
  (**T
    brange 65 69 = "ABCDE"
    brange 67 67 = "C"
    brange 69 65 = "EDCBA"
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
  let takeWhile f s = take (maybeNF (len s-1) (findIndex (fun v -> not (f v))) s + 1) s

  let drop n s = sub (-n) (len s - n) s
  let dropWhile f s = drop (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s

  let splitAt n xs = (take n xs, drop n xs)

  let break f s = splitAt (maybeNF (len s) (findIndex f) s) s
  let span f s = break (fun v -> not (f v)) s

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (2 * len s - 1)

  let reject f s = filter (fun v -> not (f v)) s
  let without v s = filter ((<>) v) s


(* Subsequence iterators *)

  let iterSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    for j=first to first+sub_len-1 do f (unsafe_get s j) done

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s

  let mapSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun j -> f (unsafe_get s (first+j))) sub_len

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)

  let foldl1Sub i len f s =
    let i = normalizeIndex i s in
    if i < 0 || i >= length s then raise Not_found;
    foldlSub (i+1) (len-1) f (unsafe_get s i) s

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)

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
    let process_count = max 1 (process_count |? !global_process_count) in
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
    let process_count = max 1 (process_count |? !global_process_count) in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat (par_map ~process_count process (0--(process_count-1)))

  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
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
    let process_count = max 1 (process_count |? !global_process_count) in
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
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    PreList.concat (par_map ~process_count (uncurry (zipWith f)) (PreList.zip aspl bspl))

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
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
let areplicate = PreArray.replicate
let anormalizeIndex = PreArray.normalizeIndex
let acycle = PreArray.cycle

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

let amaximum = PreArray.maximum
let amaximumBy = PreArray.maximumBy
let amaximumByWith = PreArray.maximumByWith

let aminimum = PreArray.minimum
let aminimumBy = PreArray.minimumBy
let aminimumByWith = PreArray.minimumByWith

let aaverage = PreArray.average
let aaverageSub = PreArray.averageSub
let aaverageSlice = PreArray.averageSlice

let aaveragef = PreArray.averagef
let aaverageSubf = PreArray.averageSubf
let aaverageSlicef = PreArray.averageSlicef

let arange = PreArray.range
let arangef = PreArray.rangef
let acharRange = PreArray.charRange
let azipWith = PreArray.zipWith
let aindexOf = PreArray.indexOf
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
let atakeUntil = PreArray.takeUntil

let adrop = PreArray.drop
let adropWhile = PreArray.dropWhile
let adropUntil = PreArray.dropUntil

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
(**T
  (1--|3) @| (4--|6) = (1--|6)
**)
let (@|*) n a = PreArray.times a n
(**T
  (1--|3) @|* 2 = [|1;2;3;1;2;3|]
**)
let (--|) = PreArray.range
(**T
  (1--|10) = array (1--10)
  (10--|1) = array (10--1)
**)
let (--.|) = PreArray.rangef
(**T
  (1.--.|10.) = array (1.--.10.)
  (10.--.|1.) = array (10.--.1.)
**)
let (-~|) = PreArray.charRange
(**T
  ('a'-~|'c') = [|'a';'b';'c'|]
**)

(* String operation shortcuts *)

let suget = PreString.unsafe_get
let suset = PreString.unsafe_set

let smake = PreString.make
let screate = PreString.create
let sinit = PreString.init
let srange = PreString.range
let slen = PreString.length
let sconcat = PreString.concat
let sreverse = PreString.reverse
let srev = sreverse
let snormalizeIndex = PreString.normalizeIndex
let sreplicate = PreString.replicate
let stimes = PreString.times
let scycle = PreString.cycle

let smapToList = PreString.mapToList
let smapToArray = PreString.mapToArray

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
let sindexOf = PreString.indexOf

let (+^) a b = chr (ord a + ord b)
let (-^) a b = chr (ord a - ord b)
let ( *^ ) a b = chr (ord a * ord b)
let ( /^ ) a b = chr (ord a / ord b)

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

let smaximum = PreString.maximum
let smaximumBy = PreString.maximumBy
let smaximumByWith = PreString.maximumByWith

let sminimum = PreString.minimum
let sminimumBy = PreString.minimumBy
let sminimumByWith = PreString.minimumByWith

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

let srange = PreString.range
let szipWith = PreString.zipWith
let smap2 = PreString.zipWith
let szipWith3 = PreString.zipWith3
let smap3 = PreString.zipWith3
let ssub = PreString.sub
let sslice = PreString.slice
let ssubStride = PreString.subStride
let sgroupsOf = PreString.groupsOf
let ssplitInto = PreString.splitInto

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
let stakeUntil = PreString.takeUntil

let sdrop = PreString.drop
let sdropWhile = PreString.dropWhile
let sdropUntil = PreString.dropUntil

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

let (^*) n a = PreString.times a n
(**T
  "foo" ^* 3 = "foofoofoo"
**)
let (--^) = PreString.range
(**T
  ('a'--^'d') = "abcd"
**)

(* String specific shortcuts *)

let uppercase = String.uppercase
let lowercase = String.lowercase
let capitalize = String.capitalize

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
let brange = Bytestring.range
let blen = Bytestring.length
let bconcat = Bytestring.concat
let breverse = Bytestring.reverse
let brev = breverse
let bnormalizeIndex = Bytestring.normalizeIndex

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

let (--^|) = Bytestring.range



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
