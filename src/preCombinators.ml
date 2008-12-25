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
(**T
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
let char_of_string s = if String.length s <> 1 then None else Some (s.[0])


(* Unfolds and recursion *)

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
let generateR p f init = unfoldr p (fupler f) init
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


(* Time operations *)

let timeNow = Unix.gettimeofday
let timeZone = Netdate.localzone
let formatTime ?(zone=timeZone) fmt f = Netdate.format ~fmt (Netdate.create ~zone f)
let showTime = formatTime "%Y-%m-%d %H:%M:%S%z"
let showDate = formatTime "%Y-%m-%d"
let httpDate = formatTime ~zone:0 "%a, %d %b %Y %H:%M:%S GMT"

let second = 1.0
let minute = 60.0 *. second
let hour = 60.0 *. minute
let day = 24.0 *. hour
let week = 7.0 *. day
let month = 31.0 *. day
let year = 365.0 *. day

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
