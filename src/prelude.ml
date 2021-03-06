(*
Prelude.ml: OCaml utility functions

Copyright (C) 2007-2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>
              Boyer-Moore string search:
                2007 Mauricio Fernandez <mfp@acm.org> http//eigenclass.org

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
let ignoreE f o = maybeE () (fun i -> ignore (f i)) o
(**T
  ignoreE last [] = ()
  ignoreE last [1] = ()
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

type 'a catch_result = Result of 'a | Error of exn

let catch f o = try Result (f o) with e -> Error e
(**T
  catch last [] = Error Not_found
  catch last [1;2;3] = Result 3
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

let base_default_alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let showIntInBase ?(alphabet=base_default_alphabet) n i =
  let rec aux a n sg i l =
    if i == 0
    then
      let l = if l = [] && n > 0 then [a.[0]] else l in
      let len = List.length l + if sg < 0 then 1 else 0 in
      let s = String.create len in
      if sg < 0 then s.[0] <- '-';
      let () = ignore (List.fold_right (fun i c -> s.[c-1] <- i; c-1) l len) in
      s
    else
      let quot = (i / n) in
      let rem = sg * (i - (quot*n)) in
      aux a n sg quot (a.[rem] :: l)
  in
  if String.length alphabet < n
    then invalid_arg (sprintf "Prelude.base: too short alphabet for base (%d, %S)" n alphabet);
  if n < 0
    then invalid_arg "Prelude.base: negative base is not allowed";
  let sg = if i < 0 then -1 else 1 in
  if n == 1 then
    let s = String.make (if sg < 0 then -i+1 else i) alphabet.[0] in
    let () = if sg < 0 then s.[0] <- '-' else () in
    s
  else aux alphabet n sg i []
(**T
  showIntInBase 0 0 = ""
  optEx Division_by_zero (showIntInBase 0) 47 = None
  showIntInBase 1 45 = sreplicate 45 '0'
  showIntInBase 1 0 = ""
  showIntInBase 1 (-4) = "-0000"
  showIntInBase 2 0 = "0"
  showIntInBase 2 1 = "1"
  showIntInBase 2 2 = "10"
  showIntInBase 2 (-1) = "-1"
  showIntInBase 2 (-2) = "-10"
  showIntInBase 10 8489 = string_of_int 8489
  showIntInBase 16 255 = "FF"
  showIntInBase 16 (-255) = "-FF"
  showIntInBase 16 (-1) = "-1"
  showIntInBase ~alphabet:(lowercase base_default_alphabet) 16 255 = "ff"
  showIntInBase 17 16 = "G"
  showIntInBase 17 17 = "10"
  showIntInBase 32 31 = "V"
  showIntInBase 32 (-31) = "-V"
  showIntInBase 32 32 = "10"

  optE (showIntInBase 80) 92 = None
  optE (showIntInBase (-1)) 73 = None
**)
let showInt ?base ?alphabet i = match base with
  | None -> string_of_int i
  | Some b -> showIntInBase ?alphabet b i 
(**T
  showInt 0 = "0"
  showInt 8489 = "8489"
  showInt (-1) = "-1"
  showInt ~base:0 0 = ""
  optEx Division_by_zero (showInt ~base:0) 47 = None
  showInt ~base:1 45 = sreplicate 45 '0'
  showInt ~base:1 0 = ""
  showInt ~base:1 (-4) = "-0000"
  showInt ~base:2 0 = "0"
  showInt ~base:2 1 = "1"
  showInt ~base:2 2 = "10"
  showInt ~base:2 (-1) = "-1"
  showInt ~base:2 (-2) = "-10"
  showInt ~base:16 255 = "FF"
  showInt ~base:16 (-255) = "-FF"
  showInt ~base:16 (-1) = "-1"
  showInt ~base:16 ~alphabet:(lowercase base_default_alphabet) 255 = "ff"
  showInt ~base:17 16 = "G"
  showInt ~base:17 17 = "10"
  showInt ~base:32 31 = "V"
  showInt ~base:32 (-31) = "-V"
  showInt ~base:32 32 = "10"

  optE (showInt ~base:80) 92 = None
  optE (showInt ~base:(-1)) 73 = None
**)

let binary i =
  let rec aux s i c =
    if i == 0
    then s
    else (
      s.[Sys.word_size-2-c] <- if i land 1 == 0 then '0' else '1';
      aux s (i lsr 1) (c+1)
    ) in
  let s = String.make (Sys.word_size-1) '0' in
  aux s i 0
(**T
  binary 0 = sreplicate (Sys.word_size-1) '0'
  binary 1 = sreplicate (Sys.word_size-2) '0' ^ "1"
  binary 2 = sreplicate (Sys.word_size-3) '0' ^ "10"
  binary (-1) = sreplicate (Sys.word_size-2) '1' ^ "1"
  binary (-2) = sreplicate (Sys.word_size-3) '1' ^ "10"
  binary (-3) = sreplicate (Sys.word_size-3) '1' ^ "01"
  binary min_int = "1" ^ sreplicate (Sys.word_size-2) '0'
  binary max_int = "0" ^ sreplicate (Sys.word_size-2) '1'
**)

let hex i = sprintf "%02X" i
(**T
  hex 0 = "00"
  hex 1 = "01"
  hex 15 = "0F"
  hex 16 = "10"
  hex 255 = "FF"
  hex 256 = "100"

  xmatch "^7FFFFF+$" (hex (-1))
  xmatch "^3FFFFF+$" (hex max_int)
  xmatch "^400000+$" (hex min_int)
**)

let octal i = sprintf "%o" i
(**T
  octal 0 = "0"
  octal 1 = "1"
  octal 7 = "7"
  octal 8 = "10"
  octal 63 = "77"
  octal 64 = "100"

  xmatch "^[17]77777+$" (octal (-1))
  xmatch "^[37]77777+$" (octal max_int)
  xmatch "^[41]00000+$" (octal min_int)
**)

let clamp1f c = min 1.0 (max 0.0 c)
(**T
  clamp1f 0.0 = 0.0
  clamp1f 0.5 = 0.5
  clamp1f 1.0 = 1.0
  clamp1f (-1.0) = 0.0
  clamp1f 2.0 = 1.0
**)

let clamp8 c = min 255 (max 0 c)
(**T
  clamp8 0 = 0
  clamp8 125 = 125
  clamp8 255 = 255
  clamp8 (-1) = 0
  clamp8 256 = 255
**)

let clamp16 c = min 65535 (max 0 c)
(**T
  clamp16 0 = 0
  clamp16 125 = 125
  clamp16 65535 = 65535
  clamp16 (-1) = 0
  clamp16 65536 = 65535
**)

let htmlColor r g b =
  sprintf "#%02X%02X%02X" (clamp8 r) (clamp8 g) (clamp8 b)
(**T
  htmlColor 0 0 0 = "#000000"
  htmlColor 255 255 255 = "#FFFFFF"
  htmlColor 255 0 127 = "#FF007F"
  htmlColor 0 255 0 = "#00FF00"

  htmlColor (-1) (-90300) 56684 = "#0000FF"
**)
let htmlRgbaColor r g b a =
  sprintf "rgba(%d,%d,%d,%s)" (clamp8 r) (clamp8 g) (clamp8 b) (showFloat (clamp1f a))
(**T
  htmlRgbaColor 0 0 0 0. = "rgba(0,0,0,0.0)"
  htmlRgbaColor 255 255 255 1.0 = "rgba(255,255,255,1.0)"
  htmlRgbaColor 255 0 127 0.5 = "rgba(255,0,127,0.5)"

  htmlRgbaColor (-1) (-90300) 56684 72.0 = "rgba(0,0,255,1.0)"
  htmlRgbaColor (-1) (-90300) 56684 (-72.0) = "rgba(0,0,255,0.0)"
**)
let rgbColor r g b = sprintf "rgb:%02X/%02X/%02X" (clamp8 r) (clamp8 g) (clamp8 b)
(**T
  rgbColor 0 0 0 = "rgb:00/00/00"
  rgbColor 255 255 255 = "rgb:FF/FF/FF"
  rgbColor 255 0 127 = "rgb:FF/00/7F"
  rgbColor 0 255 0 = "rgb:00/FF/00"

  rgbColor (-1) (-90300) 56684 = "rgb:00/00/FF"
**)

let bashResetStyle = "\027[0m"
let bashBlack = "\027[0;30m"
let bashBoldBlack = "\027[1;30m"
let bashBlue = "\027[0;34m"
let bashBoldBlue = "\027[1;34m"
let bashGreen = "\027[0;32m"
let bashBoldGreen = "\027[1;32m"
let bashCyan = "\027[0;36m"
let bashBoldCyan = "\027[1;36m"
let bashRed = "\027[0;31m"
let bashBoldRed = "\027[1;31m"
let bashPurple = "\027[0;35m"
let bashBoldPurple = "\027[1;35m"
let bashBrown = "\027[0;33m"
let bashYellow = "\027[1;33m"
let bashLightGray = "\027[0;37m"
let bashWhite = "\027[1;37m"


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
    max 1 (countCores (readLines ic []))
  )

let global_process_count = ref (coreCount () |? 4)

let rec restartEINTR f v =
  try f v
  with Unix.Unix_error(Unix.EINTR,_,_) -> restartEINTR f v

let invoke f x =
  (try flush stdout with _ -> ());
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
        ignore (restartEINTR (Unix.waitpid []) pid);
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

  (* Predicates *)

  let all p a = None = optNF (find (fun i -> not (p i))) a
  (**T
    aall (gt 5) (1--|10) = false
    aall (lt 11) (1--|10) = true
    aall (gt 4) [||] = true
  **)
  let any p a = None <> optNF (find p) a
  (**T
    aany (gt 5) (1--|10) = true
    aany (gt 11) (1--|10) = false
    aany (gt 4) [||] = false
  **)

  let allEqual l =
    if len l = 0 then true
    else let h = l.(0) in all (fun i -> h = i) l
  (**T
    aallEqual (1--|10) = false
    aallEqual [||] = true
    aallEqual (areplicate 10 'a') = true
  **)

  let includes x s = None <> optNF (indexOf x) s
  (**T
    aincludes 4 (1--|10) = true
    aincludes 0 (1--|10) = false
    aincludes 5 [||] = false
  **)
  let has = includes
  (**T
    ahas 1 (1--|10) = true
    ahas 0 (1--|10) = false
  **)
  let elem = includes
  (**T
    aelem 'b' [|'a'; 'c'|] = false
    aelem "foo" [|"bar"; "baz"; "foo"|] = true
  **)
  let notElem x lst = not @@ elem x lst
  (**T
    anotElem 4 (1--|10) = false
    anotElem 0 (1--|10) = true
    anotElem 5 [||] = true
  **)

  let null = function [||] -> true | _ -> false
  (**T
    anull [||] = true
    anull (1--|10) = false
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
    if PreList.exists (fun i -> i >= l || i < 0) indices then raise Not_found;
    PreList.map (fun i -> unsafe_get s i) indices
  (**T
    apick [2; 3] (aexplode "foobar") = ['o'; 'b']
    apick [] [||] = []
    apick [] (1--|10) = []
    apick [0; 9] (1--|10) = [1; 10]
    optNF (apick [2;3]) [|1;2;3|] = None
    optNF (apick [2;3]) [||] = None
    optNF (apick [-2;3]) (1--|10) = None
  **)

  let pickWith funcs s = PreList.map (fun f -> f s) funcs
  (**T
    apickWith [afirst; alast] (aexplode "foobar") = ['f'; 'r']
  **)

  let count f s = foldl (fun s i -> s + if f i then 1 else 0) 0 s
  (**T
    acount ((=) '/') [||] = 0
    acount ((=) '/') (aexplode "/foooo/bar//baz") = 4
  **)
  (**Q
    (Q.pair (Q.int) (Q.array Q.int)) (fun (i,a) -> acount (gt i) a = alen (afilter (gt i) a))
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




module type STRINGSEARCH =
  sig
    type t

    val make : string -> t
    val find : t -> string -> int -> int
  end


module BoyerMoore : STRINGSEARCH =
struct
  exception Done of int

  let suffixes x =
    let min_i (a:int) (b:int) = if a < b then a else b in
    let rec aux2 x g m f =
      if g >= 0 && x.[g] = x.[g+m-1-f]
      then aux2 x (g-1) m f
      else g in
    let rec aux x suff m i g f =
      if i < 0 then
        suff
      else (
        if (i > g && suff.(i+m-1-f) < i-g) then (
          suff.(i) <- suff.(i+m-1-f);
          aux x suff m (i-1) g f
        ) else (
          let g = aux2 x (min_i i g) m i in
          suff.(i) <- i - g;
          aux x suff m (i-1) g i
        )
      ) in
    let m = String.length x in
    let suff = Array.make m m in
    aux x suff m (m-2) (m-1) (m-2)

  let good_suffixes x =
    let rec aux2 i j m bmGs =
      if j >= m-1-i then j
      else (
        if bmGs.(j) = m then bmGs.(j) <- m-1-i;
        aux2 i (j+1) m bmGs
      ) in
    let rec aux i j m suff bmGs =
      if i < 0 then ()
      else if suff.(i) = i+1 then
        let j = aux2 i j m bmGs in
        aux (i-1) j m suff bmGs
      else
        aux (i-1) j m suff bmGs
      in
    let m = String.length x in
    let suff = suffixes x in
    let bmGs = Array.make m m in
    aux (m-1) 0 m suff bmGs;
    for i=0 to m-2 do
      bmGs.(m-1-suff.(i)) <- m-1-i;
    done;
    bmGs
    
  let max_i (a : int) (b : int) = if a > b then a else b

  type t = {
    skip : int array;
    occ : int array;
    needle : string;
    nlen : int
  }

  let make needle =
    let nlen = String.length needle in
    let skip = good_suffixes needle in
    let occ = Array.make 256 (-1) in

      for a = 0 to nlen - 1 - 1 do
        occ.(Char.code needle.[a]) <- a;
      done;

      { skip = skip; occ = occ; needle = needle; nlen = nlen }

  let boyermoore_search t haystack start hlen =
    let skip = t.skip and occ = t.occ and needle = t.needle and nlen = t.nlen in
      if nlen > hlen || nlen <= 0 then raise Not_found;
      try
        let hpos = ref start in
        let lim = hlen - nlen in
          while !hpos <= lim do
            let npos = ref (nlen - 1) in
              while needle.[!npos] = haystack.[!npos + !hpos] do
                if !npos = 0 then raise (Done !hpos);
                decr npos
              done;
              hpos := !hpos +
              max_i (Array.unsafe_get skip !npos)
                (!npos - (occ.(Char.code (haystack.[!npos + !hpos]))))
          done;
          raise Not_found
      with Done m -> m

  let find t haystack start =
    if String.length t.needle = 0 && String.length haystack = 0 && start = 0
    then 0
    else boyermoore_search t haystack start (String.length haystack)

end
(**T
  BoyerMoore.find (BoyerMoore.make "foo") "foobarfoofoo" 0 = 0
  BoyerMoore.find (BoyerMoore.make "bar") "foobarfoofoo" 0 = 3
  BoyerMoore.find (BoyerMoore.make "foo") "foobarfoofoo" 3 = 6
  BoyerMoore.find (BoyerMoore.make "foo") "foobarfoofoo" 9 = 9
  optNF (BoyerMoore.find (BoyerMoore.make "foo") "foobarfoofoo") 10 = None
  optNF (BoyerMoore.find (BoyerMoore.make "foo") "foobarfoofoo") 12 = None
  optNF (BoyerMoore.find (BoyerMoore.make "fooo") "foobarfoofoo") 0 = None
  optNF (BoyerMoore.find (BoyerMoore.make "") "foobarfoofoo") 0 = None
  BoyerMoore.find (BoyerMoore.make "") "" 0 = 0
**)
(**Q
  Q.string (fun s -> BoyerMoore.find (BoyerMoore.make (ssub (Random.int(max 1 (slen s))) (Random.int(max 1 (slen s))+1) s)) s 0 >= 0)
**)

module StringSearch : STRINGSEARCH =
struct
  let endsWith suffix s =
    let rec aux s p pl sl i =
      if i >= pl then true
      else if String.unsafe_get s (sl-i-1) = String.unsafe_get p (pl-i-1)
      then aux s p pl sl (i+1)
      else false in
    let sl = String.length s
    and pl = String.length suffix in
    if pl > sl then false
    else aux s suffix pl sl 0

  let char_find needle haystack start =
    let rec aux c h hl hi =
      if hi >= hl then raise Not_found
      else if c = String.unsafe_get h hi then hi
      else aux c h hl (hi+1) in
    aux needle haystack (String.length haystack) start

  let escape_null_bytes s =
    let idxs = unfoldlOpt (fun i ->
      try let ni = char_find '\000' s i in
          Some (ni, ni+1)
      with Not_found -> None
    ) 0 in
    match idxs with
      | [] -> s
      | lst ->
        let rep = "\\x00" in
        let ls = String.length s in
        let r = Buffer.create (ls + List.length lst * (String.length rep - 1)) in
        let idx = List.fold_left (fun sidx i ->
          Buffer.add_substring r s sidx (i-sidx);
          Buffer.add_string r rep;
          i+1
        ) 0 lst in
        Buffer.add_substring r s idx (ls - idx);
        Buffer.contents r


  let rex_make s =
    Pcre.regexp (escape_null_bytes (Pcre.quote s))

  let rex_find rex haystack pos =
    (Pcre.pcre_exec ~rex ~pos haystack).(0)

  type engine_t = BM of BoyerMoore.t | RE of Pcre.regexp | Char of char | Null

  type t = int * string * engine_t

  let make needle =
    let nl = String.length needle in
    let et = match nl with
      | 0 -> Null
      | 1 -> Char needle.[0]
      | x when x < 5 -> RE (rex_make needle)
      | _ -> BM (BoyerMoore.make needle) in
    (nl, needle, et)
      

  let find (nl,needle,et) haystack start =
    let hl = String.length haystack in
    if nl = (hl-start) && endsWith needle haystack then
      start
    else if nl = 0 || nl > (hl-start) then
      raise Not_found
    else
      match et with
        | BM t -> BoyerMoore.find t haystack start
        | RE t -> rex_find t haystack start
        | Char t -> char_find t haystack start
        | Null -> raise Not_found
end
(**T
  StringSearch.find (StringSearch.make "foo") "foobarfoofoo" 0 = 0
  StringSearch.find (StringSearch.make "bar") "foobarfoofoo" 0 = 3
  StringSearch.find (StringSearch.make "foo") "foobarfoofoo" 3 = 6
  StringSearch.find (StringSearch.make "foo") "foobarfoofoo" 9 = 9
  optNF (StringSearch.find (StringSearch.make "foo") "foobarfoofoo") 10 = None
  optNF (StringSearch.find (StringSearch.make "foo") "foobarfoofoo") 12 = None
  optNF (StringSearch.find (StringSearch.make "fooo") "foobarfoofoo") 0 = None
  optNF (StringSearch.find (StringSearch.make "") "foobarfoofoo") 0 = None
  StringSearch.find (StringSearch.make "") "" 0 = 0
**)
(**Q
  Q.string (fun s -> StringSearch.find (StringSearch.make (ssub (Random.int(max 1 (slen s))) (Random.int(max 1 (slen s))+1) s)) s 0 >= 0)
**)


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

  (* Predicates *)

  let all p a = None = optNF (find (fun i -> not (p i))) a
  (**T
    sall (gt (chr 5)) (1--^|10) = false
    sall (lt (chr 11)) (1--^|10) = true
    sall (gt (chr 4)) "" = true
  **)
  let any p a = None <> optNF (find p) a
  (**T
    sany (gt (chr 5)) (1--^|10) = true
    sany (gt (chr 11)) (1--^|10) = false
    sany (gt (chr 4)) "" = false
  **)

  let allEqual l =
    if len l = 0 then true
    else let h = unsafe_get l 0 in all (fun i -> h = i) l
  (**T
    sallEqual (1--^|10) = false
    sallEqual "" = true
    sallEqual (sreplicate 10 'a') = true
  **)

  let includes x s = None <> optNF (indexOf x) s
  (**T
    sincludes (chr 4) (1--^|10) = true
    sincludes (chr 0) (1--^|10) = false
    sincludes (chr 5) "" = false
  **)
  let has = includes
  (**T
    shas (chr 1) (1--^|10) = true
    shas (chr 0) (1--^|10) = false
  **)
  let elem = includes
  (**T
    selem 'b' "foo" = false
    selem 'b' "foobar" = true
  **)
  let notElem x lst = not @@ elem x lst
  (**T
    snotElem (chr 4) (1--^|10) = false
    snotElem (chr 0) (1--^|10) = true
    snotElem (chr 5) "" = true
  **)

  let null = function "" -> true | _ -> false
  (**T
    snull "" = true
    snull (1--^|10) = false
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
  (**T
    smaximumByWith (square @. subtract 3 @. ord) (0 --^| 5) = (chr 0, 9)
    smaximumByWith (square @. subtract 2 @. ord) (0 --^| 5) = (chr 5, 9)
    optNF (smaximumByWith (square @. ord)) "" = None
  **)
  let minimumByWith f lst = PreArray.minimumBy snd (mapToArray (fupler f) lst)
  (**T
    sminimumByWith (square @. subtract 3 @. ord) (0 --^| 5) = (chr 3, 0)
    sminimumByWith (square @. subtract 3 @. ord) (0 --^| 1) = (chr 1, 4)
    optNF (sminimumByWith (square @. ord)) "" = None
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
  (**T
    mapWith (siterSub 0 10) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSub 2 6) id (1--^|10) = ((chr 3)-~(chr 8))
    mapWith (siterSub (-2) 10) id (1--^|10) = ((chr 9)-~(chr 10))
    mapWith (siterSub (-18) 10) id (1--^|10) = [chr 1; chr 2]
    mapWith (siterSub (-10) 10) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSub (-12) 1) id (1--^|10) = []
    mapWith (siterSub 9 10) id (1--^|10) = [chr 10]
    mapWith (siterSub (-19) 10) id (1--^|10) = [chr 1]
    mapWith (siterSub (-20) 10) id (1--^|10) = []
    mapWith (siterSub (-20) 20) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSub (-20) 50) id "" = []
    mapWith (siterSub (-20) 50) id "\001" = [chr 1]
  **)

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s
  (**T
    mapWith (siterSlice 0 9) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSlice 2 6) id (1--^|10) = ((chr 3)-~(chr 7))
    mapWith (siterSlice 6 2) id (1--^|10) = []
    mapWith (siterSlice 0 0) id (1--^|10) = [chr 1]
    mapWith (siterSlice 1 0) id (1--^|10) = []
    mapWith (siterSlice 9 9) id (1--^|10) = [chr 10]
    mapWith (siterSlice 9 8) id (1--^|10) = []
    mapWith (siterSlice (-2) (-1)) id (1--^|10) = [chr 9; chr 10]
    mapWith (siterSlice (-12) 0) id (1--^|10) = [chr 1]
    mapWith (siterSlice (-12) (-1)) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSlice (-12) (-11)) id (1--^|10) = []
    mapWith (siterSlice (-5) (-1)) id (1--^|10) = ((chr 6)-~(chr 10))
    mapWith (siterSlice (-20) 20) id (1--^|10) = ((chr 1)-~(chr 10))
    mapWith (siterSlice (-20) 50) id "" = []
    mapWith (siterSlice (-20) 50) id "\001" = [chr 1]
  **)

  let mapSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun j -> f (unsafe_get s (first+j))) sub_len
  (**T
    smapSub 0 10 succChar (1--^|10) = (2--^|11)
    smapSub 2 6 id (1--^|10) = (3--^|8)
    smapSub (-2) 10 id (1--^|10) = (9--^|10)
    smapSub (-18) 10 id (1--^|10) = "\001\002"
    smapSub (-10) 10 id (1--^|10) = (1--^|10)
    smapSub (-12) 1 id (1--^|10) = ""
    smapSub 9 10 id (1--^|10) = "\010"
    smapSub (-19) 10 id (1--^|10) = "\001"
    smapSub (-20) 10 id (1--^|10) = ""
    smapSub (-20) 20 id (1--^|10) = (1--^|10)
    smapSub (-20) 50 id "" = ""
    smapSub (-20) 50 id "\001" = "\001"
  **)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s
  (**T
    smapSlice 0 9 id (1--^|10) = (1--^|10)
    smapSlice 2 6 id (1--^|10) = (3--^|7)
    smapSlice 6 2 id (1--^|10) = ""
    smapSlice 0 0 id (1--^|10) = "\001"
    smapSlice 1 0 id (1--^|10) = ""
    smapSlice 9 9 id (1--^|10) = "\010"
    smapSlice 9 8 id (1--^|10) = ""
    smapSlice (-2) (-1) id (1--^|10) = "\009\010"
    smapSlice (-12) 0 id (1--^|10) = "\001"
    smapSlice (-12) (-1) id (1--^|10) = (1--^|10)
    smapSlice (-12) (-11) id (1--^|10) = ""
    smapSlice (-5) (-1) id (1--^|10) = (6--^|10)
    smapSlice (-20) 20 id (1--^|10) = (1--^|10)
    smapSlice (-20) 50 id "" = ""
    smapSlice (-20) 50 id "\001" = "\001"
  **)

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    sfoldlSub 0 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSub (-10) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSub (-20) 20 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSub 0 3 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|3)
    sfoldlSub 3 3 (+^) '\000' (1--^|10) = chr @@ ssum (4--^|6)
    sfoldlSub (-3) 3 (+^) '\000' (1--^|10) = chr @@ ssum (8--^|10)
    sfoldlSub (-1) 3 (+^) '\000' (1--^|10) = chr @@ ssum (10--^|10)
    sfoldlSub (-3) 1 (+^) '\000' (1--^|10) = chr @@ ssum (8--^|8)
    sfoldlSub 20 (-20) (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSub (-20) 10 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSub 10 0 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSub 3 (-1) (+^) '\000' (1--^|10) = chr @@ ssum ""

    sfoldlSub 0 1 (+^) '\000' (1--^|1) = chr @@ ssum (1--^|1)
    sfoldlSub 0 1 (+^) '\000' "" = chr @@ ssum ""
  **)

  let foldl1Sub i len f s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    sfoldl1Sub 0 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Sub (-10) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Sub (-20) 20 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Sub 0 3 (+^) (1--^|10) = chr @@ ssum (1--^|3)
    sfoldl1Sub 3 3 (+^) (1--^|10) = chr @@ ssum (4--^|6)
    sfoldl1Sub (-3) 3 (+^) (1--^|10) = chr @@ ssum (8--^|10)
    sfoldl1Sub (-1) 3 (+^) (1--^|10) = chr @@ ssum (10--^|10)
    sfoldl1Sub (-3) 1 (+^) (1--^|10) = chr @@ ssum (8--^|8)
    optNF (sfoldl1Sub 20 (-20) (+^)) (1--^|10) = None
    optNF (sfoldl1Sub (-20) 10 (+^)) (1--^|10) = None
    optNF (sfoldl1Sub 10 0 (+^)) (1--^|10) = None
    optNF (sfoldl1Sub 3 (-1) (+^)) (1--^|10) = None

    sfoldl1Sub 0 1 (+^) (1--^|1) = chr @@ ssum (1--^|1)
    optNF (sfoldl1Sub 0 1 (+^)) "" = None
  **)

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    sfoldrSub 0 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSub (-10) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSub (-20) 20 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSub 0 3 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|3)
    sfoldrSub 3 3 (+^) '\000' (1--^|10) = chr @@ ssum (4--^|6)
    sfoldrSub (-3) 3 (+^) '\000' (1--^|10) = chr @@ ssum (8--^|10)
    sfoldrSub (-1) 3 (+^) '\000' (1--^|10) = chr @@ ssum (10--^|10)
    sfoldrSub (-3) 1 (+^) '\000' (1--^|10) = chr @@ ssum (8--^|8)
    sfoldrSub 20 (-20) (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSub (-20) 10 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSub 10 0 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSub 3 (-1) (+^) '\000' (1--^|10) = chr @@ ssum ""

    sfoldrSub 0 1 (+^) '\000' (1--^|1) = chr @@ ssum (1--^|1)
    sfoldrSub 0 1 (+^) '\000' "" = chr @@ ssum ""
  **)

  let foldr1Sub i len f s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    sfoldr1Sub 0 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Sub (-10) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Sub (-20) 20 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Sub 0 3 (+^) (1--^|10) = chr @@ ssum (1--^|3)
    sfoldr1Sub 3 3 (+^) (1--^|10) = chr @@ ssum (4--^|6)
    sfoldr1Sub (-3) 3 (+^) (1--^|10) = chr @@ ssum (8--^|10)
    sfoldr1Sub (-1) 3 (+^) (1--^|10) = chr @@ ssum (10--^|10)
    sfoldr1Sub (-3) 1 (+^) (1--^|10) = chr @@ ssum (8--^|8)
    optNF (sfoldr1Sub 20 (-20) (+^)) (1--^|10) = None
    optNF (sfoldr1Sub (-20) 10 (+^)) (1--^|10) = None
    optNF (sfoldr1Sub 10 0 (+^)) (1--^|10) = None
    optNF (sfoldr1Sub 3 (-1) (+^)) (1--^|10) = None

    sfoldr1Sub 0 1 (+^) (1--^|1) = chr @@ ssum (1--^|1)
    optNF (sfoldr1Sub 0 1 (+^)) "" = None
  **)

  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s
  (**T
    sfoldlSlice 0 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice 0 9 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice 0 (-1) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice (-10) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice (-20) 20 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice (-20) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldlSlice 0 3 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|4)
    sfoldlSlice 3 (-1) (+^) '\000' (1--^|10) = chr @@ ssum (4--^|10)
    sfoldlSlice 3 3 (+^) '\000' (1--^|10) = chr @@ ssum (4--^|4)
    sfoldlSlice (-1) (-1) (+^) '\000' (1--^|10) = chr @@ ssum (10--^|10)
    sfoldlSlice (-3) 3 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSlice (-3) 1 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSlice 20 (-20) (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldlSlice 10 0 (+^) '\000' (1--^|10) = chr @@ ssum ""

    sfoldlSlice 0 1 (+^) '\000' (1--^|1) = chr @@ ssum (1--^|1)
    sfoldlSlice 0 1 (+^) '\000' "" = chr @@ ssum ""
  **)

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s
  (**T
    sfoldl1Slice 0 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice 0 9 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice 0 (-1) (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice (-10) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice (-20) 20 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice (-20) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldl1Slice 0 3 (+^) (1--^|10) = chr @@ ssum (1--^|4)
    sfoldl1Slice 3 (-1) (+^) (1--^|10) = chr @@ ssum (4--^|10)
    sfoldl1Slice 3 3 (+^) (1--^|10) = chr @@ ssum (4--^|4)
    sfoldl1Slice (-1) (-1) (+^) (1--^|10) = chr @@ ssum (10--^|10)
    optNF (sfoldl1Slice (-3) 3 (+^)) (1--^|10) = None
    optNF (sfoldl1Slice (-3) 1 (+^)) (1--^|10) = None
    optNF (sfoldl1Slice 20 (-20) (+^)) (1--^|10) = None
    optNF (sfoldl1Slice 10 0 (+^)) (1--^|10) = None

    sfoldl1Slice 0 1 (+^) (1--^|1) = chr @@ ssum (1--^|1)
    optNF (sfoldl1Slice 0 1 (+^)) "" = None
  **)

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s
  (**T
    sfoldrSlice 0 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice 0 9 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice 0 (-1) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice (-10) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice (-20) 20 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice (-20) 10 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    sfoldrSlice 0 3 (+^) '\000' (1--^|10) = chr @@ ssum (1--^|4)
    sfoldrSlice 3 (-1) (+^) '\000' (1--^|10) = chr @@ ssum (4--^|10)
    sfoldrSlice 3 3 (+^) '\000' (1--^|10) = chr @@ ssum (4--^|4)
    sfoldrSlice (-1) (-1) (+^) '\000' (1--^|10) = chr @@ ssum (10--^|10)
    sfoldrSlice (-3) 3 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSlice (-3) 1 (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSlice 20 (-20) (+^) '\000' (1--^|10) = chr @@ ssum ""
    sfoldrSlice 10 0 (+^) '\000' (1--^|10) = chr @@ ssum ""

    sfoldrSlice 0 1 (+^) '\000' (1--^|1) = chr @@ ssum (1--^|1)
    sfoldrSlice 0 1 (+^) '\000' "" = chr @@ ssum ""
  **)

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s
  (**T
    sfoldr1Slice 0 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice 0 9 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice 0 (-1) (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice (-10) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice (-20) 20 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice (-20) 10 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    sfoldr1Slice 0 3 (+^) (1--^|10) = chr @@ ssum (1--^|4)
    sfoldr1Slice 3 (-1) (+^) (1--^|10) = chr @@ ssum (4--^|10)
    sfoldr1Slice 3 3 (+^) (1--^|10) = chr @@ ssum (4--^|4)
    sfoldr1Slice (-1) (-1) (+^) (1--^|10) = chr @@ ssum (10--^|10)
    optNF (sfoldr1Slice (-3) 3 (+^)) (1--^|10) = None
    optNF (sfoldr1Slice (-3) 1 (+^)) (1--^|10) = None
    optNF (sfoldr1Slice 20 (-20) (+^)) (1--^|10) = None
    optNF (sfoldr1Slice 10 0 (+^)) (1--^|10) = None

    sfoldr1Slice 0 1 (+^) (1--^|1) = chr @@ ssum (1--^|1)
    optNF (sfoldr1Slice 0 1 (+^)) "" = None
  **)

  let add_int i c = i + ord c
  let add_float i c = i +. float (ord c)
  let mul_int i c = i * ord c
  let mul_float i c = i *. float (ord c)

  let sum a = foldl add_int 0 a
  (**T
    ssum (1--^|10) = 55
    ssum "\001\002" = 3
    ssum "\001" = 1
    ssum "\000" = 0
    ssum "" = 0
  **)
  let sumf a = foldl add_float 0. a
  (**T
    ssumf (1--^|10) = 55.
    ssumf "\001\002" = 3.
    ssumf "\001" = 1.
    ssumf "\000" = 0.
    ssumf "" = 0.
  **)
  let product a = foldl mul_int 1 a
  (**T
    sproduct (1--^|10) = 3628800
    sproduct "\001\002" = 2
    sproduct "\001" = 1
    sproduct "\000" = 0
    sproduct "" = 1
  **)
  let productf a = foldl mul_float 1. a
  (**T
    sproductf (1--^|10) = 3628800.
    sproductf "\001\002" = 2.
    sproductf "\001" = 1.
    sproductf "\000" = 0.
    sproductf "" = 1.
  **)
  let average a = sum a / len a
  (**T
    saverage (1--^|10) = 5
    saverage "\001" = 1
    optEx Division_by_zero saverage "" = None
  **)
  let averagef a = sumf a /. float (len a)
  (**T
    saveragef (1--^|10) = 5.5
    saveragef "\001" = 1.
    isNaN (saveragef "")
  **)

  let sumSub i len a = foldlSub i len add_int 0 a
  (**T
    ssumSub 0 10 (1--^|10) = ssum (1--^|10)
    ssumSub (-10) 10 (1--^|10) = ssum (1--^|10)
    ssumSub (-20) 20 (1--^|10) = ssum (1--^|10)
    ssumSub 0 3 (1--^|10) = ssum (1--^|3)
    ssumSub 3 3 (1--^|10) = ssum (4--^|6)
    ssumSub (-3) 3 (1--^|10) = ssum (8--^|10)
    ssumSub (-1) 3 (1--^|10) = ssum (10--^|10)
    ssumSub (-3) 1 (1--^|10) = ssum (8--^|8)
    ssumSub 20 (-20) (1--^|10) = ssum ""
    ssumSub (-20) 10 (1--^|10) = ssum ""
    ssumSub 10 0 (1--^|10) = ssum ""
    ssumSub 3 (-1) (1--^|10) = ssum ""

    ssumSub 0 1 (1--^|1) = ssum (1--^|1)
    ssumSub 0 1 "" = ssum ""
  **)
  let sumSubf i len a = foldlSub i len add_float 0. a
  (**T
    ssumSubf 0 10 (1--^|10) = ssumf (1--^|10)
    ssumSubf (-10) 10 (1--^|10) = ssumf (1--^|10)
    ssumSubf (-20) 20 (1--^|10) = ssumf (1--^|10)
    ssumSubf 0 3 (1--^|10) = ssumf (1--^|3)
    ssumSubf 3 3 (1--^|10) = ssumf (4--^|6)
    ssumSubf (-3) 3 (1--^|10) = ssumf (8--^|10)
    ssumSubf (-1) 3 (1--^|10) = ssumf (10--^|10)
    ssumSubf (-3) 1 (1--^|10) = ssumf (8--^|8)
    ssumSubf 20 (-20) (1--^|10) = ssumf ""
    ssumSubf (-20) 10 (1--^|10) = ssumf ""
    ssumSubf 10 0 (1--^|10) = ssumf ""
    ssumSubf 3 (-1) (1--^|10) = ssumf ""

    ssumSubf 0 1 (1--^|1) = ssumf (1--^|1)
    ssumSubf 0 1 "" = ssumf ""
  **)

  let sumSlice i j a = foldlSlice i j add_int 0 a
  (**T
    ssumSlice 0 10 (1--^|10) = ssum (1--^|10)
    ssumSlice 0 9 (1--^|10) = ssum (1--^|10)
    ssumSlice 0 (-1) (1--^|10) = ssum (1--^|10)
    ssumSlice (-10) 10 (1--^|10) = ssum (1--^|10)
    ssumSlice (-20) 20 (1--^|10) = ssum (1--^|10)
    ssumSlice (-20) 10 (1--^|10) = ssum (1--^|10)
    ssumSlice 0 3 (1--^|10) = ssum (1--^|4)
    ssumSlice 3 (-1) (1--^|10) = ssum (4--^|10)
    ssumSlice 3 3 (1--^|10) = ssum (4--^|4)
    ssumSlice (-1) (-1) (1--^|10) = ssum (10--^|10)
    ssumSlice (-3) 3 (1--^|10) = ssum ""
    ssumSlice (-3) 1 (1--^|10) = ssum ""
    ssumSlice 20 (-20) (1--^|10) = ssum ""
    ssumSlice 10 0 (1--^|10) = ssum ""

    ssumSlice 0 1 (1--^|1) = ssum (1--^|1)
    ssumSlice 0 1 "" = ssum ""
  **)
  let sumSlicef i j a = foldlSlice i j add_float 0. a
  (**T
    ssumSlicef 0 10 (1--^|10) = ssumf (1--^|10)
    ssumSlicef 0 9 (1--^|10) = ssumf (1--^|10)
    ssumSlicef 0 (-1) (1--^|10) = ssumf (1--^|10)
    ssumSlicef (-10) 10 (1--^|10) = ssumf (1--^|10)
    ssumSlicef (-20) 20 (1--^|10) = ssumf (1--^|10)
    ssumSlicef (-20) 10 (1--^|10) = ssumf (1--^|10)
    ssumSlicef 0 3 (1--^|10) = ssumf (1--^|4)
    ssumSlicef 3 (-1) (1--^|10) = ssumf (4--^|10)
    ssumSlicef 3 3 (1--^|10) = ssumf (4--^|4)
    ssumSlicef (-1) (-1) (1--^|10) = ssumf (10--^|10)
    ssumSlicef (-3) 3 (1--^|10) = ssumf ""
    ssumSlicef (-3) 1 (1--^|10) = ssumf ""
    ssumSlicef 20 (-20) (1--^|10) = ssumf ""
    ssumSlicef 10 0 (1--^|10) = ssumf ""

    ssumSlicef 0 1 (1--^|1) = ssumf (1--^|1)
    ssumSlicef 0 1 "" = ssumf ""
  **)

  let productSub i len a = foldlSub i len mul_int 1 a
  (**T
    sproductSub 0 10 (1--^|10) = sproduct (1--^|10)
    sproductSub (-10) 10 (1--^|10) = sproduct (1--^|10)
    sproductSub (-20) 20 (1--^|10) = sproduct (1--^|10)
    sproductSub 0 3 (1--^|10) = sproduct (1--^|3)
    sproductSub 3 3 (1--^|10) = sproduct (4--^|6)
    sproductSub (-3) 3 (1--^|10) = sproduct (8--^|10)
    sproductSub (-1) 3 (1--^|10) = sproduct (10--^|10)
    sproductSub (-3) 1 (1--^|10) = sproduct (8--^|8)
    sproductSub 20 (-20) (1--^|10) = sproduct ""
    sproductSub (-20) 10 (1--^|10) = sproduct ""
    sproductSub 10 0 (1--^|10) = sproduct ""
    sproductSub 3 (-1) (1--^|10) = sproduct ""

    sproductSub 0 1 (1--^|1) = sproduct (1--^|1)
    sproductSub 0 1 "" = sproduct ""
  **)
  let productSubf i len a = foldlSub i len mul_float 1. a
  (**T
    sproductSubf 0 10 (1--^|10) = sproductf (1--^|10)
    sproductSubf (-10) 10 (1--^|10) = sproductf (1--^|10)
    sproductSubf (-20) 20 (1--^|10) = sproductf (1--^|10)
    sproductSubf 0 3 (1--^|10) = sproductf (1--^|3)
    sproductSubf 3 3 (1--^|10) = sproductf (4--^|6)
    sproductSubf (-3) 3 (1--^|10) = sproductf (8--^|10)
    sproductSubf (-1) 3 (1--^|10) = sproductf (10--^|10)
    sproductSubf (-3) 1 (1--^|10) = sproductf (8--^|8)
    sproductSubf 20 (-20) (1--^|10) = sproductf ""
    sproductSubf (-20) 10 (1--^|10) = sproductf ""
    sproductSubf 10 0 (1--^|10) = sproductf ""
    sproductSubf 3 (-1) (1--^|10) = sproductf ""

    sproductSubf 0 1 (1--^|1) = sproductf (1--^|1)
    sproductSubf 0 1 "" = sproductf ""
  **)

  let productSlice i j a = foldlSlice i j mul_int 1 a
  (**T
    sproductSlice 0 10 (1--^|10) = sproduct (1--^|10)
    sproductSlice 0 9 (1--^|10) = sproduct (1--^|10)
    sproductSlice 0 (-1) (1--^|10) = sproduct (1--^|10)
    sproductSlice (-10) 10 (1--^|10) = sproduct (1--^|10)
    sproductSlice (-20) 20 (1--^|10) = sproduct (1--^|10)
    sproductSlice (-20) 10 (1--^|10) = sproduct (1--^|10)
    sproductSlice 0 3 (1--^|10) = sproduct (1--^|4)
    sproductSlice 3 (-1) (1--^|10) = sproduct (4--^|10)
    sproductSlice 3 3 (1--^|10) = sproduct (4--^|4)
    sproductSlice (-1) (-1) (1--^|10) = sproduct (10--^|10)
    sproductSlice (-3) 3 (1--^|10) = sproduct ""
    sproductSlice (-3) 1 (1--^|10) = sproduct ""
    sproductSlice 20 (-20) (1--^|10) = sproduct ""
    sproductSlice 10 0 (1--^|10) = sproduct ""

    sproductSlice 0 1 (1--^|1) = sproduct (1--^|1)
    sproductSlice 0 1 "" = sproduct ""
  **)
  let productSlicef i j a = foldlSlice i j mul_float 1. a
  (**T
    sproductSlicef 0 10 (1--^|10) = sproductf (1--^|10)
    sproductSlicef 0 9 (1--^|10) = sproductf (1--^|10)
    sproductSlicef 0 (-1) (1--^|10) = sproductf (1--^|10)
    sproductSlicef (-10) 10 (1--^|10) = sproductf (1--^|10)
    sproductSlicef (-20) 20 (1--^|10) = sproductf (1--^|10)
    sproductSlicef (-20) 10 (1--^|10) = sproductf (1--^|10)
    sproductSlicef 0 3 (1--^|10) = sproductf (1--^|4)
    sproductSlicef 3 (-1) (1--^|10) = sproductf (4--^|10)
    sproductSlicef 3 3 (1--^|10) = sproductf (4--^|4)
    sproductSlicef (-1) (-1) (1--^|10) = sproductf (10--^|10)
    sproductSlicef (-3) 3 (1--^|10) = sproductf ""
    sproductSlicef (-3) 1 (1--^|10) = sproductf ""
    sproductSlicef 20 (-20) (1--^|10) = sproductf ""
    sproductSlicef 10 0 (1--^|10) = sproductf ""

    sproductSlicef 0 1 (1--^|1) = sproductf (1--^|1)
    sproductSlicef 0 1 "" = sproductf ""
  **)

  let averageSub i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSub i len a / sub_len
  (**T
    saverageSub 0 10 (1--^|10) = saverage (1--^|10)
    saverageSub (-10) 10 (1--^|10) = saverage (1--^|10)
    saverageSub (-20) 20 (1--^|10) = saverage (1--^|10)
    saverageSub 0 3 (1--^|10) = saverage (1--^|3)
    saverageSub 3 3 (1--^|10) = saverage (4--^|6)
    saverageSub (-3) 3 (1--^|10) = saverage (8--^|10)
    saverageSub (-1) 3 (1--^|10) = saverage (10--^|10)
    saverageSub (-3) 1 (1--^|10) = saverage (8--^|8)
    optEx Division_by_zero (saverageSub 20 (-20)) (1--^|10) = None
    optEx Division_by_zero (saverageSub (-20) 10) (1--^|10) = None
    optEx Division_by_zero (saverageSub 10 0) (1--^|10) = None
    optEx Division_by_zero (saverageSub 3 (-1)) (1--^|10) = None

    saverageSub 0 1 (1--^|1) = saverage (1--^|1)
    optEx Division_by_zero (saverageSub 0 1) "" = None
  **)

  let averageSubf i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSubf i len a /. float sub_len
  (**T
    saverageSubf 0 10 (1--^|10) = saveragef (1--^|10)
    saverageSubf (-10) 10 (1--^|10) = saveragef (1--^|10)
    saverageSubf (-20) 20 (1--^|10) = saveragef (1--^|10)
    saverageSubf 0 3 (1--^|10) = saveragef (1--^|3)
    saverageSubf 3 3 (1--^|10) = saveragef (4--^|6)
    saverageSubf (-3) 3 (1--^|10) = saveragef (8--^|10)
    saverageSubf (-1) 3 (1--^|10) = saveragef (10--^|10)
    saverageSubf (-3) 1 (1--^|10) = saveragef (8--^|8)
    isNaN (saverageSubf 20 (-20) (1--^|10))
    isNaN  (saverageSubf (-20) 10 (1--^|10))
    isNaN  (saverageSubf 10 0 (1--^|10))
    isNaN  (saverageSubf 3 (-1) (1--^|10))

    saverageSubf 0 1 (1--^|1) = saveragef (1--^|1)
    isNaN  (saverageSubf 0 1 "")
  **)

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s
  (**T
    saverageSlice 0 10 (1--^|10) = saverage (1--^|10)
    saverageSlice 0 9 (1--^|10) = saverage (1--^|10)
    saverageSlice 0 (-1) (1--^|10) = saverage (1--^|10)
    saverageSlice (-10) 10 (1--^|10) = saverage (1--^|10)
    saverageSlice (-20) 20 (1--^|10) = saverage (1--^|10)
    saverageSlice (-20) 10 (1--^|10) = saverage (1--^|10)
    saverageSlice 0 3 (1--^|10) = saverage (1--^|4)
    saverageSlice 3 (-1) (1--^|10) = saverage (4--^|10)
    saverageSlice 3 3 (1--^|10) = saverage (4--^|4)
    saverageSlice (-1) (-1) (1--^|10) = saverage (10--^|10)
    optEx Division_by_zero (saverageSlice (-3) 3) (1--^|10) = None
    optEx Division_by_zero (saverageSlice (-3) 1) (1--^|10) = None
    optEx Division_by_zero (saverageSlice 20 (-20)) (1--^|10) = None
    optEx Division_by_zero (saverageSlice 10 0) (1--^|10) = None

    saverageSlice 0 1 (1--^|1) = saverage (1--^|1)
    optEx Division_by_zero (saverageSlice 0 1) "" = None
  **)

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s
  (**T
    saverageSlicef 0 10 (1--^|10) = saveragef (1--^|10)
    saverageSlicef 0 9 (1--^|10) = saveragef (1--^|10)
    saverageSlicef 0 (-1) (1--^|10) = saveragef (1--^|10)
    saverageSlicef (-10) 10 (1--^|10) = saveragef (1--^|10)
    saverageSlicef (-20) 20 (1--^|10) = saveragef (1--^|10)
    saverageSlicef (-20) 10 (1--^|10) = saveragef (1--^|10)
    saverageSlicef 0 3 (1--^|10) = saveragef (1--^|4)
    saverageSlicef 3 (-1) (1--^|10) = saveragef (4--^|10)
    saverageSlicef 3 3 (1--^|10) = saveragef (4--^|4)
    saverageSlicef (-1) (-1) (1--^|10) = saveragef (10--^|10)
    isNaN @@ saverageSlicef (-3) 3 (1--^|10)
    isNaN @@ saverageSlicef (-3) 1 (1--^|10)
    isNaN @@ saverageSlicef 20 (-20) (1--^|10)
    isNaN @@ saverageSlicef 10 0 (1--^|10)

    saverageSlicef 0 1 (1--^|1) = saveragef (1--^|1)
    isNaN @@ saverageSlicef 0 1 ""
  **)

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (fun i -> i >= l || i < 0) indices then raise Not_found;
    PreList.map (fun i -> unsafe_get s i) indices
  (**T
    spick [2; 3] ("foobar") = ['o'; 'b']
    spick [] ""= []
    spick [] (1--^|10) = []
    spick [0; 9] (1--^|10) = ['\001'; '\010']
    optNF (spick [2;3]) "123"= None
    optNF (spick [2;3]) ""= None
    optNF (spick [-2;3]) (1--^|10) = None
  **)

  let pickWith funcs s = PreList.map (fun f -> f s) funcs
  (**T
    spickWith [sfirst; slast] ("foobar") = ['f'; 'r']
  **)


  let count f s = foldl (fun s i -> s + if f i then 1 else 0) 0 s
  (**T
    scount ((=) '/') "" = 0
    scount ((=) '/') "/foooo/bar//baz" = 4
  **)

  (* String specific *)

  let findAllIndexes pat s =
    let t = StringSearch.make pat in
    let l = len pat in
    unfoldlOpt (fun i ->
      try let ni = StringSearch.find t s i in
          Some (ni, ni+l)
      with Not_found -> None
    ) 0
  (**T
    PreString.findAllIndexes "" "foo" = []
    PreString.findAllIndexes "foo" "foo" = [0]
    findAllIndexes "foo" "foobarfofoobarfoo" = [0; 8; 14]
  **)
  (**Q
    Q.printable_string (fun s -> all (lt (slen s)) (findAllIndexes "ax" s))
  **)

  let replace pat rep s =
    match pat with
      | "" when s = "" -> ""
      | "" -> concat rep (""::(PreList.map string_of_char @@ to_list s)@[""])
      | pat ->
        let plen = len pat in
        let indices = findAllIndexes pat s in
        match indices with
          | [] -> copy s
          | lst ->
            let r = Buffer.create (len s + List.length lst * (len rep - plen)) in
            let idx = PreList.foldl (fun sidx i ->
              Buffer.add_substring r s sidx (i-sidx);
              Buffer.add_string r rep;
              i+plen
            ) 0 lst in
            Buffer.add_substring r s idx (len s - idx); 
            Buffer.contents r
  (**T
    PreString.replace "foob" "nub" "foobar" = "nubar"
    PreString.replace "foo" "bar" "foo" = "bar"
    PreString.replace "" " " "foo" = " f o o "
    PreString.replace "" "foo" "" = ""
    PreString.replace "foo" "bar" "" = ""
    PreString.replace "foo" "" "" = ""
    PreString.replace "foo" "" "foobar" = "bar"
    PreString.replace "f.*b" "nub" "foobar" = "foobar"
    PreString.replace "foob" "n$0b" "foobar" = "n$0bar"

    replace "\000" "\001" "foo\000bar" = "foo\001bar"

    replace "#" " " "##foo###bar####" = join " " (split "#" "##foo###bar####")
  **)

  let rx ?study ?limit ?iflags ?flags ?chtables s =
    Pcre.regexp  ?study ?limit ?iflags ?flags ?chtables (replace "\000" "\\x00" s)
  (***
    ignore @@ rx "foo";
    ignore @@ rx ~study:true "foo";
    ignore @@ rx ~limit:4 ~flags:[`MULTILINE; `UTF8; `CASELESS] "foo"
  **)
  let rex = rx
  (***
    ignore @@ rex "foo";
    ignore @@ rex ~study:true "foo";
    ignore @@ rex ~limit:4 ~flags:[`MULTILINE; `UTF8; `CASELESS] "foo"
  **)
  let escape_rex = Pcre.quote
  (**T
    escape_rex ".[|]foo(+*?)" = "\\.\\[\\|]foo\\(\\+\\*\\?\\)"
    escape_rex "" = ""
  **)

  let strip = Pcre.replace ~rex:(Pcre.regexp "^\\s+|\\s+$") ~templ:""
  (**T
    strip "" = ""
    strip " " = ""
    strip "        " = ""
    strip "   foo   bar  " = "foo   bar"
    strip "\t\ndude\r\n\r\n" = "dude"
  **)

  let split' ?(delete_empty=false) ?n ?rex s =
    let rec aux res l = match l with
      | [] -> PreList.rev res
      (* trailing delim *)
      | ((Pcre.Text s) :: (Pcre.Delim _) :: []) -> aux (""::s::res) []
      (* trailing delim *)
      | ((Pcre.Delim _) :: (Pcre.Delim _) :: []) -> aux (""::""::res) []
      (* multiple delims *)
      | ((Pcre.Delim _) :: (Pcre.Delim d) :: t) -> aux (""::res) ((Pcre.Delim d) :: t)
      (* text *)
      | ((Pcre.Text s) :: t) -> aux (s::res) t
      (* delim *)
      | ((Pcre.Delim d) :: t) -> aux res t
      | (h::t) -> aux res t in
    let l = Pcre.full_split ?max:(optMap (max 1) n) ?rex s in
    let l = if delete_empty
      then PreList.filter (function Pcre.Text _ -> true | _ -> false) l
      else l in
    match l with
      | ((Pcre.Delim _) :: []) -> [""; ""]
      | ((Pcre.Delim _) :: (Pcre.Delim d) :: t) -> aux [""; ""] ((Pcre.Delim d) :: t)
      | ((Pcre.Delim _) :: t) -> aux [""] t
      | l -> aux [] l

  let rexsplit ?delete_empty ?n rex s = split' ?delete_empty ?n ~rex s
  (**T
    rexsplit (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rexsplit ~n:3 (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rexsplit ~n:2 (rex ",") "foo,bar,baz" = ["foo"; "bar,baz"]
    rexsplit ~n:1 (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexsplit ~n:0 (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexsplit ~n:(-1) (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexsplit ~n:min_int (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexsplit ~n:max_int (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]

    rexsplit (rex "#") "foo#bar#" = ["foo"; "bar"; ""]
    rexsplit (rex "[^a-z]") "foo###bar####" = ["foo"; ""; ""; "bar"; ""; ""; ""; ""]
    rexsplit (rex "[^a-z]+") "foo###bar####" = ["foo"; "bar"; ""]
    rexsplit (rex "#") "#foo#bar" = [""; "foo"; "bar"]
    rexsplit (rex "#") "#foo#bar#" = [""; "foo"; "bar"; ""]
    rexsplit (rex "#") "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    rexsplit ~delete_empty:true (rex "#") "##foo#bar##" = ["foo"; "bar"]
  **)
  let rexrsplit ?delete_empty ?n rex s = PreList.rev (
    PreList.map rev (rexsplit ?delete_empty ?n rex (rev s)))
  (**T
    rexrsplit (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rexrsplit ~n:3 (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rexrsplit ~n:2 (rex ",") "foo,bar,baz" = ["foo,bar"; "baz"]
    rexrsplit ~n:1 (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexrsplit ~n:0 (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexrsplit ~n:(-1) (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexrsplit ~n:min_int (rex ",") "foo,bar,baz" = ["foo,bar,baz"]
    rexrsplit ~n:max_int (rex ",") "foo,bar,baz" = ["foo"; "bar"; "baz"]

    rexrsplit (rex "#") "foo#bar#" = ["foo"; "bar"; ""]
    rexrsplit (rex "[^a-z]") "foo###bar####" = ["foo"; ""; ""; "bar"; ""; ""; ""; ""]
    rexrsplit (rex "[^a-z]+") "foo###bar####" = ["foo"; "bar"; ""]
    rexrsplit (rex "#") "#foo#bar" = [""; "foo"; "bar"]
    rexrsplit (rex "#") "#foo#bar#" = [""; "foo"; "bar"; ""]
    rexrsplit (rex "#") "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    rexrsplit ~delete_empty:true (rex "#") "##foo#bar##" = ["foo"; "bar"]
  **)

  let split ?delete_empty ?n pat s =
    split' ?delete_empty ?n ~rex:(rex (escape_rex pat)) s
  (**T
    split "" "" = []
    split "," "" = []
    split "" "foo" = [""; "f"; "o"; "o"; ""]
    split ~delete_empty:true "" "foo" = ["f"; "o"; "o"]

    split "foo" "foo" = [""; ""]

    split "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    split ~n:3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    split ~n:2 "," "foo,bar,baz" = ["foo"; "bar,baz"]
    split ~n:1 "," "foo,bar,baz" = ["foo,bar,baz"]
    split ~n:0 "," "foo,bar,baz" = ["foo,bar,baz"]
    split ~n:(-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    split ~n:min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    split ~n:max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]

    split "#" "foo#bar#" = ["foo"; "bar"; ""]
    split "#" "foo###bar##" = ["foo"; ""; ""; "bar"; ""; ""]
    split "#" "#foo#bar" = [""; "foo"; "bar"]
    split "#" "#foo#bar#" = [""; "foo"; "bar"; ""]
    split "#" "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    split ~delete_empty:true "#" "##foo##bar##" = ["foo"; "bar"]

    (let s = "###f####oo#ba##r###" in join "#" (split "#" s) = s)
  **)
  let rsplit ?delete_empty ?n sep s = PreList.rev (
    PreList.map rev (split ?delete_empty ?n sep (rev s)))
  (**T
    rsplit "" "" = []
    rsplit "," "" = []
    rsplit "" "foo" = [""; "f"; "o"; "o"; ""]
    rsplit ~delete_empty:true "" "foo" = ["f"; "o"; "o"]

    rsplit "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rsplit ~n:3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    rsplit ~n:2 "," "foo,bar,baz" = ["foo,bar"; "baz"]
    rsplit ~n:1 "," "foo,bar,baz" = ["foo,bar,baz"]
    rsplit ~n:0 "," "foo,bar,baz" = ["foo,bar,baz"]
    rsplit ~n:(-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    rsplit ~n:min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    rsplit ~n:max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]

    rsplit "#" "foo#bar#" = ["foo"; "bar"; ""]
    rsplit "#" "foo###bar##" = ["foo"; ""; ""; "bar"; ""; ""]
    rsplit "#" "#foo#bar" = [""; "foo"; "bar"]
    rsplit "#" "#foo#bar#" = [""; "foo"; "bar"; ""]
    rsplit "#" "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    rsplit ~delete_empty:true "#" "##foo##bar##" = ["foo"; "bar"]

    (let s = "###f####oo#ba##r###" in join "#" (rsplit "#" s) = s)
  **)
  let nsplit ?delete_empty n sep s = split ?delete_empty ~n sep s
  (**T
    nsplit 0 "" "" = []
    nsplit 1 "," "" = []
    nsplit 5 "" "foo" = [""; "f"; "o"; "o"; ""]
    nsplit ~delete_empty:true 3 "" "foo" = ["f"; "o"; "o"]
    nsplit ~delete_empty:true 2 "" "foo" = ["f"; "oo"]

    nsplit 3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    nsplit 2 "," "foo,bar,baz" = ["foo"; "bar,baz"]
    nsplit 1 "," "foo,bar,baz" = ["foo,bar,baz"]
    nsplit 0 "," "foo,bar,baz" = ["foo,bar,baz"]
    nsplit (-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    nsplit min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    nsplit max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
  **)
  let nrsplit ?delete_empty n sep s = rsplit ?delete_empty ~n sep s
  (**T
    nrsplit 0 "" "" = []
    nrsplit 1 "," "" = []
    nrsplit 5 "" "foo" = [""; "f"; "o"; "o"; ""]
    nrsplit ~delete_empty:true 3 "" "foo" = ["f"; "o"; "o"]
    nrsplit ~delete_empty:true 2 "" "foo" = ["fo"; "o"]

    nrsplit 3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    nrsplit 2 "," "foo,bar,baz" = ["foo,bar"; "baz"]
    nrsplit 1 "," "foo,bar,baz" = ["foo,bar,baz"]
    nrsplit 0 "," "foo,bar,baz" = ["foo,bar,baz"]
    nrsplit (-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    nrsplit min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    nrsplit max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
  **)

  let xsplit ?delete_empty ?n rexs s = rexsplit ?delete_empty ?n (rx rexs) s
  (**T
    xsplit "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xsplit ~n:3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xsplit ~n:2 "," "foo,bar,baz" = ["foo"; "bar,baz"]
    xsplit ~n:1 "," "foo,bar,baz" = ["foo,bar,baz"]
    xsplit ~n:0 "," "foo,bar,baz" = ["foo,bar,baz"]
    xsplit ~n:(-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    xsplit ~n:min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    xsplit ~n:max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]

    xsplit "#" "foo#bar#" = ["foo"; "bar"; ""]
    xsplit "[^a-z]" "foo###bar####" = ["foo"; ""; ""; "bar"; ""; ""; ""; ""]
    xsplit "[^a-z]+" "foo###bar####" = ["foo"; "bar"; ""]
    xsplit "#" "#foo#bar" = [""; "foo"; "bar"]
    xsplit "#" "#foo#bar#" = [""; "foo"; "bar"; ""]
    xsplit "#" "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    xsplit ~delete_empty:true "#" "##foo#bar##" = ["foo"; "bar"]
  **)
  let xrsplit ?delete_empty ?n rexs s = rexrsplit ?delete_empty ?n (rx rexs) s
  (**T
    xrsplit "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xrsplit ~n:3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xrsplit ~n:2 "," "foo,bar,baz" = ["foo,bar"; "baz"]
    xrsplit ~n:1 "," "foo,bar,baz" = ["foo,bar,baz"]
    xrsplit ~n:0 "," "foo,bar,baz" = ["foo,bar,baz"]
    xrsplit ~n:(-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    xrsplit ~n:min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    xrsplit ~n:max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]

    xrsplit "#" "foo#bar#" = ["foo"; "bar"; ""]
    xrsplit "[^a-z]" "foo###bar####" = ["foo"; ""; ""; "bar"; ""; ""; ""; ""]
    xrsplit "[^a-z]+" "foo###bar####" = ["foo"; "bar"; ""]
    xrsplit "#" "#foo#bar" = [""; "foo"; "bar"]
    xrsplit "#" "#foo#bar#" = [""; "foo"; "bar"; ""]
    xrsplit "#" "##foo#bar##" = [""; ""; "foo"; "bar"; ""; ""]

    xrsplit ~delete_empty:true "#" "##foo#bar##" = ["foo"; "bar"]
  **)
  
  let xnsplit ?delete_empty n rexs s = xsplit ?delete_empty ~n rexs s
  (**T
    xnsplit 0 "" "" = []
    xnsplit 1 "," "" = []
    xnsplit 5 "" "foo" = [""; "f"; "o"; "o"; ""]
    xnsplit ~delete_empty:true 3 "" "foo" = ["f"; "o"; "o"]
    xnsplit ~delete_empty:true 2 "" "foo" = ["f"; "oo"]

    xnsplit 3 "[^a-z]" "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xnsplit 2 "[^a-z]" "foo,bar,baz" = ["foo"; "bar,baz"]

    xnsplit 3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xnsplit 2 "," "foo,bar,baz" = ["foo"; "bar,baz"]
    xnsplit 1 "," "foo,bar,baz" = ["foo,bar,baz"]
    xnsplit 0 "," "foo,bar,baz" = ["foo,bar,baz"]
    xnsplit (-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    xnsplit min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    xnsplit max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
  **)
  let xnrsplit ?delete_empty n rexs s = xrsplit ?delete_empty  ~n rexs s
  (**T
    xnrsplit 0 "" "" = []
    xnrsplit 1 "," "" = []
    xnrsplit 5 "" "foo" = [""; "f"; "o"; "o"; ""]
    xnrsplit ~delete_empty:true 3 "" "foo" = ["f"; "o"; "o"]
    xnrsplit ~delete_empty:true 2 "" "foo" = ["fo"; "o"]

    xnrsplit 3 "[^a-z]" "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xnrsplit 2 "[^a-z]" "foo,bar,baz" = ["foo,bar"; "baz"]

    xnrsplit 3 "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
    xnrsplit 2 "," "foo,bar,baz" = ["foo,bar"; "baz"]
    xnrsplit 1 "," "foo,bar,baz" = ["foo,bar,baz"]
    xnrsplit 0 "," "foo,bar,baz" = ["foo,bar,baz"]
    xnrsplit (-1) "," "foo,bar,baz" = ["foo,bar,baz"]
    xnrsplit min_int "," "foo,bar,baz" = ["foo,bar,baz"]
    xnrsplit max_int "," "foo,bar,baz" = ["foo"; "bar"; "baz"]
  **)

  let rexscan rex s =
    try PreArray.to_list (Array.map PreArray.to_list (Pcre.extract_all ~rex s))
    with _ -> []
  (**T
    rexscan (rex ".(.)(.)") "foobar" = [["foo"; "o"; "o"]; ["bar";"a";"r"]]

    rexscan (rex "..") "foobar" = [["fo"]; ["ob"]; ["ar"]]
    rexscan (rex "..") "foo" = [["fo"]]
    rexscan (rex "..") "fo" = [["fo"]]
    rexscan (rex "..") "f" = []
    rexscan (rex "..") "" = []
    rexscan (rex "") "foo" = [[""]]
    rexscan (rex "") "" = [[""]]
    rexscan (rex "[0-9]+") "A 7 greetings from the 5th world of 159" = [["7"];["5"];["159"]]
  **)
  let scan rexs s = rexscan (rx rexs) s
  (**T
    scan ".(.)(.)" "foobar" = [["foo"; "o"; "o"]; ["bar";"a";"r"]]

    scan ".." "foobar" = [["fo"]; ["ob"]; ["ar"]]
    scan ".." "foo" = [["fo"]]
    scan ".." "fo" = [["fo"]]
    scan ".." "f" = []
    scan ".." "" = []
    scan "" "foo" = [[""]]
    scan "" "" = [[""]]
    scan "[0-9]+" "A 7 greetings from the 5th world of 159" = [["7"];["5"];["159"]]
  **)

  let rexscan_nth n rex s =
    try
      if n < 0 then raise Not_found;
      let arr = Pcre.extract_all ~rex s in
      list (Array.map (fun a ->
        if PreArray.length a <= n
        then raise Not_found;
        a.(n)
      ) arr)
    with _ -> []
  (**T
    rexscan_nth 0 (rex ".(.)(.)") "foobar" = ["foo"; "bar"]
    rexscan_nth 1 (rex ".(.)(.)") "foobar" = ["o"; "a"]
    rexscan_nth 2 (rex ".(.)(.)") "foobar" = ["o"; "r"]
    rexscan_nth 3 (rex ".(.)(.)") "foobar" = []
    rexscan_nth (-1) (rex ".(.)(.)") "foobar" = []

    rexscan_nth 0 (rex "..") "foobar" = ["fo"; "ob"; "ar"]
    rexscan_nth 0 (rex "..") "foo" = ["fo"]
    rexscan_nth 0 (rex "..") "fo" = ["fo"]
    rexscan_nth 0 (rex "..") "f" = []
    rexscan_nth 0 (rex "..") "" = []
    rexscan_nth 0 (rex "") "foo" = [""]
    rexscan_nth 0 (rex "") "" = [""]
    rexscan_nth 0 (rex "[0-9]+") "A 7 greetings from the 5th world of 159" = ["7";"5";"159"]
  **)
  let scan_nth n rexs s = rexscan_nth n (rx rexs) s
  (**T
    scan_nth 0 (".(.)(.)") "foobar" = ["foo"; "bar"]
    scan_nth 1 (".(.)(.)") "foobar" = ["o"; "a"]
    scan_nth 2 (".(.)(.)") "foobar" = ["o"; "r"]
    scan_nth 3 (".(.)(.)") "foobar" = []
    scan_nth (-1) (".(.)(.)") "foobar" = []

    scan_nth 0 ".." "foobar" = ["fo"; "ob"; "ar"]
    scan_nth 0 ".." "foo" = ["fo"]
    scan_nth 0 ".." "fo" = ["fo"]
    scan_nth 0 ".." "f" = []
    scan_nth 0 ".." "" = []
    scan_nth 0 "" "foo" = [""]
    scan_nth 0 "" "" = [""]
    scan_nth 0 "[0-9]+" "A 7 greetings from the 5th world of 159" = ["7";"5";"159"]
  **)

  let extract rex s = list (Pcre.extract ~rex s)
  (**T
    extract (rex "bo(.)([a-z])") "Look, a boomerang!" = ["boom"; "o"; "m"]
    extract (rex "") "Look, a boomerang!" = [""]
  **)
  let rexfind rex s = PreList.first (extract rex s)
  (**T
    rexfind (rex "bo.(.)") "Look, a boomerang!" = "boom"
    rexfind (rex "") "Look, a boomerang!" = ""
    optNF (rexfind (rex "go..")) "Look, a boomerang!" = None
    optNF (rexfind (rex "go..")) "" = None
    rexfind (rex "") "" = ""
  **)
  let rexfindOpt rex s = optNF (fun s -> PreList.first (extract rex s)) s
  (**T
    rexfindOpt (rex "bo.(.)") "Look, a boomerang!" = Some "boom"
    rexfindOpt (rex "") "Look, a boomerang!" = Some ""
    rexfindOpt (rex "go..") "Look, a boomerang!" = None
    rexfindOpt (rex "go..") "" = None
    rexfindOpt (rex "") "" = Some ""
  **)
  let xfind x s = rexfind (rex x) s
  (**T
    xfind ("bo.(.)") "Look, a boomerang!" = "boom"
    xfind ("") "Look, a boomerang!" = ""
    optNF (xfind ("go..")) "Look, a boomerang!" = None
    optNF (xfind ("go..")) "" = None
    xfind ("") "" = ""
  **)
  let xfindOpt x s = rexfindOpt (rex x) s
  (**T
    xfindOpt ("bo.(.)") "Look, a boomerang!" = Some "boom"
    xfindOpt ("") "Look, a boomerang!" = Some ""
    xfindOpt ("go..") "Look, a boomerang!" = None
    xfindOpt ("go..") "" = None
    xfindOpt ("") "" = Some ""
  **)

  let rexmatch rex = Pcre.pmatch ~rex
  (**T
    rexmatch (rex "") "" = true
    rexmatch (rex "") "foo" = true
    rexmatch (rex "foo") "" = false

    rexmatch (rex "foo") "foobar" = true
    rexmatch (rex "bar") "foobar" = true
    rexmatch (rex "oba") "foobar" = true

    rexmatch (rex "o*b") "foobar" = true
    rexmatch (rex "x*b") "foobar" = true
  **)
  let xmatch s = rexmatch (rx s)
  (**T
    xmatch "" "" = true
    xmatch "" "foo" = true
    xmatch "foo" "" = false

    xmatch "foo" "foobar" = true
    xmatch "bar" "foobar" = true
    xmatch "oba" "foobar" = true

    xmatch "o*b" "foobar" = true
    xmatch "x*b" "foobar" = true
  **)
  let smatch pat str = xmatch (escape_rex pat) str
  (**T
    smatch "" "" = true
    smatch "" "foo" = true
    smatch "foo" "" = false

    smatch "foo" "foobar" = true
    smatch "bar" "foobar" = true
    smatch "oba" "foobar" = true

    smatch "o*b" "foobar" = false
    smatch "x*b" "foobar" = false
  **)

  let xstartsWith prefix = rexmatch (rx ("^" ^ prefix))
  (**T
    xstartsWith "" "foo" = true
    xstartsWith "f" "foo" = true
    xstartsWith "f." "foo" = true
    xstartsWith "fo" "foo" = true
    xstartsWith "foo" "foo" = true
    xstartsWith ".*o" "foo" = true
    xstartsWith ".*o$" "foo" = true

    xstartsWith "" "" = true

    xstartsWith "foooo" "foo" = false
    xstartsWith "foooo" "" = false
    xstartsWith "f." "f" = false
  **)
  let startsWith prefix s =
    let rec aux s p pl i =
      if i >= pl then true
      else if unsafe_get s i = unsafe_get p i
      then aux s p pl (i+1)
      else false in
    let sl = len s
    and pl = len prefix in
    if pl > sl then false
    else aux s prefix pl 0
  (**T
    startsWith "" "foo" = true
    startsWith "f" "foo" = true
    startsWith "f." "foo" = false
    startsWith "fo" "foo" = true
    startsWith "foo" "foo" = true
    startsWith ".*o" "foo" = false
    startsWith ".*o$" "foo" = false

    startsWith "" "" = true

    startsWith "foooo" "foo" = false
    startsWith "foooo" "" = false
    startsWith "f." "f" = false
  **)

  let xendsWith suffix = rexmatch (rx (suffix ^ "$"))
  (**T
    xendsWith "" "foo" = true
    xendsWith "o" "foo" = true
    xendsWith "o." "foo" = true
    xendsWith "oo" "foo" = true
    xendsWith "foo" "foo" = true
    xendsWith ".*o" "foo" = true
    xendsWith "^.*o" "foo" = true

    xendsWith "" "" = true

    xendsWith "foooo" "foo" = false
    xendsWith "foooo" "" = false
    xendsWith "f." "f" = false
  **)
  let endsWith suffix s =
    let rec aux s p pl sl i =
      if i >= pl then true
      else if unsafe_get s (sl-i-1) = unsafe_get p (pl-i-1)
      then aux s p pl sl (i+1)
      else false in
    let sl = len s
    and pl = len suffix in
    if pl > sl then false
    else aux s suffix pl sl 0
  (**T
    endsWith "" "foo" = true
    endsWith "o" "foo" = true
    endsWith "o." "foo" = false
    endsWith "oo" "foo" = true
    endsWith "foo" "foo" = true
    endsWith ".*o" "foo" = false
    endsWith "^.*o" "foo" = false

    endsWith "" "" = true

    endsWith "foooo" "foo" = false
    endsWith "foooo" "" = false
    endsWith "f." "f" = false
  **)

  let rexreplace rex templ s = Pcre.replace ~rex ~templ s
  (**T
    rexreplace (rex "foob") "nub" "foobar" = "nubar"
    rexreplace (rex "foo") "bar" "foo" = "bar"
    rexreplace (rex "") " " "foo" = " f o o "
    rexreplace (rex "") "foo" "" = "foo"
    rexreplace (rex "foo") "bar" "" = ""
    rexreplace (rex "foo") "" "" = ""
    rexreplace (rex "foo") "" "foobar" = "bar"
    rexreplace (rex "f.*b") "nub" "foobar" = "nubar"
    rexreplace (rex "f(.+)b") "n$1b" "foobar" = "noobar"
    rexreplace (rex "f(.+)b") "n$1b" "foobar" = "noobar"
    optE (rexreplace (rex "f(.+)b") "n$2b") "foobar" = None

    rexreplace (rex "#") " " "##foo###bar####" = join " " (split "#" "##foo###bar####")
  **)
  let xreplace x templ s = rexreplace (rx x) templ s
  (**T
    xreplace "foob" "nub" "foobar" = "nubar"
    xreplace "foo" "bar" "foo" = "bar"
    xreplace "" " " "foo" = " f o o "
    xreplace "" "foo" "" = "foo"
    xreplace "foo" "bar" "" = ""
    xreplace "foo" "" "" = ""
    xreplace "foo" "" "foobar" = "bar"
    xreplace "f.*b" "nub" "foobar" = "nubar"
    xreplace "f(.+)b" "n$&b" "foobar" = "nfoobbar"
    xreplace "f(.+)b" "n$1b" "foobar" = "noobar"
    xreplace "f(.+)b" "n$1b" "foobar" = "noobar"
    optE (xreplace "f(.+)b" "n$2b") "foobar" = None

    xreplace "#" " " "##foo###bar####" = join " " (split "#" "##foo###bar####")
  **)

  let frexreplace f rex s =
    let split = Pcre.full_split ~rex s in
    let processed = PreList.map (function
      | Pcre.Text s -> s
      | Pcre.Delim s -> f s
      | _ -> "") split in
    String.concat "" processed
  (**T
    frexreplace (fun s -> if s.[0] = 'f' then "Go" else "!") (rex ".oo") "foohoohoo" = "Go!!"
    frexreplace (fun s -> if s.[0] = 'f' then "G" else "") (rex ".oo") "" = ""
    frexreplace (fun s -> "G") (rex "") "..." = "G.G.G.G"
    frexreplace (fun s -> "G") (rex "") "" = ""
    frexreplace (fun s -> srev s) (rex "...") "" = ""
    frexreplace (fun s -> srev s) (rex "...") "foobar" = "oofrab"
  **)
  let fxreplace f s = frexreplace f (rx s)
  (**T
    fxreplace (fun s -> if s.[0] = 'f' then "Go" else "!") ( ".oo") "foohoohoo" = "Go!!"
    fxreplace (fun s -> if s.[0] = 'f' then "G" else "") ( ".oo") "" = ""
    fxreplace (fun s -> "G") ( "") "..." = "G.G.G.G"
    fxreplace (fun s -> "G") ( "") "" = ""
    fxreplace (fun s -> srev s) ( "...") "" = ""
    fxreplace (fun s -> srev s) ( "...") "foobar" = "oofrab"
  **)

  let quote l r s = concat "" [l; s; r]
  (**T
    quote "`" "'" "foo" = "`foo'"
    quote "`" "'" "" = "`'"
    quote "" "" "" = ""
    quote "a" "" "" = "a"
    quote "" "b" "" = "b"
    quote "" "" "c" = "c"
    quote "a" "b" "c" = "acb"
  **)

  let join = String.concat
  (**T
    join ", " ["1"; "2"; "3"] = "1, 2, 3"
    join ", " ["1"] = "1"
    join ", " [] = ""
  **)
  let joinArray s a = join s (Array.to_list a)
  (**T
    joinArray ", " [|"1"; "2"; "3"|] = "1, 2, 3"
    joinArray ", " [|"1"|] = "1"
    joinArray ", " [||] = ""
  **)

  let xreplaceMulti x_rep s =
    let pat = x_rep |> PreList.map (fun (k,v) -> quote "(" ")" k) |> join "|" in
    frexreplace (fun p ->
      let k,v = PreList.find (fun (x,_) -> xmatch x p) x_rep in
      xreplace k v p
    ) (rex pat) s
  (**T
    xreplaceMulti ["f.o","bar"; "b.r","foo"] "foobar" = "barfoo"
    xreplaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "barfoo"
    xreplaceMulti ["f.o","bar"; "bar","foo"] "" = ""
    xreplaceMulti ["f(o+)","b$1r"; "bar","foo"] "foo" = "boor"
    xreplaceMulti ["","bar"; "bar","foo"] "" = ""
    xreplaceMulti ["f","-"; "o","+"; ""," "] "foo" = "-++ "
    xreplaceMulti [""," "; "f","-"; "o","+"] "foo" = " f   o   o  "
  **)

  let replaceMulti pat_rep s =
    let pat = pat_rep |> PreList.map fst |> PreList.map escape_rex |> join "|" in
    frexreplace (flip PreList.assoc pat_rep) (rex pat) s
  (**T
    replaceMulti ["foo","bar"; "bar","foo"] "foobar" = "barfoo"
    replaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "foofoo"
    replaceMulti ["f.o","bar"; "bar","foo"] "" = ""
    replaceMulti ["f.o","bar"; "bar","foo"] "f.o" = "bar"
    replaceMulti ["","bar"; "bar","foo"] "" = ""
    replaceMulti [""," "; "f","-"; "o","+"] "foo" = "- + + "
  **)

  let words s = rexsplit ~delete_empty:true (rx "\\s+") s
  (**T
    words "  foo\nbar baz\tqux  \n  bob  " = ["foo"; "bar"; "baz"; "qux"; "bob"]
    words "" = []
    words "1" = ["1"]
    words " " = []
  **)
  let unwords a = join " " a
  (**T
    unwords ["foo"; "bar"; "baz"; "qux"; "bob"] = "foo bar baz qux bob"
    unwords [] = ""
    unwords ["1"] = "1"
  **)

  let lines s = split "\n" s
  (**T
    lines "  foo\nbar\n\nbaz\tqux  \n  bob  " = ["  foo"; "bar"; ""; "baz\tqux  "; "  bob  "]
    lines "" = []
    lines "1" = ["1"]
    lines "\n" = ["";  ""]
  **)
  let unlines a = join "\n" a
  (**T
    unlines ["  foo"; "bar"; ""; "baz\tqux  "; "  bob  "] = "  foo\nbar\n\nbaz\tqux  \n  bob  "
    unlines [] = ""
    unlines ["1"] = "1"
    unlines ["";  ""] = "\n"
  **)

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
  (**T
    rexsplitPartition (rex "f.o") "foobarfnobu" = (["","foo" ; "bar","fno"], Some "bu")
    rexsplitPartition (rex "f.o") "foobarfno" = (["","foo" ; "bar","fno"], None)
    rexsplitPartition (rex "f") "bar" = ([], Some "bar")
    rexsplitPartition (rex "") "fo" = (["","" ; "f","" ; "o",""], None)
    rexsplitPartition (rex "") "" = ([], None)
  **)
  let xsplitPartition x s = rexsplitPartition (rex x) s
  (**T
    xsplitPartition "f.o" "foobarfnobu" = (["","foo" ; "bar","fno"], Some "bu")
    xsplitPartition "f.o" "foobarfno" = (["","foo" ; "bar","fno"], None)
    xsplitPartition "f" "bar" = ([], Some "bar")
    xsplitPartition "" "fo" = (["","" ; "f","" ; "o",""], None)
    xsplitPartition "" "" = ([], None)
  **)


  (* Parallel operations *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l |> par_map ~process_count process |> combine
  (**T
    PreString.par_mapReduce ~combine:(join "") ~process:(smap succChar) (1--^|10) = smap succChar (1--^|10)
    PreString.par_mapReduce ~process_count:2 ~combine:(join "") ~process:(smap succChar) (1--^|10) = smap succChar (1--^|10)
    PreString.par_mapReduce ~process_count:2 ~combine:(join "" @. reverse) ~process:(smap succChar) (1--^|10) = smap succChar ((6--^|10)^(1--^|5))
    PreString.par_mapReduce ~process_count:2 ~combine:(join "" @. reverse) ~process:(smap succChar) "" = ""
    PreString.par_mapReduce ~process_count:2 ~combine:(join "" @. reverse) ~process:(smap succChar) "1" = "2"
  **)

  let pmapReduce combine process = par_mapReduce ~combine ~process
  (**T
    PreString.pmapReduce (join "") (smap succChar) (1--^|10) = smap succChar (1--^|10)
    PreString.pmapReduce ~process_count:2 (join "") (smap succChar) (1--^|10) = smap succChar (1--^|10)
    PreString.pmapReduce ~process_count:2 (join "" @. reverse) (smap succChar) (1--^|10) = smap succChar ((6--^|10)^(1--^|5))
    PreString.pmapReduce ~process_count:2 (join "" @. reverse) (smap succChar) "" = ""
    PreString.pmapReduce ~process_count:2 (join "" @. reverse) (smap succChar) "1" = "2"
  **)
  
  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  (**T
    spfoldl (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl ~process_count:2 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl ~process_count:2 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldl ~process_count:1 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl ~process_count:1 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldl ~process_count:0 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl ~process_count:0 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldl ~process_count:3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl ~process_count:3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldl ~process_count:2 (multiply) (flip (multiply @. ord)) 1 (1--^|10) = sproduct (1--^|10)
    spfoldl ~process_count:2 (multiply) (flip (multiply @. ord)) 1 "1" = sproduct "1"
    optNF (spfoldl ~process_count:2 (+^) (+^) '\000') "" = Some (chr 0)
  **)

  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  (**T
    spfoldl1 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl1 ~process_count:3 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl1 ~process_count:2 (+^) "1" = chr @@ ssum "1"
    spfoldl1 ~process_count:1 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldl1 ~process_count:0 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    optNF (spfoldl1 ~process_count:2 (+^)) "" = None
  **)

  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  (**T
    spfoldr ~process_count:2 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr ~process_count:2 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldr (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldr ~process_count:1 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr ~process_count:1 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldr ~process_count:0 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr ~process_count:0 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldr ~process_count:3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr ~process_count:3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    optNF (spfoldr ~process_count:2 (+^) (+^) '\000') "" = Some (chr 0)
  **)

  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)
  (**T
    spfoldr1 ~process_count:3 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr1 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr1 ~process_count:2 (+^) "1" = chr @@ ssum "1"
    spfoldr1 ~process_count:1 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    spfoldr1 ~process_count:0 (+^) (1--^|10) = chr @@ ssum (1--^|10)
    optNF (spfoldr1 ~process_count:2 (+^)) "" = None
  **)

  let piter f = pmapReduce ignore (iter f)
  (**T
    spiter ~process_count:3 (ignore @. succChar) (1--^|10) = ()
    spiter ~process_count:2 (ignore @. succChar) (1--^|10) = ()
    spiter ~process_count:1 (ignore @. succChar) (1--^|10) = ()
    spiter ~process_count:0 (ignore @. succChar) (1--^|10) = ()
    spiter ~process_count:3 (ignore @. succChar) "1" = ()
    spiter ~process_count:2 (ignore @. succChar) "1" = ()
    spiter ~process_count:1 (ignore @. succChar) "1" = ()
    spiter ~process_count:0 (ignore @. succChar) "1" = ()
    spiter ~process_count:3 (ignore @. succChar) "" = ()
    spiter ~process_count:2 (ignore @. succChar) "" = ()
    spiter ~process_count:1 (ignore @. succChar) "" = ()
    spiter ~process_count:0 (ignore @. succChar) "" = ()
    spiter (ignore @. succChar) "" = ()
    spiter (ignore @. succChar) (1--^|10) = ()
    spiter (ignore @. succChar) "1" = ()
  **)

  let pmap f = pmapReduce (concat "") (map f)
  (**T
    spmap ~process_count:3 succChar (1--^|10) = smap succChar (1--^|10)
    spmap ~process_count:2 succChar (1--^|10) = smap succChar (1--^|10)
    spmap ~process_count:1 succChar (1--^|10) = smap succChar (1--^|10)
    spmap ~process_count:0 succChar (1--^|10) = smap succChar (1--^|10)
    spmap ~process_count:3 succChar "1" = smap succChar "1"
    spmap ~process_count:2 succChar "1" = smap succChar "1"
    spmap ~process_count:1 succChar "1" = smap succChar "1"
    spmap ~process_count:0 succChar "1" = smap succChar "1"
    spmap ~process_count:3 succChar "" = smap succChar ""
    spmap ~process_count:2 succChar "" = smap succChar ""
    spmap ~process_count:1 succChar "" = smap succChar ""
    spmap ~process_count:0 succChar "" = smap succChar ""
    spmap succChar (1--^|10) = smap succChar (1--^|10)
    spmap succChar "" = smap succChar ""
    spmap succChar "1" = smap succChar "1"
  **)

  let pfilter f = pmapReduce (concat "") (filter f)
  (**T
    spfilter (even @. ord) ('0'--^'9') = "02468"
    spfilter (odd @. ord) ('0'--^'9') = "13579"
    spfilter (even @. ord) "1" = ""
    spfilter (odd @. ord) "1" = "1"
    spfilter (even @. ord) "" = ""
  **)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)
  (**T
    spfoldlSeqN 3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldlSeqN ~process_count:2 3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldlSeqN ~process_count:2 3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldlSeqN ~process_count:1 3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldlSeqN ~process_count:1 3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldlSeqN ~process_count:0 3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldlSeqN ~process_count:0 3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldlSeqN ~process_count:3 3 (+^) (+^) '\000' (1--^|10) = chr @@ ssum (1--^|10)
    spfoldlSeqN ~process_count:3 3 (+^) (+^) '\000' "1" = chr @@ ssum "1"
    spfoldlSeqN ~process_count:2 3 (multiply) (flip (multiply @. ord)) 1 (1--^|10) = sproduct (1--^|10)
    spfoldlSeqN ~process_count:2 3 (multiply) (flip (multiply @. ord)) 1 "1" = sproduct "1"
    optNF (spfoldlSeqN ~process_count:2 3 (+^) (+^) '\000') "" = Some (chr 0)
  **)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)
  (**T
    spiterSeqN ~process_count:3 1 ignore succChar (1--^|10) = ()
    spiterSeqN ~process_count:2 2 ignore succChar (1--^|10) = ()
    spiterSeqN ~process_count:1 1 ignore succChar (1--^|10) = ()
    spiterSeqN ~process_count:0 4 ignore succChar (1--^|10) = ()
    spiterSeqN ~process_count:3 1 ignore succChar "1" = ()
    spiterSeqN ~process_count:2 6 ignore succChar "1" = ()
    spiterSeqN ~process_count:1 1 ignore succChar "1" = ()
    spiterSeqN ~process_count:0 1 ignore succChar "1" = ()
    spiterSeqN ~process_count:3 2 ignore succChar "" = ()
    spiterSeqN ~process_count:2 1 ignore succChar "" = ()
    spiterSeqN ~process_count:1 3 ignore succChar "" = ()
    spiterSeqN ~process_count:0 1 ignore succChar "" = ()
    spiterSeqN 0 ignore succChar "" = ()
    spiterSeqN 1 ignore succChar (1--^|10) = ()
    spiterSeqN 1 ignore succChar "1" = ()
  **)

  let pinit ?process_count f l =
    let process_count = max 1 (process_count |? !global_process_count) in
    let plen = int (ceil (float l /. float process_count)) in
    let process i =
      let start = plen * i in
      let len = min plen (l - start) in
      init (fun j -> f (start + j)) len in
    concat "" (par_map ~process_count process (0--(process_count-1)))
  (**T
    spinit (chr @. succ) 10 = (1--^|10)
    spinit (chr @. pred @. add 2) 10 = (1--^|10)
    spinit (chr @. succ) 0 = ""
    spinit (chr @. succ) 1 = "\001"
    spinit ~process_count:4 (chr @. succ) 10 = (1--^|10)
    spinit ~process_count:3 (chr @. pred @. add 2) 10 = (1--^|10)
    spinit ~process_count:2 (chr @. succ) 0 = ""
    spinit ~process_count:1 (chr @. pred @. add 2) 10 = (1--^|10)
    spinit ~process_count:1 (chr @. succ) 1 = "\001"
    spinit ~process_count:0 (chr @. succ) 1 = "\001"
  **)

  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len
  (**T
    spzipWith (+^) (1--^|10) (1--^|10) = smap (dup (+^)) (1--^|10)
    spzipWith (-^) (3--^|7) (3--^|1) = "\000\002\004"
    spzipWith (-^) (5--^|7) (5--^|1) = "\000\002\004"
    spzipWith (+^) "1" (1--^|10) = "2"
    spzipWith (+^) (1--^|10) "1" = "2"
    spzipWith (+^) "\001" "\001" = "\002"
    spzipWith (+^) "" (1--^|10) = ""
    spzipWith (+^) (1--^|10) "" = ""
    spzipWith (+^) "" "" = ""
    spzipWith (+^) ~process_count:3 (1--^|10) (1--^|10) = smap (dup (+^)) (1--^|10)
    spzipWith (-^) ~process_count:2 (3--^|7) (3--^|1) = "\000\002\004"
    spzipWith (-^) ~process_count:1 (5--^|7) (5--^|1) = "\000\002\004"
    spzipWith (+^) ~process_count:0 "1" (1--^|10) = "2"
  **)

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine
  (**T
    PreString.par_mapReduceWithIndex ~process_count:5 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) ('0'--^'8') = "96521"
    PreString.par_mapReduceWithIndex ~process_count:5 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) "" = ""
    PreString.par_mapReduceWithIndex ~process_count:5 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) "0" = "1"
    PreString.par_mapReduceWithIndex ~process_count:0 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) "0" = "1"
    PreString.par_mapReduceWithIndex ~process_count:0 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) "" = ""
    PreString.par_mapReduceWithIndex ~process_count:0 ~combine:(sreverse @. join "") ~process:(fun (l, idx) -> smap succChar (if odd idx then "" else l)) (0--^|9) = (10--^|1)
  **)

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process
  (**T
    spmapReduceWithIndex ~process_count:5 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) ('0'--^'8') = "96521"
    spmapReduceWithIndex ~process_count:5 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) "" = ""
    spmapReduceWithIndex ~process_count:5 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) "0" = "1"
    spmapReduceWithIndex ~process_count:0 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) "0" = "1"
    spmapReduceWithIndex ~process_count:0 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) "" = ""
    spmapReduceWithIndex ~process_count:0 (sreverse @. join "") (fun (l, idx) -> smap succChar (if odd idx then "" else l)) (0--^|9) = (10--^|1)
  **)

  let pmapWithInit init f =
    pmapReduceWithIndex (concat "") (fun (sublist, idx) -> map f (init sublist idx))
  (**T
    spmapWithInit ~process_count:2 (fun l i -> if odd i then sreverse l else l) succChar (0--^|9) = (1--^|5) ^ (10--^|6)
    spmapWithInit ~process_count:2 (fun l i -> if odd i then sreverse l else l) succChar "0" = "1"
    spmapWithInit ~process_count:2 (fun l i -> if odd i then sreverse l else l) succChar "" = ""
    spmapWithInit ~process_count:1 (fun l i -> if odd i then sreverse l else l) succChar "0" = "1"
    spmapWithInit ~process_count:1 (fun l i -> if odd i then sreverse l else l) succChar "" = ""
    spmapWithInit ~process_count:(-1) (fun l i -> if odd i then sreverse l else l) succChar "0" = "1"
    spmapWithInit ~process_count:0 (fun l i -> if odd i then sreverse l else l) succChar "" = ""
  **)

end




module Bytestring =
struct
  include PreString

  (* Basic operations *)

  let unsafe_get s i = ord (String.unsafe_get s i)
  (**T
    buget "ABCD" 0 = 65
    buget "ABCD" 1 = 66
    buget "ABCD" 2 = 67
    buget "ABCD" 3 = 68
  **)
  let unsafe_set s i c = String.unsafe_set s i (chr c)
  (**T
    (let s = "a" in buset s 0 65; s = "A")
  **)

  let make l i = make l (chr i)
  (**T
    bmake 0 65 = ""
    bmake 3 65 = "AAA"
  **)

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

  let replicate n v = make (max 0 n) v
  (**T
    Bytestring.replicate 5 65 = "AAAAA"
    Bytestring.replicate 1 65 = "A"
    breplicate 0 65 = ""
    Bytestring.replicate (-1) 65 = ""
  **)

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  (**T
    mapWith biter id (sreplicate 100000 '.') = replicate 100000 (ord '.')
    mapWith biter id (1--^|10) = (1--10)
    mapWith biter succ (1--^|10) = (2--11)
    mapWith biter succ (1--^|1) = [2]
    mapWith biter succ "" = []
  **)
  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done
  (**T
    mapWith (fun f -> biterWithIndex (fun s i -> f (s,i))) (uncurry (+)) (0--^|10) = map (multiply 2) (0--10)
    (let i = ref 0 and j = ref 0 in biterWithIndex (fun a b -> i:=a; j:=b) (20--^|30); !i = 30 && !j = 10)
    biterWithIndex (ignore @.. add) "1" = ()
    biterWithIndex (ignore @.. add) "" = ()
  **)

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    bmap succ (1--^|10) = (2--^|11)
    bmap succ "" = ""
    bmap succ "1" = "2"
  **)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)
  (**T
    bmapWithIndex (fun s i -> s + i) (0--^|10) = bmap (multiply 2) (0--^|10)
    bmapWithIndex ((-)) (10--^|20) = breplicate 11 10
    bmapWithIndex ((+)) "" = ""
    bmapWithIndex ((+)) "1" = "1"
  **)

  let mapToList f s = PreList.init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    bmapToList id "ABC" = (65--67)
    bmapToList succ "ABC" = (66--68)
    bmapToList id "a" = [97]
    bmapToList id "" = []
  **)
  let mapToArray f s = PreArray.init (fun i -> f (unsafe_get s i)) (len s)
  (**T
    bmapToArray id "ABC" = (65--|67)
    bmapToArray succ "ABC" = (66--|68)
    bmapToArray id "a" = [|97|]
    bmapToArray id "" = [||]
  **)

  (* Conversions *)

  let to_array s = PreArray.init (unsafe_get s) (len s)
  (**T
    Bytestring.to_array "ABCDEF" = (65--|70)
    Bytestring.to_array "A" = [|65|]
    Bytestring.to_array "" = [||]
  **)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)
  (**T
    Bytestring.of_array (65--|70) = "ABCDEF"
    Bytestring.of_array [|65|] = "A"
    Bytestring.of_array [||] = ""
  **)

  let to_list s = PreList.init (unsafe_get s) (len s)
  (**T
    Bytestring.to_list "ABCDEF" = (65--70)
    Bytestring.to_list "A" = [65]
    Bytestring.to_list "" = []
  **)
  let of_list l = of_array (Array.of_list l)


  (* Searching *)
  (**T
    Bytestring.of_list (65--70) = "ABCDEF"
    Bytestring.of_list [65] = "A"
    Bytestring.of_list [] = ""
  **)

  let filter f s =
    let rec aux f s i res =
      if i < 0 then of_list res
      else
        let c = unsafe_get s i in
        let res = if f c then c::res else res in
        aux f s (i-1) res in
    aux f s (len s - 1) []
  (**T
    bfilter even (1--^|10) = bmap (dup (+)) (1--^|5)
    bfilter odd (1--^|10) = bmap (subtract 1 @. multiply 2) (1--^|5)
    bfilter even "1" = ""
    bfilter odd "1" = "1"
    bfilter even "" = ""
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
    bfilterWithIndex (fun e i -> e > 5) (1--^|9) = (6--^|9)
    bfilterWithIndex (fun _ i -> i > 5) (1--^|9) = (7--^|9)
    bfilterWithIndex (fun _ i -> i > 10) (1--^|9) = ""
    bfilterWithIndex (fun _ i -> i > 10) "" = ""
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
    bfindWithIndex (fun v _ -> v > 4) (2--^|9) = (5,3)
    bfindWithIndex (fun _ i -> i > 4) (2--^|9) = (7,5)
    optNF (bfindWithIndex (const (gt 4))) (0--^|3) = None
    optNF (bfindWithIndex (const (gt 4))) "" = None
  **)

  let find f s = fst (findWithIndex (fun v _ -> f v) s)
  (**T
    bfind (gt 5) (1--^|9) = 6
    optNF (bfind (gt 4)) (0--^|3) = None
    optNF (bfind (gt 4)) "" = None
  **)
  let findIndex f s = snd (findWithIndex (fun v _ -> f v) s)
  (**T
    bfindIndex (gt 5) (1--^|9) = 5
    optNF (bfindIndex (gt 4)) (0--^|3) = None
    optNF (bfindIndex (gt 4)) "" = None
  **)

  let indexOf v s = findIndex ((=) v) s
  (**T
    bindexOf 14 (10--^|20) = 4
    optNF (bindexOf 1) (10--^|20) = None
    bindexOf 97 ("foobar") = 4
  **)

  (* Predicates *)

  let all p a = None = optNF (find (fun i -> not (p i))) a
  (**T
    ball (gt (5)) (1--^|10) = false
    ball (lt (11)) (1--^|10) = true
    ball (gt (4)) "" = true
  **)
  let any p a = None <> optNF (find p) a
  (**T
    bany (gt (5)) (1--^|10) = true
    bany (gt (11)) (1--^|10) = false
    bany (gt (4)) "" = false
  **)

  let includes x s = None <> optNF (indexOf x) s
  (**T
    bincludes (4) (1--^|10) = true
    bincludes (0) (1--^|10) = false
    bincludes (5) "" = false
  **)
  let has = includes
  (**T
    bhas (1) (1--^|10) = true
    bhas (0) (1--^|10) = false
  **)
  let elem = includes
  (**T
    belem (ord 'b') "foo" = false
    belem (ord 'b') "foobar" = true
  **)
  let notElem x lst = not @@ elem x lst
  (**T
    bnotElem (4) (1--^|10) = false
    bnotElem (0) (1--^|10) = true
    bnotElem (5) "" = true
  **)

  (* Zipping *)

  let zipWith f a b =
    let len = min (len a) (len b) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) ) len
  let map2 = zipWith
  (**T
    bzipWith (+) (1--^|10) (1--^|10) = bmap (dup (+)) (1--^|10)
    bzipWith (+) "1" (1--^|10) = "2"
    bzipWith (+) (1--^|10) "1" = "2"
    bzipWith (+) "\001" "1" = "2"
    bzipWith (+) "" (1--^|10) = ""
    bzipWith (+) (1--^|10) "" = ""
    bzipWith (+) "" "" = ""
  **)

  let zipWith3 f a b c =
    let len = min (min (len a) (len b)) (len c) in
    init (fun i -> f (unsafe_get a i) (unsafe_get b i) (unsafe_get c i) ) len
  let map3 = zipWith3
  (**T
    bzipWith3 (fun a b c -> a + b + c) (1--^|50) (1--^|50) (1--^|50) = bmap (multiply 3) (1--^|50)
    bzipWith3 (fun a b c -> a + b + c) "1" (1--^|10) (1--^|10) = "3"
    bzipWith3 (fun a b c -> a + b + c) (1--^|10) "1" (1--^|10) = "3"
    bzipWith3 (fun a b c -> a + b + c) (1--^|10) (1--^|10) "1" = "3"
    bzipWith3 (fun a b c -> a + b + c) "1" (1--^|10) "\001" = "3"
    bzipWith3 (fun a b c -> a + b + c) "1" "\001" (1--^|10) = "3"
    bzipWith3 (fun a b c -> a + b + c) (1--^|10) "" "1" = ""
    bzipWith3 (fun a b c -> a + b + c) (1--^|10) "1" "" = ""
    bzipWith3 (fun a b c -> a + b + c) (1--^|10) "" "" = ""
    bzipWith3 (fun a b c -> a + b + c) "" "1" "1" = ""
    bzipWith3 (fun a b c -> a + b + c) "" "" "1" = ""
    bzipWith3 (fun a b c -> a + b + c) "" "1" "" = ""
    bzipWith3 (fun a b c -> a + b + c) "1" "" "" = ""
    bzipWith3 (fun a b c -> a + b + c) "" "" "" = ""
  **)


  (* Folds *)

  let foldl f init s =
    let rec aux f s len v i =
      if i >= len then v else aux f s len (f v (unsafe_get s i)) (i+1) in
    aux f s (len s) init 0
  (**T
    bfoldl (+) 0 (1--^|10) = 55
    bfoldl (fun s b -> s ^ (string_of_int b)) "--" (1--^|3) = "--123"
    bfoldl (+) 1 "" = 1
    bfoldl (+) 1 "\001" = 2
  **)

  let foldl1 f a =
    let rec aux f i acc len a =
      if i >= len then acc
      else aux f (i+1) (f acc (unsafe_get a i)) len a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f 1 (unsafe_get a 0) len a
  (**T
    bfoldl1 (+) (1--^|10) = 55
    optNF (bfoldl1 (+)) "" = None
    bfoldl1 (+) "\001" = 1
  **)

  let foldr f init s =
    let rec aux f s v i =
      if i < 0 then v else aux f s (f (unsafe_get s i) v) (i-1) in
    aux f s init (len s - 1)
  (**T
    bfoldr (+) 0 (1--^|10) = 55
    bfoldr (fun a s -> s ^ (string_of_int a)) "--^|" (1--^|3) = "--^|321"
    bfoldr (+) 1 "" = 1
    bfoldr (+) 1 "\001" = 2
  **)

  let foldr1 f a =
    let rec aux f i acc a =
      if i < 0 then acc
      else aux f (i-1) (f (unsafe_get a i) acc) a in
    let len = len a in
    if len < 1 then raise Not_found;
    aux f (len-2) (unsafe_get a (len-1)) a
  (**T
    bfoldr1 (+) (1--^|10) = 55
    optNF (bfoldr1 (+)) "" = None
    bfoldr1 (+) "\001" = 1
  **)


  let maximum a = foldl1 max a
  (**T
    bmaximum "12301431" = ord '4'
    bmaximum "\001" = 1
    optNF bmaximum "" = None
  **)
  let minimum a = foldl1 min a
  (**T
    bminimum "12301431" = ord '0'
    bminimum "\001" = 1
    optNF bminimum "" = None
  **)

  let maximumBy f = foldl1 (fun s i -> if (f s) < (f i) then i else s)
  (**T
    bmaximumBy (square @. subtract 3) (0 --^| 5) = 0
    bmaximumBy (square @. subtract 2) (0 --^| 5) = 5
    optNF (bmaximumBy square) "" = None
  **)
  let minimumBy f = foldl1 (fun s i -> if (f s) > (f i) then i else s)
  (**T
    bminimumBy (square @. subtract 3) (0 --^| 5) = 3
    bminimumBy (square @. subtract 3) (0 --^| 1) = 1
    optNF (bminimumBy square) "" = None
  **)

  let maximumByWith f lst = PreArray.maximumBy snd (mapToArray (fupler f) lst)
  (**T
    bmaximumByWith (square @. subtract 3) (0 --^| 5) = (0, 9)
    bmaximumByWith (square @. subtract 2) (0 --^| 5) = (5, 9)
    optNF (bmaximumByWith (square)) "" = None
  **)
  let minimumByWith f lst = PreArray.minimumBy snd (mapToArray (fupler f) lst)
  (**T
    bminimumByWith (square @. subtract 3) (0 --^| 5) = (3, 0)
    bminimumByWith (square @. subtract 3) (0 --^| 1) = (1, 4)
    optNF (bminimumByWith (square)) "" = None
  **)


  (* List-like interface *)

  let first a = if len a = 0 then raise Not_found else unsafe_get a 0
  let head = first
  (**T
    bfirst (2--^|10) = ord '\002'
    optNF bhead "" = None
    optNF bhead "1" = Some (ord '1')
    optNF bhead "123456789" = Some (ord '1')
  **)

  let last a = if len a = 0 then raise Not_found else unsafe_get a (len a - 1)
  (**T
    blast "123" = ord '3'
    blast "1" = ord '1'
    optNF blast "" = None
  **)

  let pop a = (popped a, last a)
  (**T
    bpop "123" = ("12", ord '3')
    optNF bpop "" = None
    optNF bpop "1" = Some ("", ord '1')
    optNF bpop "12" = Some ("1", ord '2')
  **)
  let push v a = append a (string_of_char (chr v))
  (**T
    bpush 10 (1--^|9) = (1--^|10)
    bpush 1 "\000" = (0--^|1)
    bpush 0 "" = "\000"
  **)

  let shift a = (tail a, first a)
  (**T
    bshift (1--^|10) = ((2--^|10), 1)
    optNF bshift "" = None
    optNF bshift "1" = Some ("", ord '1')
  **)
  let unshift v a = append (string_of_char (chr v)) a
  (**T
    bunshift 0 (1--^|10) = (0--^|10)
    bunshift 0 "\001" = (0--^|1)
    bunshift 0 "" = "\000"
  **)

  let takeWhile f s = take (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    btakeWhile (lt 5) (1--^|10) = (1--^|4)
    btakeWhile (lt 5) (6--^|10) = ""
    btakeWhile (lt 5) "" = ""
    btakeWhile (lt 5) (1--^|3) = (1--^|3)
  **)
  let takeUntil f s = take (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    btakeUntil (gte 5) (1--^|10) = (1--^|4)
    btakeUntil (gte 5) (6--^|10) = ""
    btakeUntil (gte 5) "" = ""
    btakeUntil (gte 5) (1--^|3) = (1--^|3)
  **)

  let dropWhile f s = drop (maybeNF (len s) (findIndex (fun v -> not (f v))) s) s
  (**T
    bdropWhile (lt 5) (1--^|10) = (5--^|10)
    bdropWhile (lt 5) (6--^|10) = (6--^|10)
    bdropWhile (lt 5) "" = ""
    bdropWhile (lt 5) (1--^|3) = ""
  **)
  let dropUntil f s = drop (maybeNF (len s) (findIndex (fun v -> f v)) s) s
  (**T
    bdropUntil (gte 5) (1--^|10) = (5--^|10)
    bdropUntil (gte 5) (6--^|10) = (6--^|10)
    bdropUntil (gte 5) "" = ""
    bdropUntil (gte 5) (1--^|3) = ""
  **)

  let break f s = splitAt (maybeNF (len s) (findIndex f) s) s
  (**T
    bbreak (gt 5) "\004\003\006\002" = ("\004\003", "\006\002")
    bbreak (gt 5) (1--^|10) = ((1--^|5), (6--^|10))
  **)
  let span f s = break (fun v -> not (f v)) s
  (**T
    bspan (gt (ord '5')) "6006" = ("6", "006")
    bspan (lessOrEqualTo (ord '5')) ('0'--^'9') = ("012345", "6789")
    bspan (gt (ord '5')) "" = ("", "")
    bspan (gt (ord '5')) "6" = ("6", "")
    bspan (gt (ord '5')) "0" = ("", "0")
    bspan (gt (ord '5')) "06" = ("", "06")
    bspan (gt (ord '5')) "60" = ("6", "0")
    bspan (gt (ord '5')) "66" = ("66", "")
    bspan (gt (ord '5')) "00" = ("", "00")
  **)

  let interlace elem s =
    init (fun i -> if i mod 2 = 0 then unsafe_get s (i/2) else elem) (max 0 (2 * len s - 1))
  (**T
    binterlace (ord '0') "123" = "10203"
    binterlace (ord '-') "abcde" = "a-b-c-d-e"
    binterlace (ord '0') "" = ""
    binterlace (ord '0') "1" = "1"
    binterlace (ord '0') "12" = "102"
  **)


  let reject f s = filter (fun v -> not (f v)) s
 (**T
    breject (gt 4) (1--^|5) = (1--^|4)
    breject (gt 4) "" = ""
    breject (gt 0) (1--^|5) = ""
    breject (gt 5) (1--^|5) = (1--^|5)
    breject (gt 3) (5--^|1) = (3--^|1)
  **)
  let without v s = filter ((<>) v) s
  (**T
    bwithout (ord '4') "124124" = "1212"
    bwithout (ord '4') "" = ""
    bwithout (ord '4') "4" = ""
    bwithout (ord '4') "1" = "1"
  **)

(* Subsequence iterators *)


  let iterSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    for j=first to first+sub_len-1 do f (unsafe_get s j) done
  (**T
    mapWith (biterSub 0 10) id (1--^|10) = ((1--10))
    mapWith (biterSub 2 6) id (1--^|10) = ((3--8))
    mapWith (biterSub (-2) 10) id (1--^|10) = ((9--10))
    mapWith (biterSub (-18) 10) id (1--^|10) = [1; 2]
    mapWith (biterSub (-10) 10) id (1--^|10) = ((1--10))
    mapWith (biterSub (-12) 1) id (1--^|10) = []
    mapWith (biterSub 9 10) id (1--^|10) = [10]
    mapWith (biterSub (-19) 10) id (1--^|10) = [1]
    mapWith (biterSub (-20) 10) id (1--^|10) = []
    mapWith (biterSub (-20) 20) id (1--^|10) = ((1--10))
    mapWith (biterSub (-20) 50) id "" = []
    mapWith (biterSub (-20) 50) id "\001" = [1]
  **)

  let iterSlice i j f s =
    let i, len = slice_to_sub i j s in
    iterSub i len f s
  (**T
    mapWith (biterSlice 0 9) id (1--^|10) = ((1--10))
    mapWith (biterSlice 2 6) id (1--^|10) = ((3--7))
    mapWith (biterSlice 6 2) id (1--^|10) = []
    mapWith (biterSlice 0 0) id (1--^|10) = [1]
    mapWith (biterSlice 1 0) id (1--^|10) = []
    mapWith (biterSlice 9 9) id (1--^|10) = [10]
    mapWith (biterSlice 9 8) id (1--^|10) = []
    mapWith (biterSlice (-2) (-1)) id (1--^|10) = [9; 10]
    mapWith (biterSlice (-12) 0) id (1--^|10) = [1]
    mapWith (biterSlice (-12) (-1)) id (1--^|10) = ((1--10))
    mapWith (biterSlice (-12) (-11)) id (1--^|10) = []
    mapWith (biterSlice (-5) (-1)) id (1--^|10) = ((6--10))
    mapWith (biterSlice (-20) 20) id (1--^|10) = ((1--10))
    mapWith (biterSlice (-20) 50) id "" = []
    mapWith (biterSlice (-20) 50) id "\001" = [1]
  **)

  let mapSub i len f s =
    let first, sub_len = sub_start_and_length i len s in
    init (fun j -> f (unsafe_get s (first+j))) sub_len
  (**T
    bmapSub 0 10 succ (1--^|10) = (2--^|11)
    bmapSub 2 6 id (1--^|10) = (3--^|8)
    bmapSub (-2) 10 id (1--^|10) = (9--^|10)
    bmapSub (-18) 10 id (1--^|10) = "\001\002"
    bmapSub (-10) 10 id (1--^|10) = (1--^|10)
    bmapSub (-12) 1 id (1--^|10) = ""
    bmapSub 9 10 id (1--^|10) = "\010"
    bmapSub (-19) 10 id (1--^|10) = "\001"
    bmapSub (-20) 10 id (1--^|10) = ""
    bmapSub (-20) 20 id (1--^|10) = (1--^|10)
    bmapSub (-20) 50 id "" = ""
    bmapSub (-20) 50 id "\001" = "\001"
  **)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s
  (**T
    bmapSlice 0 9 id (1--^|10) = (1--^|10)
    bmapSlice 2 6 id (1--^|10) = (3--^|7)
    bmapSlice 6 2 id (1--^|10) = ""
    bmapSlice 0 0 id (1--^|10) = "\001"
    bmapSlice 1 0 id (1--^|10) = ""
    bmapSlice 9 9 id (1--^|10) = "\010"
    bmapSlice 9 8 id (1--^|10) = ""
    bmapSlice (-2) (-1) id (1--^|10) = "\009\010"
    bmapSlice (-12) 0 id (1--^|10) = "\001"
    bmapSlice (-12) (-1) id (1--^|10) = (1--^|10)
    bmapSlice (-12) (-11) id (1--^|10) = ""
    bmapSlice (-5) (-1) id (1--^|10) = (6--^|10)
    bmapSlice (-20) 20 id (1--^|10) = (1--^|10)
    bmapSlice (-20) 50 id "" = ""
    bmapSlice (-20) 50 id "\001" = "\001"
  **)

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    bfoldlSub 0 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSub (-10) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSub (-20) 20 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSub 0 3 (+) 0 (1--^|10) = bsum (1--^|3)
    bfoldlSub 3 3 (+) 0 (1--^|10) = bsum (4--^|6)
    bfoldlSub (-3) 3 (+) 0 (1--^|10) = bsum (8--^|10)
    bfoldlSub (-1) 3 (+) 0 (1--^|10) = bsum (10--^|10)
    bfoldlSub (-3) 1 (+) 0 (1--^|10) = bsum (8--^|8)
    bfoldlSub 20 (-20) (+) 0 (1--^|10) = bsum ""
    bfoldlSub (-20) 10 (+) 0 (1--^|10) = bsum ""
    bfoldlSub 10 0 (+) 0 (1--^|10) = bsum ""
    bfoldlSub 3 (-1) (+) 0 (1--^|10) = bsum ""

    bfoldlSub 0 1 (+) 0 (1--^|1) = bsum (1--^|1)
    bfoldlSub 0 1 (+) 0 "" = bsum ""
  **)

  let foldl1Sub i len f s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    bfoldl1Sub 0 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Sub (-10) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Sub (-20) 20 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Sub 0 3 (+) (1--^|10) = bsum (1--^|3)
    bfoldl1Sub 3 3 (+) (1--^|10) = bsum (4--^|6)
    bfoldl1Sub (-3) 3 (+) (1--^|10) = bsum (8--^|10)
    bfoldl1Sub (-1) 3 (+) (1--^|10) = bsum (10--^|10)
    bfoldl1Sub (-3) 1 (+) (1--^|10) = bsum (8--^|8)
    optNF (bfoldl1Sub 20 (-20) (+)) (1--^|10) = None
    optNF (bfoldl1Sub (-20) 10 (+)) (1--^|10) = None
    optNF (bfoldl1Sub 10 0 (+)) (1--^|10) = None
    optNF (bfoldl1Sub 3 (-1) (+)) (1--^|10) = None

    bfoldl1Sub 0 1 (+) (1--^|1) = bsum (1--^|1)
    optNF (bfoldl1Sub 0 1 (+)) "" = None
  **)

  let foldrSub i len f init s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    aux f s init first (first+sub_len-1)
  (**T
    bfoldrSub 0 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSub (-10) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSub (-20) 20 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSub 0 3 (+) 0 (1--^|10) = bsum (1--^|3)
    bfoldrSub 3 3 (+) 0 (1--^|10) = bsum (4--^|6)
    bfoldrSub (-3) 3 (+) 0 (1--^|10) = bsum (8--^|10)
    bfoldrSub (-1) 3 (+) 0 (1--^|10) = bsum (10--^|10)
    bfoldrSub (-3) 1 (+) 0 (1--^|10) = bsum (8--^|8)
    bfoldrSub 20 (-20) (+) 0 (1--^|10) = bsum ""
    bfoldrSub (-20) 10 (+) 0 (1--^|10) = bsum ""
    bfoldrSub 10 0 (+) 0 (1--^|10) = bsum ""
    bfoldrSub 3 (-1) (+) 0 (1--^|10) = bsum ""

    bfoldrSub 0 1 (+) 0 (1--^|1) = bsum (1--^|1)
    bfoldrSub 0 1 (+) 0 "" = bsum ""
  **)

  let foldr1Sub i len f s =
    let rec aux f s v i j =
      if j < i then v else aux f s (f v (unsafe_get s j)) i (j-1) in
    let first, sub_len = sub_start_and_length i len s in
    if sub_len <= 0 || first < 0 || first >= length s
    then raise Not_found
    else aux f s (unsafe_get s first) (first+1) (first+sub_len-1)
  (**T
    bfoldr1Sub 0 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Sub (-10) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Sub (-20) 20 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Sub 0 3 (+) (1--^|10) = bsum (1--^|3)
    bfoldr1Sub 3 3 (+) (1--^|10) = bsum (4--^|6)
    bfoldr1Sub (-3) 3 (+) (1--^|10) = bsum (8--^|10)
    bfoldr1Sub (-1) 3 (+) (1--^|10) = bsum (10--^|10)
    bfoldr1Sub (-3) 1 (+) (1--^|10) = bsum (8--^|8)
    optNF (bfoldr1Sub 20 (-20) (+)) (1--^|10) = None
    optNF (bfoldr1Sub (-20) 10 (+)) (1--^|10) = None
    optNF (bfoldr1Sub 10 0 (+)) (1--^|10) = None
    optNF (bfoldr1Sub 3 (-1) (+)) (1--^|10) = None

    bfoldr1Sub 0 1 (+) (1--^|1) = bsum (1--^|1)
    optNF (bfoldr1Sub 0 1 (+)) "" = None
  **)

  let foldlSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldlSub i len f init s
  (**T
    bfoldlSlice 0 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice 0 9 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice 0 (-1) (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice (-10) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice (-20) 20 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice (-20) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldlSlice 0 3 (+) 0 (1--^|10) = bsum (1--^|4)
    bfoldlSlice 3 (-1) (+) 0 (1--^|10) = bsum (4--^|10)
    bfoldlSlice 3 3 (+) 0 (1--^|10) = bsum (4--^|4)
    bfoldlSlice (-1) (-1) (+) 0 (1--^|10) = bsum (10--^|10)
    bfoldlSlice (-3) 3 (+) 0 (1--^|10) = bsum ""
    bfoldlSlice (-3) 1 (+) 0 (1--^|10) = bsum ""
    bfoldlSlice 20 (-20) (+) 0 (1--^|10) = bsum ""
    bfoldlSlice 10 0 (+) 0 (1--^|10) = bsum ""

    bfoldlSlice 0 1 (+) 0 (1--^|1) = bsum (1--^|1)
    bfoldlSlice 0 1 (+) 0 "" = bsum ""
  **)

  let foldl1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldl1Sub i len f s
  (**T
    bfoldl1Slice 0 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice 0 9 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice 0 (-1) (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice (-10) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice (-20) 20 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice (-20) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldl1Slice 0 3 (+) (1--^|10) = bsum (1--^|4)
    bfoldl1Slice 3 (-1) (+) (1--^|10) = bsum (4--^|10)
    bfoldl1Slice 3 3 (+) (1--^|10) = bsum (4--^|4)
    bfoldl1Slice (-1) (-1) (+) (1--^|10) = bsum (10--^|10)
    optNF (bfoldl1Slice (-3) 3 (+)) (1--^|10) = None
    optNF (bfoldl1Slice (-3) 1 (+)) (1--^|10) = None
    optNF (bfoldl1Slice 20 (-20) (+)) (1--^|10) = None
    optNF (bfoldl1Slice 10 0 (+)) (1--^|10) = None

    bfoldl1Slice 0 1 (+) (1--^|1) = bsum (1--^|1)
    optNF (bfoldl1Slice 0 1 (+)) "" = None
  **)

  let foldrSlice i j f init s =
    let i, len = slice_to_sub i j s in
    foldrSub i len f init s
  (**T
    bfoldrSlice 0 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice 0 9 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice 0 (-1) (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice (-10) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice (-20) 20 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice (-20) 10 (+) 0 (1--^|10) = bsum (1--^|10)
    bfoldrSlice 0 3 (+) 0 (1--^|10) = bsum (1--^|4)
    bfoldrSlice 3 (-1) (+) 0 (1--^|10) = bsum (4--^|10)
    bfoldrSlice 3 3 (+) 0 (1--^|10) = bsum (4--^|4)
    bfoldrSlice (-1) (-1) (+) 0 (1--^|10) = bsum (10--^|10)
    bfoldrSlice (-3) 3 (+) 0 (1--^|10) = bsum ""
    bfoldrSlice (-3) 1 (+) 0 (1--^|10) = bsum ""
    bfoldrSlice 20 (-20) (+) 0 (1--^|10) = bsum ""
    bfoldrSlice 10 0 (+) 0 (1--^|10) = bsum ""

    bfoldrSlice 0 1 (+) 0 (1--^|1) = bsum (1--^|1)
    bfoldrSlice 0 1 (+) 0 "" = bsum ""
  **)

  let foldr1Slice i j f s =
    let i, len = slice_to_sub i j s in
    foldr1Sub i len f s
  (**T
    bfoldr1Slice 0 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice 0 9 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice 0 (-1) (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice (-10) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice (-20) 20 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice (-20) 10 (+) (1--^|10) = bsum (1--^|10)
    bfoldr1Slice 0 3 (+) (1--^|10) = bsum (1--^|4)
    bfoldr1Slice 3 (-1) (+) (1--^|10) = bsum (4--^|10)
    bfoldr1Slice 3 3 (+) (1--^|10) = bsum (4--^|4)
    bfoldr1Slice (-1) (-1) (+) (1--^|10) = bsum (10--^|10)
    optNF (bfoldr1Slice (-3) 3 (+)) (1--^|10) = None
    optNF (bfoldr1Slice (-3) 1 (+)) (1--^|10) = None
    optNF (bfoldr1Slice 20 (-20) (+)) (1--^|10) = None
    optNF (bfoldr1Slice 10 0 (+)) (1--^|10) = None

    bfoldr1Slice 0 1 (+) (1--^|1) = bsum (1--^|1)
    optNF (bfoldr1Slice 0 1 (+)) "" = None
  **)

  let add_int = (+)
  let add_float i c = i +. float c
  let mul_int = ( * )
  let mul_float i c = i *. float c

  let sum a = foldl add_int 0 a
  (**T
    bsum (1--^|10) = 55
    bsum "\001\002" = 3
    bsum "\001" = 1
    bsum "\000" = 0
    bsum "" = 0
  **)
  let sumf a = foldl add_float 0. a
  (**T
    bsumf (1--^|10) = 55.
    bsumf "\001\002" = 3.
    bsumf "\001" = 1.
    bsumf "\000" = 0.
    bsumf "" = 0.
  **)
  let product a = foldl mul_int 1 a
  (**T
    bproduct (1--^|10) = 3628800
    bproduct "\001\002" = 2
    bproduct "\001" = 1
    bproduct "\000" = 0
    bproduct "" = 1
  **)
  let productf a = foldl mul_float 1. a
  (**T
    bproductf (1--^|10) = 3628800.
    bproductf "\001\002" = 2.
    bproductf "\001" = 1.
    bproductf "\000" = 0.
    bproductf "" = 1.
  **)
  let average a = sum a / len a
  (**T
    baverage (1--^|10) = 5
    baverage "\001" = 1
    optEx Division_by_zero baverage "" = None
  **)
  let averagef a = sumf a /. float (len a)
  (**T
    baveragef (1--^|10) = 5.5
    baveragef "\001" = 1.
    isNaN (baveragef "")
  **)

  let sumSub i len a = foldlSub i len add_int 0 a
  (**T
    bsumSub 0 10 (1--^|10) = bsum (1--^|10)
    bsumSub (-10) 10 (1--^|10) = bsum (1--^|10)
    bsumSub (-20) 20 (1--^|10) = bsum (1--^|10)
    bsumSub 0 3 (1--^|10) = bsum (1--^|3)
    bsumSub 3 3 (1--^|10) = bsum (4--^|6)
    bsumSub (-3) 3 (1--^|10) = bsum (8--^|10)
    bsumSub (-1) 3 (1--^|10) = bsum (10--^|10)
    bsumSub (-3) 1 (1--^|10) = bsum (8--^|8)
    bsumSub 20 (-20) (1--^|10) = bsum ""
    bsumSub (-20) 10 (1--^|10) = bsum ""
    bsumSub 10 0 (1--^|10) = bsum ""
    bsumSub 3 (-1) (1--^|10) = bsum ""

    bsumSub 0 1 (1--^|1) = bsum (1--^|1)
    bsumSub 0 1 "" = bsum ""
  **)
  let sumSubf i len a = foldlSub i len add_float 0. a
  (**T
    bsumSubf 0 10 (1--^|10) = bsumf (1--^|10)
    bsumSubf (-10) 10 (1--^|10) = bsumf (1--^|10)
    bsumSubf (-20) 20 (1--^|10) = bsumf (1--^|10)
    bsumSubf 0 3 (1--^|10) = bsumf (1--^|3)
    bsumSubf 3 3 (1--^|10) = bsumf (4--^|6)
    bsumSubf (-3) 3 (1--^|10) = bsumf (8--^|10)
    bsumSubf (-1) 3 (1--^|10) = bsumf (10--^|10)
    bsumSubf (-3) 1 (1--^|10) = bsumf (8--^|8)
    bsumSubf 20 (-20) (1--^|10) = bsumf ""
    bsumSubf (-20) 10 (1--^|10) = bsumf ""
    bsumSubf 10 0 (1--^|10) = bsumf ""
    bsumSubf 3 (-1) (1--^|10) = bsumf ""

    bsumSubf 0 1 (1--^|1) = bsumf (1--^|1)
    bsumSubf 0 1 "" = bsumf ""
  **)

  let sumSlice i j a = foldlSlice i j add_int 0 a
  (**T
    bsumSlice 0 10 (1--^|10) = bsum (1--^|10)
    bsumSlice 0 9 (1--^|10) = bsum (1--^|10)
    bsumSlice 0 (-1) (1--^|10) = bsum (1--^|10)
    bsumSlice (-10) 10 (1--^|10) = bsum (1--^|10)
    bsumSlice (-20) 20 (1--^|10) = bsum (1--^|10)
    bsumSlice (-20) 10 (1--^|10) = bsum (1--^|10)
    bsumSlice 0 3 (1--^|10) = bsum (1--^|4)
    bsumSlice 3 (-1) (1--^|10) = bsum (4--^|10)
    bsumSlice 3 3 (1--^|10) = bsum (4--^|4)
    bsumSlice (-1) (-1) (1--^|10) = bsum (10--^|10)
    bsumSlice (-3) 3 (1--^|10) = bsum ""
    bsumSlice (-3) 1 (1--^|10) = bsum ""
    bsumSlice 20 (-20) (1--^|10) = bsum ""
    bsumSlice 10 0 (1--^|10) = bsum ""

    bsumSlice 0 1 (1--^|1) = bsum (1--^|1)
    bsumSlice 0 1 "" = bsum ""
  **)
  let sumSlicef i j a = foldlSlice i j add_float 0. a
  (**T
    bsumSlicef 0 10 (1--^|10) = bsumf (1--^|10)
    bsumSlicef 0 9 (1--^|10) = bsumf (1--^|10)
    bsumSlicef 0 (-1) (1--^|10) = bsumf (1--^|10)
    bsumSlicef (-10) 10 (1--^|10) = bsumf (1--^|10)
    bsumSlicef (-20) 20 (1--^|10) = bsumf (1--^|10)
    bsumSlicef (-20) 10 (1--^|10) = bsumf (1--^|10)
    bsumSlicef 0 3 (1--^|10) = bsumf (1--^|4)
    bsumSlicef 3 (-1) (1--^|10) = bsumf (4--^|10)
    bsumSlicef 3 3 (1--^|10) = bsumf (4--^|4)
    bsumSlicef (-1) (-1) (1--^|10) = bsumf (10--^|10)
    bsumSlicef (-3) 3 (1--^|10) = bsumf ""
    bsumSlicef (-3) 1 (1--^|10) = bsumf ""
    bsumSlicef 20 (-20) (1--^|10) = bsumf ""
    bsumSlicef 10 0 (1--^|10) = bsumf ""

    bsumSlicef 0 1 (1--^|1) = bsumf (1--^|1)
    bsumSlicef 0 1 "" = bsumf ""
  **)

  let productSub i len a = foldlSub i len mul_int 1 a
  (**T
    bproductSub 0 10 (1--^|10) = bproduct (1--^|10)
    bproductSub (-10) 10 (1--^|10) = bproduct (1--^|10)
    bproductSub (-20) 20 (1--^|10) = bproduct (1--^|10)
    bproductSub 0 3 (1--^|10) = bproduct (1--^|3)
    bproductSub 3 3 (1--^|10) = bproduct (4--^|6)
    bproductSub (-3) 3 (1--^|10) = bproduct (8--^|10)
    bproductSub (-1) 3 (1--^|10) = bproduct (10--^|10)
    bproductSub (-3) 1 (1--^|10) = bproduct (8--^|8)
    bproductSub 20 (-20) (1--^|10) = bproduct ""
    bproductSub (-20) 10 (1--^|10) = bproduct ""
    bproductSub 10 0 (1--^|10) = bproduct ""
    bproductSub 3 (-1) (1--^|10) = bproduct ""

    bproductSub 0 1 (1--^|1) = bproduct (1--^|1)
    bproductSub 0 1 "" = bproduct ""
  **)
  let productSubf i len a = foldlSub i len mul_float 1. a
  (**T
    bproductSubf 0 10 (1--^|10) = bproductf (1--^|10)
    bproductSubf (-10) 10 (1--^|10) = bproductf (1--^|10)
    bproductSubf (-20) 20 (1--^|10) = bproductf (1--^|10)
    bproductSubf 0 3 (1--^|10) = bproductf (1--^|3)
    bproductSubf 3 3 (1--^|10) = bproductf (4--^|6)
    bproductSubf (-3) 3 (1--^|10) = bproductf (8--^|10)
    bproductSubf (-1) 3 (1--^|10) = bproductf (10--^|10)
    bproductSubf (-3) 1 (1--^|10) = bproductf (8--^|8)
    bproductSubf 20 (-20) (1--^|10) = bproductf ""
    bproductSubf (-20) 10 (1--^|10) = bproductf ""
    bproductSubf 10 0 (1--^|10) = bproductf ""
    bproductSubf 3 (-1) (1--^|10) = bproductf ""

    bproductSubf 0 1 (1--^|1) = bproductf (1--^|1)
    bproductSubf 0 1 "" = bproductf ""
  **)

  let productSlice i j a = foldlSlice i j mul_int 1 a
  (**T
    bproductSlice 0 10 (1--^|10) = bproduct (1--^|10)
    bproductSlice 0 9 (1--^|10) = bproduct (1--^|10)
    bproductSlice 0 (-1) (1--^|10) = bproduct (1--^|10)
    bproductSlice (-10) 10 (1--^|10) = bproduct (1--^|10)
    bproductSlice (-20) 20 (1--^|10) = bproduct (1--^|10)
    bproductSlice (-20) 10 (1--^|10) = bproduct (1--^|10)
    bproductSlice 0 3 (1--^|10) = bproduct (1--^|4)
    bproductSlice 3 (-1) (1--^|10) = bproduct (4--^|10)
    bproductSlice 3 3 (1--^|10) = bproduct (4--^|4)
    bproductSlice (-1) (-1) (1--^|10) = bproduct (10--^|10)
    bproductSlice (-3) 3 (1--^|10) = bproduct ""
    bproductSlice (-3) 1 (1--^|10) = bproduct ""
    bproductSlice 20 (-20) (1--^|10) = bproduct ""
    bproductSlice 10 0 (1--^|10) = bproduct ""

    bproductSlice 0 1 (1--^|1) = bproduct (1--^|1)
    bproductSlice 0 1 "" = bproduct ""
  **)
  let productSlicef i j a = foldlSlice i j mul_float 1. a
  (**T
    bproductSlicef 0 10 (1--^|10) = bproductf (1--^|10)
    bproductSlicef 0 9 (1--^|10) = bproductf (1--^|10)
    bproductSlicef 0 (-1) (1--^|10) = bproductf (1--^|10)
    bproductSlicef (-10) 10 (1--^|10) = bproductf (1--^|10)
    bproductSlicef (-20) 20 (1--^|10) = bproductf (1--^|10)
    bproductSlicef (-20) 10 (1--^|10) = bproductf (1--^|10)
    bproductSlicef 0 3 (1--^|10) = bproductf (1--^|4)
    bproductSlicef 3 (-1) (1--^|10) = bproductf (4--^|10)
    bproductSlicef 3 3 (1--^|10) = bproductf (4--^|4)
    bproductSlicef (-1) (-1) (1--^|10) = bproductf (10--^|10)
    bproductSlicef (-3) 3 (1--^|10) = bproductf ""
    bproductSlicef (-3) 1 (1--^|10) = bproductf ""
    bproductSlicef 20 (-20) (1--^|10) = bproductf ""
    bproductSlicef 10 0 (1--^|10) = bproductf ""

    bproductSlicef 0 1 (1--^|1) = bproductf (1--^|1)
    bproductSlicef 0 1 "" = bproductf ""
  **)

  let averageSub i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSub i len a / sub_len
  (**T
    baverageSub 0 10 (1--^|10) = baverage (1--^|10)
    baverageSub (-10) 10 (1--^|10) = baverage (1--^|10)
    baverageSub (-20) 20 (1--^|10) = baverage (1--^|10)
    baverageSub 0 3 (1--^|10) = baverage (1--^|3)
    baverageSub 3 3 (1--^|10) = baverage (4--^|6)
    baverageSub (-3) 3 (1--^|10) = baverage (8--^|10)
    baverageSub (-1) 3 (1--^|10) = baverage (10--^|10)
    baverageSub (-3) 1 (1--^|10) = baverage (8--^|8)
    optEx Division_by_zero (baverageSub 20 (-20)) (1--^|10) = None
    optEx Division_by_zero (baverageSub (-20) 10) (1--^|10) = None
    optEx Division_by_zero (baverageSub 10 0) (1--^|10) = None
    optEx Division_by_zero (baverageSub 3 (-1)) (1--^|10) = None

    baverageSub 0 1 (1--^|1) = baverage (1--^|1)
    optEx Division_by_zero (baverageSub 0 1) "" = None
  **)

  let averageSubf i len a =
    let first, sub_len = sub_start_and_length i len a in
    sumSubf i len a /. float sub_len
  (**T
    baverageSubf 0 10 (1--^|10) = baveragef (1--^|10)
    baverageSubf (-10) 10 (1--^|10) = baveragef (1--^|10)
    baverageSubf (-20) 20 (1--^|10) = baveragef (1--^|10)
    baverageSubf 0 3 (1--^|10) = baveragef (1--^|3)
    baverageSubf 3 3 (1--^|10) = baveragef (4--^|6)
    baverageSubf (-3) 3 (1--^|10) = baveragef (8--^|10)
    baverageSubf (-1) 3 (1--^|10) = baveragef (10--^|10)
    baverageSubf (-3) 1 (1--^|10) = baveragef (8--^|8)
    isNaN (baverageSubf 20 (-20) (1--^|10))
    isNaN  (baverageSubf (-20) 10 (1--^|10))
    isNaN  (baverageSubf 10 0 (1--^|10))
    isNaN  (baverageSubf 3 (-1) (1--^|10))

    baverageSubf 0 1 (1--^|1) = baveragef (1--^|1)
    isNaN  (baverageSubf 0 1 "")
  **)

  let averageSlice i j s =
    let i, len = slice_to_sub i j s in
    averageSub i len s
  (**T
    baverageSlice 0 10 (1--^|10) = baverage (1--^|10)
    baverageSlice 0 9 (1--^|10) = baverage (1--^|10)
    baverageSlice 0 (-1) (1--^|10) = baverage (1--^|10)
    baverageSlice (-10) 10 (1--^|10) = baverage (1--^|10)
    baverageSlice (-20) 20 (1--^|10) = baverage (1--^|10)
    baverageSlice (-20) 10 (1--^|10) = baverage (1--^|10)
    baverageSlice 0 3 (1--^|10) = baverage (1--^|4)
    baverageSlice 3 (-1) (1--^|10) = baverage (4--^|10)
    baverageSlice 3 3 (1--^|10) = baverage (4--^|4)
    baverageSlice (-1) (-1) (1--^|10) = baverage (10--^|10)
    optEx Division_by_zero (baverageSlice (-3) 3) (1--^|10) = None
    optEx Division_by_zero (baverageSlice (-3) 1) (1--^|10) = None
    optEx Division_by_zero (baverageSlice 20 (-20)) (1--^|10) = None
    optEx Division_by_zero (baverageSlice 10 0) (1--^|10) = None

    baverageSlice 0 1 (1--^|1) = baverage (1--^|1)
    optEx Division_by_zero (baverageSlice 0 1) "" = None
  **)

  let averageSlicef i j s =
    let i, len = slice_to_sub i j s in
    averageSubf i len s
  (**T
    baverageSlicef 0 10 (1--^|10) = baveragef (1--^|10)
    baverageSlicef 0 9 (1--^|10) = baveragef (1--^|10)
    baverageSlicef 0 (-1) (1--^|10) = baveragef (1--^|10)
    baverageSlicef (-10) 10 (1--^|10) = baveragef (1--^|10)
    baverageSlicef (-20) 20 (1--^|10) = baveragef (1--^|10)
    baverageSlicef (-20) 10 (1--^|10) = baveragef (1--^|10)
    baverageSlicef 0 3 (1--^|10) = baveragef (1--^|4)
    baverageSlicef 3 (-1) (1--^|10) = baveragef (4--^|10)
    baverageSlicef 3 3 (1--^|10) = baveragef (4--^|4)
    baverageSlicef (-1) (-1) (1--^|10) = baveragef (10--^|10)
    isNaN @@ baverageSlicef (-3) 3 (1--^|10)
    isNaN @@ baverageSlicef (-3) 1 (1--^|10)
    isNaN @@ baverageSlicef 20 (-20) (1--^|10)
    isNaN @@ baverageSlicef 10 0 (1--^|10)

    baverageSlicef 0 1 (1--^|1) = baveragef (1--^|1)
    isNaN @@ baverageSlicef 0 1 ""
  **)

  (* Random access *)

  let pick indices s =
    let l = len s in
    if PreList.exists (fun i -> i >= l || i < 0) indices then raise Not_found;
    PreList.map (fun i -> unsafe_get s i) indices
  (**T
    bpick [2; 3] ("foobar") = [ord 'o'; ord 'b']
    bpick [] "" = []
    bpick [] (1--^|10) = []
    bpick [0; 9] (1--^|10) = [1;  10]
    optNF (bpick [2;3]) "123"= None
    optNF (bpick [2;3]) ""= None
    optNF (bpick [-2;3]) (1--^|10) = None
  **)

  let pickWith funcs s = PreList.map (fun f -> f s) funcs
  (**T
    bpickWith [bfirst; blast] ("foobar") = [ord 'f'; ord 'r']
  **)

  let count f s = foldl (fun s i -> s + if f i then 1 else 0) 0 s
  (**T
    bcount ((=) (ord '/')) "" = 0
    bcount ((=) (ord '/')) "/foooo/bar//baz" = 4
  **)

  let concat = String.concat ""
  (**T
    bconcat [] = ""
    bconcat [1--^|5; 6--^|10] = 1--^|10
  **)


  (* Parallel combinators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l |> par_map ~process_count process |> combine
  (**T
    Bytestring.par_mapReduce ~combine:(bconcat) ~process:(bmap succ) (1--^|10) = bmap succ (1--^|10)
    Bytestring.par_mapReduce ~process_count:2 ~combine:(bconcat) ~process:(bmap succ) (1--^|10) = bmap succ (1--^|10)
    Bytestring.par_mapReduce ~process_count:2 ~combine:(bconcat @. reverse) ~process:(bmap succ) (1--^|10) = bmap succ ((6--^|10)^(1--^|5))
    Bytestring.par_mapReduce ~process_count:2 ~combine:(bconcat @. reverse) ~process:(bmap succ) "" = ""
    Bytestring.par_mapReduce ~process_count:2 ~combine:(bconcat @. reverse) ~process:(bmap succ) "1" = "2"
  **)

  let pmapReduce combine process = par_mapReduce ~combine ~process
  (**T
    Bytestring.pmapReduce (bconcat) (bmap succ) (1--^|10) = bmap succ (1--^|10)
    Bytestring.pmapReduce ~process_count:2 (bconcat) (bmap succ) (1--^|10) = bmap succ (1--^|10)
    Bytestring.pmapReduce ~process_count:2 (bconcat @. reverse) (bmap succ) (1--^|10) = bmap succ ((6--^|10)^(1--^|5))
    Bytestring.pmapReduce ~process_count:2 (bconcat @. reverse) (bmap succ) "" = ""
    Bytestring.pmapReduce ~process_count:2 (bconcat @. reverse) (bmap succ) "1" = "2"
  **)

  let pfoldl r f init = pmapReduce (PreList.foldl1 r) (foldl f init)
  (**T
    bpfoldl (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldl ~process_count:2 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldl ~process_count:2 (+) (+) 0 "1" = bsum "1"
    bpfoldl ~process_count:1 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldl ~process_count:1 (+) (+) 0 "1" = bsum "1"
    bpfoldl ~process_count:0 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldl ~process_count:0 (+) (+) 0 "1" = bsum "1"
    bpfoldl ~process_count:3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldl ~process_count:3 (+) (+) 0 "1" = bsum "1"
    bpfoldl ~process_count:2 (multiply) (flip (multiply)) 1 (1--^|10) = bproduct (1--^|10)
    bpfoldl ~process_count:2 (multiply) (flip (multiply)) 1 "1" = bproduct "1"
    optNF (bpfoldl ~process_count:2 (+) (+) 0) "" = Some (0)
  **)

  let pfoldl1 f = pmapReduce (PreList.foldl1 f) (foldl1 f)
  (**T
    bpfoldl1 (+) (1--^|10) = bsum (1--^|10)
    bpfoldl1 ~process_count:3 (+) (1--^|10) = bsum (1--^|10)
    bpfoldl1 ~process_count:2 (+) "1" = bsum "1"
    bpfoldl1 ~process_count:1 (+) (1--^|10) = bsum (1--^|10)
    bpfoldl1 ~process_count:0 (+) (1--^|10) = bsum (1--^|10)
    optNF (bpfoldl1 ~process_count:2 (+)) "" = None
  **)

  let pfoldr r f init = pmapReduce (PreList.foldr1 r) (foldr f init)
  (**T
    bpfoldr ~process_count:2 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldr ~process_count:2 (+) (+) 0 "1" = bsum "1"
    bpfoldr (+) (+) 0 "1" = bsum "1"
    bpfoldr ~process_count:1 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldr ~process_count:1 (+) (+) 0 "1" = bsum "1"
    bpfoldr ~process_count:0 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldr ~process_count:0 (+) (+) 0 "1" = bsum "1"
    bpfoldr ~process_count:3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldr ~process_count:3 (+) (+) 0 "1" = bsum "1"
    optNF (bpfoldr ~process_count:2 (+) (+) 0) "" = Some (0)
  **)

  let pfoldr1 f = pmapReduce (PreList.foldr1 f) (foldr1 f)
  (**T
    bpfoldr1 ~process_count:3 (+) (1--^|10) = bsum (1--^|10)
    bpfoldr1 (+) (1--^|10) = bsum (1--^|10)
    bpfoldr1 ~process_count:2 (+) "1" = bsum "1"
    bpfoldr1 ~process_count:1 (+) (1--^|10) = bsum (1--^|10)
    bpfoldr1 ~process_count:0 (+) (1--^|10) = bsum (1--^|10)
    optNF (bpfoldr1 ~process_count:2 (+)) "" = None
  **)

  let piter f = pmapReduce ignore (iter f)
  (**T
    bpiter ~process_count:3 (ignore @. succ) (1--^|10) = ()
    bpiter ~process_count:2 (ignore @. succ) (1--^|10) = ()
    bpiter ~process_count:1 (ignore @. succ) (1--^|10) = ()
    bpiter ~process_count:0 (ignore @. succ) (1--^|10) = ()
    bpiter ~process_count:3 (ignore @. succ) "1" = ()
    bpiter ~process_count:2 (ignore @. succ) "1" = ()
    bpiter ~process_count:1 (ignore @. succ) "1" = ()
    bpiter ~process_count:0 (ignore @. succ) "1" = ()
    bpiter ~process_count:3 (ignore @. succ) "" = ()
    bpiter ~process_count:2 (ignore @. succ) "" = ()
    bpiter ~process_count:1 (ignore @. succ) "" = ()
    bpiter ~process_count:0 (ignore @. succ) "" = ()
    bpiter (ignore @. succ) "" = ()
    bpiter (ignore @. succ) (1--^|10) = ()
    bpiter (ignore @. succ) "1" = ()
  **)

  let pmap f = pmapReduce (concat) (map f)
  (**T
    bpmap ~process_count:3 succ (1--^|10) = bmap succ (1--^|10)
    bpmap ~process_count:2 succ (1--^|10) = bmap succ (1--^|10)
    bpmap ~process_count:1 succ (1--^|10) = bmap succ (1--^|10)
    bpmap ~process_count:0 succ (1--^|10) = bmap succ (1--^|10)
    bpmap ~process_count:3 succ "1" = bmap succ "1"
    bpmap ~process_count:2 succ "1" = bmap succ "1"
    bpmap ~process_count:1 succ "1" = bmap succ "1"
    bpmap ~process_count:0 succ "1" = bmap succ "1"
    bpmap ~process_count:3 succ "" = bmap succ ""
    bpmap ~process_count:2 succ "" = bmap succ ""
    bpmap ~process_count:1 succ "" = bmap succ ""
    bpmap ~process_count:0 succ "" = bmap succ ""
    bpmap succ (1--^|10) = bmap succ (1--^|10)
    bpmap succ "" = bmap succ ""
    bpmap succ "1" = bmap succ "1"
  **)

  let pfilter f = pmapReduce (concat) (filter f)
  (**T
    bpfilter (even) ('0'--^'9') = "02468"
    bpfilter (odd) ('0'--^'9') = "13579"
    bpfilter (even) "1" = ""
    bpfilter (odd) "1" = "1"
    bpfilter (even) "" = ""
  **)

  let pfoldlSeqN ?process_count n r f init l =
    PreList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)
  (**T
    bpfoldlSeqN 3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldlSeqN ~process_count:2 3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldlSeqN ~process_count:2 3 (+) (+) 0 "1" = bsum "1"
    bpfoldlSeqN ~process_count:1 3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldlSeqN ~process_count:1 3 (+) (+) 0 "1" = bsum "1"
    bpfoldlSeqN ~process_count:0 3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldlSeqN ~process_count:0 3 (+) (+) 0 "1" = bsum "1"
    bpfoldlSeqN ~process_count:3 3 (+) (+) 0 (1--^|10) = bsum (1--^|10)
    bpfoldlSeqN ~process_count:3 3 (+) (+) 0 "1" = bsum "1"
    bpfoldlSeqN ~process_count:2 3 (multiply) (flip (multiply)) 1 (1--^|10) = bproduct (1--^|10)
    bpfoldlSeqN ~process_count:2 3 (multiply) (flip (multiply)) 1 "1" = bproduct "1"
    optNF (bpfoldlSeqN ~process_count:2 3 (+) (+) 0) "" = Some (0)
  **)

  let piterSeqN ?process_count n r f l =
    PreList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)
  (**T
    bpiterSeqN ~process_count:3 1 ignore succ (1--^|10) = ()
    bpiterSeqN ~process_count:2 2 ignore succ (1--^|10) = ()
    bpiterSeqN ~process_count:1 1 ignore succ (1--^|10) = ()
    bpiterSeqN ~process_count:0 4 ignore succ (1--^|10) = ()
    bpiterSeqN ~process_count:3 1 ignore succ "1" = ()
    bpiterSeqN ~process_count:2 6 ignore succ "1" = ()
    bpiterSeqN ~process_count:1 1 ignore succ "1" = ()
    bpiterSeqN ~process_count:0 1 ignore succ "1" = ()
    bpiterSeqN ~process_count:3 2 ignore succ "" = ()
    bpiterSeqN ~process_count:2 1 ignore succ "" = ()
    bpiterSeqN ~process_count:1 3 ignore succ "" = ()
    bpiterSeqN ~process_count:0 1 ignore succ "" = ()
    bpiterSeqN 0 ignore succ "" = ()
    bpiterSeqN 1 ignore succ (1--^|10) = ()
    bpiterSeqN 1 ignore succ "1" = ()
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
    bpinit (succ) 10 = (1--^|10)
    bpinit (pred @. add 2) 10 = (1--^|10)
    bpinit (succ) 0 = ""
    bpinit (succ) 1 = "\001"
    bpinit ~process_count:4 (succ) 10 = (1--^|10)
    bpinit ~process_count:3 (pred @. add 2) 10 = (1--^|10)
    bpinit ~process_count:2 (succ) 0 = ""
    bpinit ~process_count:1 (pred @. add 2) 10 = (1--^|10)
    bpinit ~process_count:1 (succ) 1 = "\001"
    bpinit ~process_count:0 (succ) 1 = "\001"
  **)

  let pzipWith ?process_count f a b =
    let process_count = max 1 (process_count |? !global_process_count) in
    let len = min (length a) (length b) in
    pinit ~process_count (fun i ->
      f (unsafe_get a i) (unsafe_get b i)
    ) len
  (**T
    bpzipWith (+) (1--^|10) (1--^|10) = bmap (dup (+)) (1--^|10)
    bpzipWith (-) (3--^|7) (3--^|1) = "\000\002\004"
    bpzipWith (-) (5--^|7) (5--^|1) = "\000\002\004"
    bpzipWith (+) "1" (1--^|10) = "2"
    bpzipWith (+) (1--^|10) "1" = "2"
    bpzipWith (+) "\001" "\001" = "\002"
    bpzipWith (+) "" (1--^|10) = ""
    bpzipWith (+) (1--^|10) "" = ""
    bpzipWith (+) "" "" = ""
    bpzipWith (+) ~process_count:3 (1--^|10) (1--^|10) = bmap (dup (+)) (1--^|10)
    bpzipWith (-) ~process_count:2 (3--^|7) (3--^|1) = "\000\002\004"
    bpzipWith (-) ~process_count:1 (5--^|7) (5--^|1) = "\000\002\004"
    bpzipWith (+) ~process_count:0 "1" (1--^|10) = "2"
  **)

  let par_mapReduceWithIndex ?process_count ~combine ~process l =
    let process_count = max 1 (process_count |? !global_process_count) in
    splitInto process_count l
      |> PreList.mapWithIndex tuple
      |> par_map  ~process_count process |> combine
  (**T
    Bytestring.par_mapReduceWithIndex ~process_count:5 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) ('0'--^'8') = "96521"
    Bytestring.par_mapReduceWithIndex ~process_count:5 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) "" = ""
    Bytestring.par_mapReduceWithIndex ~process_count:5 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) "0" = "1"
    Bytestring.par_mapReduceWithIndex ~process_count:0 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) "0" = "1"
    Bytestring.par_mapReduceWithIndex ~process_count:0 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) "" = ""
    Bytestring.par_mapReduceWithIndex ~process_count:0 ~combine:(breverse @. bconcat) ~process:(fun (l, idx) -> bmap succ (if odd idx then "" else l)) (0--^|9) = (10--^|1)
  **)

  let pmapReduceWithIndex combine process =
    par_mapReduceWithIndex ~combine ~process
  (**T
    bpmapReduceWithIndex ~process_count:5 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) ('0'--^'8') = "96521"
    bpmapReduceWithIndex ~process_count:5 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) "" = ""
    bpmapReduceWithIndex ~process_count:5 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) "0" = "1"
    bpmapReduceWithIndex ~process_count:0 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) "0" = "1"
    bpmapReduceWithIndex ~process_count:0 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) "" = ""
    bpmapReduceWithIndex ~process_count:0 (breverse @. bconcat) (fun (l, idx) -> bmap succ (if odd idx then "" else l)) (0--^|9) = (10--^|1)
  **)

  let pmapWithInit init f =
    pmapReduceWithIndex (concat) (fun (sublist, idx) -> map f (init sublist idx))
  (**T
    bpmapWithInit ~process_count:2 (fun l i -> if odd i then breverse l else l) succ (0--^|9) = (1--^|5) ^ (10--^|6)
    bpmapWithInit ~process_count:2 (fun l i -> if odd i then breverse l else l) succ "0" = "1"
    bpmapWithInit ~process_count:2 (fun l i -> if odd i then breverse l else l) succ "" = ""
    bpmapWithInit ~process_count:1 (fun l i -> if odd i then breverse l else l) succ "0" = "1"
    bpmapWithInit ~process_count:1 (fun l i -> if odd i then breverse l else l) succ "" = ""
    bpmapWithInit ~process_count:(-1) (fun l i -> if odd i then breverse l else l) succ "0" = "1"
    bpmapWithInit ~process_count:0 (fun l i -> if odd i then breverse l else l) succ "" = ""
  **)

end




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

let aall = PreArray.all
let aany = PreArray.any
let aallEqual = PreArray.allEqual
let aincludes = PreArray.includes
let ahas = PreArray.has
let aelem = PreArray.elem
let anotElem = PreArray.notElem
let anull = PreArray.null

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

let acount = PreArray.count

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

let sall = PreString.all
let sany = PreString.any
let sallEqual = PreString.allEqual
let sincludes = PreString.includes
let shas = PreString.has
let selem = PreString.elem
let snotElem = PreString.notElem
let snull = PreString.null

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

let scount = PreString.count

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

let extract = PreString.extract

let rexfind = PreString.rexfind
let rexfindOpt = PreString.rexfindOpt

let xfind = PreString.xfind
let xfindOpt = PreString.xfindOpt

let smatch = PreString.smatch
let rexmatch = PreString.rexmatch
let xmatch = PreString.xmatch

let findAllIndexes = PreString.findAllIndexes

let replace = PreString.replace
let rexreplace = PreString.rexreplace
let xreplace = PreString.xreplace

let frexreplace = PreString.frexreplace
let fxreplace = PreString.fxreplace

let quote = PreString.quote

let join = PreString.join
let joinArray = PreString.joinArray

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
let breplicate = Bytestring.replicate

let bmapToArray = Bytestring.mapToArray
let bmapToList = Bytestring.mapToList

let ball = Bytestring.all
let bany = Bytestring.any
let ballEqual = Bytestring.allEqual
let bincludes = Bytestring.includes
let bhas = Bytestring.has
let belem = Bytestring.elem
let bnotElem = Bytestring.notElem
let bnull = Bytestring.null

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
let bindexOf = Bytestring.indexOf

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

let bmaximum = Bytestring.maximum
let bmaximumBy = Bytestring.maximumBy
let bmaximumByWith = Bytestring.maximumByWith

let bminimum = Bytestring.minimum
let bminimumBy = Bytestring.minimumBy
let bminimumByWith = Bytestring.minimumByWith

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

let brange = Bytestring.range
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
let btakeUntil = Bytestring.takeUntil

let bdrop = Bytestring.drop
let bdropWhile = Bytestring.dropWhile
let bdropUntil = Bytestring.dropUntil

let bsplitAt = Bytestring.splitAt

let bbreak = Bytestring.break
let bspan = Bytestring.span

let binterlace = Bytestring.interlace

let breject = Bytestring.reject
let bwithout = Bytestring.without

let bpick = Bytestring.pick
let bpickWith = Bytestring.pickWith

let bcount = Bytestring.count

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



(** File opening wrappers *)
(* testing priority = low, wrappers of tested functions *)

let open_append = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666
let open_append_bin = open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o666

let fileExists = Sys.file_exists

let withFile filename f = finally close_in f (open_in_bin filename)
let withFileOut filename f = finally close_out f (open_out_bin filename)
let withFileAppend filename f = finally close_out f (open_append_bin filename)

let withFiles f infile outfile =
  withFile infile (fun ic -> withFileOut outfile (fun oc -> f ic oc))
let withFilesAppend f infile outfile =
  withFile infile (fun ic -> withFileAppend outfile (fun oc -> f ic oc))

let withUnixFile ?(flags=[Unix.O_RDONLY]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileOut ?(flags=[Unix.O_WRONLY;Unix.O_TRUNC;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileAppend ?(flags=[Unix.O_APPEND;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)

let withUnixFiles f infile outfile =
  withUnixFile infile (fun ic -> withUnixFileOut outfile (fun oc -> f ic oc))
let withUnixFilesAppend f infile outfile =
  withUnixFile infile (fun ic -> withUnixFileAppend outfile (fun oc -> f ic oc))


(** Common filesystem operations *)

(***
  ()
  (* specify some testing utils *)

  let fileTest f =
    mkdir_p Tests.data_dir;
    withCd Tests.data_dir (fun _ ->
      iter (ignoreE rm_r) (lsFull ".");
      let rv = catch f () in
      iter (ignoreE rm_r) (lsFull ".");
      match rv with
        | Error e -> raise e
        | Result v -> v
    )
**)

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
(***
  fileTest (fun () ->
    let fn = "foo" in
    touch fn;
    "fileUid fn = currentUid ()" @? (fileUid fn = currentUid ());
    rm fn
  )
**)

let isReadable fn = try Unix.access fn [Unix.R_OK]; true with _ -> false
let isWritable fn = try Unix.access fn [Unix.W_OK]; true with _ -> false
let isExecutable fn = try Unix.access fn [Unix.X_OK]; true with _ -> false

let atime fn = (stat fn).Unix.st_atime
let mtime fn = (stat fn).Unix.st_mtime
let ctime fn = (stat fn).Unix.st_ctime

let rename = Sys.rename
(***
  fileTest (fun () ->
    let f = "renameSrc" in
    let g = "renameDst" in
    touch f;
    "f exists" @? (fileExists f);
    "g doesn't" @? (not (fileExists g));
    rename f g;
    "g exists" @? (fileExists g);
    "f doesn't" @? (not (fileExists f));
    rm g
  )
**)

let ls d = PreArray.to_list (Sys.readdir d)
(***
  fileTest (fun () ->
    let fns = ["foo"; "bar"; "baz"] in
    "ls doesn't contain fns" @? (diff fns (ls ".") = fns);
    iter touch fns;
    "ls contains fns" @? (diff fns (ls ".") = [])
  )
**)
let rm = Sys.remove
(***
  fileTest (fun () ->
    let fn = "foo" in
    touch fn;
    "fn exists" @? (fileExists fn);
    rm fn;
    "fn doesn't exist" @? (not (fileExists fn))
  )
**)
let ln_s = Unix.symlink
let ln = Unix.link
let mkdir ?(perm=0o755) s = Unix.mkdir s perm
let rmdir = Unix.rmdir

let touch fn = withFileOut fn (fun _ -> Unix.utimes fn 0.0 0.0)
(***
  fileTest (fun () ->
    let fn = "foo" in
    "fn doesn't exist" @? (not (fileExists fn));
    touch fn;
    "fn exists" @? (fileExists fn);
    rm fn;
    "fn doesn't exist" @? (not (fileExists fn))
  )
**)

let getcwd = Sys.getcwd
let pwd = Sys.getcwd
let chdir = Unix.chdir
let cd = Unix.chdir

let withCd dir f =
  let od = getcwd () in
  finally (fun _ -> cd od) f (cd dir; dir)
(**T
  withCd "/" ((=) "/")
  withCd "/" (fun d -> getcwd () = "/")
  getcwd () != "/"
**)

let chmod perm filename = Unix.chmod filename perm
(***
  fileTest (fun () ->
    let fn = "foo" in
    touch fn;
    chmod 0o600 fn;
    "readable" @? (isReadable fn);
    "writable" @? (isWritable fn);
    chmod 0o400 fn;
    "readable" @? (isReadable fn);
    "not writable" @? (not @@ isWritable fn);
    "not executable" @? (not @@ isExecutable fn);
    chmod 0o500 fn;
    "readable" @? (isReadable fn);
    "not writable" @? (not @@ isWritable fn);
    "executable" @? (isExecutable fn);
    rm fn
  )
**)

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
let dirSeparator = sslice 1 (-2) ("a" ^/ "b")
(**T
  dirSeparator != ""
**)

let lsFull d = map ((^/) (expandPath d)) (ls d)
(**T
  all (startsWith (getcwd ())) (lsFull ".")
**)
let dirExists d = Sys.file_exists d && Sys.is_directory d
(**T
  dirExists "unit" = true
  dirExists "no_exist" = false
**)
let isRoot d =
  let fileInode fn = (Unix.stat fn).Unix.st_ino in
  let fileDevice fn = (Unix.stat fn).Unix.st_dev in
  fileInode d = fileInode "/" && fileDevice d = fileDevice "/"
(**T
  isRoot "/" = true
  isRoot "/tmp" = false
  withCd "/" (fun _ -> isRoot ".")
**)
let parentDirs d =
  let popdir d = first (nrsplit 2 "/" d) in
  let d = expandPath d in
  if d = "/"
  then ["/"]
  else generateUntil (eq "") popdir (expandPath d) @ ["/"]
(**T
  parentDirs "/tmp/foo/bar" = ["/tmp/foo/bar"; "/tmp/foo"; "/tmp"; "/"]
  parentDirs "/" = ["/"]
  parentDirs "" = parentDirs (getcwd ())
**)

let remove_trailing_slashes =
  rexreplace (rex (escape_rex "/" ^ "+$")) ""
let splitPath p = match p with
  | s when s = "/" -> ["/"]
  | p ->
    begin match split "/" (remove_trailing_slashes p) with
      | (""::t) -> "/"::t
      | ps -> ps
    end
(**T
  splitPath "" = []
  splitPath "/" = ["/"]
  splitPath "/home/foo/derr/" = ["/"; "home"; "foo"; "derr"]
**)
let joinPath ps = match ps with [] -> "" | l -> foldl1 (^/) l
(**T
  joinPath (splitPath "/foo/bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/") = "/foo"
  joinPath (splitPath "/foo") = "/foo"
  joinPath (splitPath "/") = "/"
  joinPath [] = ""

  joinPath (splitPath "/foo//bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/../bar/baz") = "/foo/../bar/baz"
**)
let relativePath path =
  let cp = splitPath (expandPath ".") in
  let pp = splitPath (expandPath path) in
  let cp, pp = dropWhile2 (=) cp pp in
  joinPath (replicate (len cp) ".." @ pp)
(**T
  relativePath "" = ""
  relativePath "." = ""
  relativePath "test" = "test"
  xmatch "^(\\.\\./)+" (relativePath "/")
**)
let dirname = Filename.dirname
let basename = Filename.basename

let rm_r fn =
  let rec aux dirs todo =
    match todo with
      | fn::t when isDir fn -> aux (fn::dirs) ((lsFull fn) @ t)
      | fn::t -> rm fn; aux dirs t
      | [] -> iter rmdir dirs in
  aux [] [fn]
(**T
  fileTest (fun () -> mkdir_p "foo/bar"; touch "foo/bar/baz"; rm_r "foo"; not (fileExists "foo"))
  fileTest (fun () -> mkdir_p "foo/bar"; touch "baz"; rm_r "foo"; fileExists "baz")
  fileTest (fun () -> touch "foo"; touch "baz"; rm_r "foo"; not (fileExists "foo") && fileExists "baz")
**)

let mkdir_p ?(perm=0o755) s =
  let nex, ex = span (not @. Sys.file_exists) (parentDirs s) in
  PreList.iter (mkdir ~perm) (reverse nex)
(**T
  fileTest (fun () -> mkdir_p "foo/bar/baz"; fileExists "foo/bar/baz" && isDir "foo/bar/baz")
  fileTest (fun () -> mkdir_p "foo"; fileExists "foo" && isDir "foo")
  fileTest (fun () -> mkdir_p "foo/"; fileExists "foo" && isDir "foo" && ls "foo" = [])
**)


(* File and IO operations *)

let output_endline oc line =
  output_string oc line; output_char oc '\n'; flush oc

let print_line = print_endline
let output_line = output_endline

let putStr = print_string
let putStrLn = print_endline

let outputs oc s =
  if endsWith "\n" s
  then (output_string oc s; flush oc)
  else output_line oc s
let puts s = outputs stdout s

let readLine = input_line
let readChar = input_char
let readByte = input_byte
let readInt ic = parseInt (readLine ic)
let readFloat ic = parseFloat (readLine ic)

let read ?buf bytes ch =
  let rec aux ch bytes c buf =
    match input ch buf c (bytes-c) with
      | 0 when c = 0 && bytes <> 0 -> raise End_of_file
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
(**Q
  Q.string (fun s -> s = fileTest (fun _ -> writeFile "foo" s; withFile "foo" (read (slen s))))
  Q.string (fun s -> s = fileTest (fun _ -> writeFile "foo" s; withFile "foo" (read ~buf:(screate (slen s)) (slen s))))
**)
(**T
  None = optE fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (read ~buf:(screate 2) 3))
  None = optE fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (read ~buf:(screate 3) 2))
  None = optE fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (read ~buf:(screate 0) (-1)))
  Some "bar" = optEOF fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (read ~buf:(screate 4) 4))

  None = optEOF fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (fun oc -> ignore (read ~buf:(screate 3) 3 oc); read 1 oc))
  Some "" = optEOF fileTest (fun _ -> writeFile "foo" "bar"; withFile "foo" (fun oc -> ignore (read ~buf:(screate 3) 3 oc); read 0 oc))
**)
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
(**Q
  Q.string (fun s -> s = fileTest (fun _ -> writeFile "foo" s; withFile "foo" readAll))
**)

let readFile filename = withFile filename readAll
(**Q
  Q.string (fun s -> s = fileTest (fun _ -> writeFile "foo" s; readFile "foo"))
**)
let writeFile filename str = withFileOut filename (fun oc -> output_string oc str)
(**Q
  Q.string (fun s -> s = fileTest (fun _ -> writeFile "foo" s; readFile "foo"))
**)
let appendToFile filename str = withFileAppend filename (fun oc -> output_string oc str)
(**Q
  Q.string (fun s -> s^(srev s) = fileTest (fun _ -> writeFile "foo" s; appendToFile "foo" (srev s); readFile "foo"))
**)

let readLines fn = lines (readFile fn)
(**Q
  Q.printable_string (fun s -> split "\n" s = fileTest (fun _ -> writeFile "foo" s; readLines "foo"))
**)

let tokenize f ic =
  unfoldlOpt (fun ic -> try Some (f ic, ic) with End_of_file -> None) ic
let tokenizeN t n ic = readN t n ic
let tokenizeIter t f ic = try while true do f (t ic) done with End_of_file -> ()
let tokenizeMap t f ic = tokenize (fun ic -> f (t ic)) ic
let rec tokenizeFold t f init ic =
  match (try Some (t ic) with End_of_file -> None) with
    | Some v -> tokenizeFold t f (f init v) ic
    | None -> init

let tokenizeFile t filename = withFile filename (fun ic -> tokenize t ic)
let tokenizeFileN t n fn = withFile fn (fun ic -> tokenizeN t n ic)

let iterLines f ic = tokenizeIter input_line f ic
let mapLines f ic = tokenizeMap input_line f ic
let foldLines f init ic = tokenizeFold input_line f init ic

let iterFileLines f fn = withFile fn (fun ic -> iterLines f ic)
let mapFileLines f fn = withFile fn (fun ic -> mapLines f ic)
let foldFileLines f init fn = withFile fn (fun ic -> foldLines f init ic)
(**T
  0 = fileTest (fun _ -> writeFile "foo" ""; foldFileLines (fun s _ -> s+1) 0 "foo")
**)
(**Q
  (Q.printable_string_of_size (fun _ -> Q.nng()+1)) (fun s -> fileTest (fun _ -> writeFile "foo" s; foldFileLines (fun s _ -> s+1) 0 "foo") = scount ((=) '\n') s + if endsWith "\n" s then 0 else 1)
**)


let withTempFile suffix f =
  let tmpfilename _ =
    "/tmp" ^/ (showInt (Random.int 1000000) ^ showFloat (timeNow ()) ^ "." ^ suffix) in
  let fn = (0--1000)
    |> find (fun i -> not (fileExists (tmpfilename i)))
    |> tmpfilename in
  finally (fun fn -> if fileExists fn then Sys.remove fn else ()) f fn
(**T
  not (fileExists (withTempFile "bar" (fun fn -> writeFile fn "hi"; fn)))
  not (fileExists (withTempFile "" (fun fn -> writeFile fn "hi"; fn)))
**)

let pipeWith f init i o = recurseOpt (f i o) init
let pipeChan f init i o = pipeWith (fun i o init -> optEOF (f i o) init) init i o
let unitPipe t f = t (fun ic () -> f ic, ())
let pipeTokenizer input output f ic oc init =
  let line, acc = f (input ic) init in
  output oc line;
  acc

let linePiper = pipeTokenizer input_line output_line
let blockPiper ?buf block_sz = pipeTokenizer (read ?buf block_sz) write

let pipeLines f = pipeChan (linePiper f)
let pipeBlocks block_sz f =
  let buf = String.create block_sz in
  pipeChan (blockPiper ~buf block_sz f)

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
(**Q
  Q.string (fun s -> fileTest (fun _ -> writeFile "foo" s; cp "foo" "bar"; readFile "bar" = s && readFile "foo" = s))
**)
let mv s d =
  try Sys.rename s d
  with Sys_error "Invalid cross-device link" -> cp s d; Sys.remove s
(**Q
  Q.string (fun s -> fileTest (fun _ -> writeFile "foo" s; mv "foo" "bar"; readFile "bar" = s && not (fileExists "foo")))
**)

let prependFile filename str =
  if fileSize filename > 32000000 (* use temp file if larger than 32 megs *)
  then withTempFile filename (fun fn ->
    withFileOut fn (fun oc -> write oc str; appendFileTo oc filename);
    mv fn filename)
  else writeFile filename (str ^ readFile filename)
(**Q
  Q.string (fun s -> ((srev s)^s = fileTest (fun _ -> writeFile "foo" s; prependFile "foo" (srev s); readFile "foo")))
**)


let shell_escape =
  let re = Pcre.regexp "'" in
  fun s ->
    if selem '\000' s
    then invalid_arg "Prelude.shell_escape: null byte in argument"
    else "'" ^ Pcre.replace ~rex:re ~templ:"'\\''" s ^ "'"
(**T
  shell_escape "" = "''"
  shell_escape " " = "' '"
  shell_escape "foo" = "'foo'"
  shell_escape "foo's" = "'foo'\\''s'"
  shell_escape "foo is fan/cy+some!" = "'foo is fan/cy+some!'"
  None = optE shell_escape "\000"
**)
(**Q
  Q.string (fun s -> let s = replace "\000" "\001" s in slen (shell_escape s) >= slen s)
  Q.string (fun s -> let s = replace "\000" "\001" s in (Some s = optE (fun s -> readRawCmd ("/bin/echo -n " ^ (shell_escape s))) s))
**)
let escape_cmd args = String.concat " " (PreList.map shell_escape args)
(**Q
  Q.string (fun s -> let s = replace "\000" "\001" s in (Some s = optE (readRawCmd @. escape_cmd) ["/bin/echo"; "-n"; s]))
  Q.string (fun s -> selem '\000' s ==> (None = optE (readRawCmd @. escape_cmd) ["/bin/echo"; "-n"; s]))
**)

exception Command_error of int * string
let command args =
  let cmd = escape_cmd args in
  let retcode = Sys.command cmd in
  if retcode <> 0 then
    raise (Command_error (retcode, (sprintf "Command failed with %d: %S" retcode cmd)))
  else
    ()
(**T
  None = optE command ["/bin/false"]
  Some () = optE command ["/bin/true"]
**)


(* Shell commands *)

let runCmd = command
let cmdCode args = try command args; 0 with Command_error (rv,_) -> rv

let withRawCmd cmd f =
  let ic,oc = Unix.open_process cmd in
  finally (fun _ -> maybeE () (fun x -> ignore (Unix.close_process x)) (ic, oc))
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

let bapar_init ?process_count ?layout kind f l =
  let ba = bacreateShared ?layout kind l in
  pforN ?process_count (fun i -> Bigarray.Array1.set ba i (f i)) l;
  ba
let bapinit = bapar_init


(* Hashtables *)

let string_hash_djb2 s =
  let rec aux s len v i =
    if i >= len then v
    else aux s len (((v lsl 5) + v) + (ord (suget s i))) (i+1) in
  aux s (slen s) 5381 0
(**T
  string_hash_djb2 "foobarbaalice" <> string_hash_djb2 "foobarbabob"
  string_hash_djb2 "foobarbaalice" = string_hash_djb2 "foobarbaalice"
**)

let string_hash_head =
  let ws = Sys.word_size / 8 - 1 in
  let rec aux sum s i =
    if i < 0 then sum
    else aux ((sum lsl 8) lor (ord (suget s i))) s (i-1) in
  fun s -> aux 0 s ((min (slen s) ws) - 1)
(**T
  string_hash_head "foobarbaalice" = string_hash_head "foobarbabob"
  string_hash_head "doobarbaalice" <> string_hash_head "foobarbabob"
**)

(* SHash is a Hashtbl with strings as keys.
   SHash uses string_hash_djb2 as the hash function.
*)
module SHash = Hashtbl.Make(struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let hash = string_hash_djb2
end)
(**T
  let h = SHash.create 10 in SHash.add h "foo" "bar"; SHash.find h "foo" = "bar"
  let h = SHash.create 10 in optNF (SHash.find h) "boo" = None
**)

(* HHash is a Hashtbl with hash strings as keys.
   HHash uses string_hash_head as the hash function.
*)
module HHash = Hashtbl.Make(struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let hash = string_hash_head
end)
(**T
  let h = HHash.create 10 in HHash.add h "foo" "bar"; HHash.find h "foo" = "bar"
  let h = HHash.create 10 in optNF (HHash.find h) "boo" = None
**)


(* Maps *)

module SMap = Map.Make(String)
(**T
  let h = SMap.empty in SMap.find "foo" (SMap.add "foo" "bar" h) = "bar"
  let h = SMap.empty in optNF (SMap.find "boo") h = None
**)
module IMap = Map.Make(struct type t = int let compare = (-) end)
(**T
  let h = IMap.empty in IMap.find 10 (IMap.add 10 "bar" h) = "bar"
  let h = IMap.empty in optNF (IMap.find 10) h = None
**)
