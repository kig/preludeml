(** Adapted from the Jane Street Capital Core quickcheck.ml,
    licensed as LGPL + linking exception *)
(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)

open Printf

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold_left (+) 0

let (==>) b1 b2 = if b1 then b2 else true (* could use too => *)

(*------------------------------------------------------------------------------*)
(* generator *)
(*------------------------------------------------------------------------------*)
type 'a gen = unit -> 'a

let fg () = 
  exp (Random.float 15. *. (if Random.float 1. < 0.5 then 1. else -1.))
  *. (if Random.float 1. < 0.5 then 1. else -1.)

let pfg () = abs_float (fg ())

(* natural number generator *)
let nng () = 
  let p = Random.float 1. in
  if p < 0.5 then Random.int 10
  else if p < 0.75 then Random.int 100
  else if p < 0.95 then Random.int 1_000
  else Random.int 10_000

(* Uniform random int generator *)
let uig () = Random.bits () - max_int / 2

let lg gen ?(size_gen=nng) () =
  foldn ~f:(fun acc _ -> (gen ())::acc) ~init:[] (size_gen ())

let ag gen ?(size_gen=nng) () =
  Array.init (size_gen ()) (fun _ -> gen ())

let pg gen1 gen2 () = (gen1 (), gen2 ())

let tg g1 g2 g3 () = (g1 (),g2 (), g3 ())

let cg () = char_of_int (Random.int 255)

let sg ?(char_gen = cg) ?(size_gen = nng) () = 
  let s = String.create (size_gen ()) in
  for i = 0 to String.length s - 1 do
    s.[i] <- char_gen ()
  done;
  s

(** given a list, returns generator that picks at random from list *)
let oneofl xs () = 
  List.nth xs (Random.int (List.length xs))

(* CRv2 rdouglass: this is a dubplicate of OUnit_utils.oneof *)
(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof xs =
  List.nth xs (Random.int (List.length xs))

(** generator that always returns given value *)
let always x () = x

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequency xs = 
  let sums = sum_int (List.map fst xs) in
  let i = Random.int sums in
  let rec aux acc = function 
    | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs 
    | _ -> failwith "frequency" 
  in
  aux 0 xs

(** like frequency, but returns generator *)
let frequencyl l = frequency (List.map (fun (i,e) -> (i,always e)) l)

(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output of [gen], and
    if [func] ever returns false, then the input that caused the failure is returned
    optionally.  *)
let rec laws iter gen func =
  if iter <= 0 then None
  else
    let input = gen () in
    try 
      if not (func input) then Some input 
      else laws (iter-1) gen func
    with _ -> Some input

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn ?pp name iter gen func =
  match laws iter gen func with
    None -> ()
  | Some i ->
    begin match pp with
      | Some f -> failwith (Printf.sprintf "law %s failed for %s" name (f i))
      | None -> failwith (Printf.sprintf "law %s failed" name)
    end

let rec statistic_number = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition (fun y -> y = x) xs in
    (1 + List.length splitg, x) :: statistic_number splitd

(* in percentage *)
let statistic xs =
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map fst stat_num) in
  List.map (fun (i, v) -> ((i * 100) / totals), v) stat_num
    
let laws2 iter func gen =
  let res = foldn ~init:[] iter 
    ~f:(fun acc _ -> let n = gen () in (n, func n) :: acc)
  in
  let stat = statistic (List.map (fun (_, (_, v)) -> v) res) in
  let res = List.filter (fun (_, (b, _)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd res)), stat)


let default_count = 500

let float ?(count=default_count) name law = laws_exn ~pp:string_of_float name count fg law
let ufloat ?(count=default_count) name law = laws_exn ~pp:string_of_float name count pfg law

let int ?(count=default_count) name law = laws_exn ~pp:string_of_int name count uig law
let uint ?(count=default_count) name law = laws_exn ~pp:string_of_int name count nng law

let char ?(count=default_count) name law = laws_exn ~pp:(sprintf "%C") name count cg law

let string ?(count=default_count) ?size_gen ?char_gen name law =
  laws_exn ~pp:(sprintf "%S") name count (sg ?size_gen ?char_gen) law

let list ?(count=default_count) ?size_gen name gen law =
  laws_exn name count (lg ?size_gen gen) law

let array ?(count=default_count) ?size_gen name gen law =
  laws_exn name count (ag ?size_gen gen) law

