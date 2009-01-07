type ('a, 'b) types =
  | Int of int
  | Float of float
  | String of string
  | Option of 'a option
  | Array of 'a array
  | List of 'a list
  | Function of ('a -> 'b)

module type GENERATOR =
sig
  type t

  val name : string

  val normal : t
  val zero : t
  val one : t
  val minus_one : t
  val even : t
  val odd : t
  val min_val : t
  val max_val : t
  val value_limit : int

  val get_value : int -> (int * t)

end

module IntGen : (GENERATOR with type t = int) =
struct

  type t = int

  let name = "int"

  let normal = 10
  let zero = 0
  let one = 1
  let minus_one = -1
  let even = 2
  let odd = 3
  let min_val = min_int
  let max_val = max_int

  let value_limit = 20
  let get_value = function
    | 0 -> (0, 0)
    | 1 -> (1, 1)
    | i -> (1 lsl (i-1), 1 lsl (i-1))

end

module ArrayGen (G : GENERATOR) : (GENERATOR with type t = G.t array) =
struct

  type t = G.t array

  let name = "(" ^ G.name ^ " array)"

  let normal = [|G.zero; G.one; G.minus_one; G.even; G.odd; G.normal|]
  let zero = [||]
  let one = [|G.one|]
  let minus_one = [|G.minus_one|]
  let even = [|G.one; G.even|]
  let odd = [|G.minus_one; G.zero; G.one|]
  let min_val = [|G.min_val|]
  let max_val = [|G.max_val|]

  let value_limit = 16
  let get_value = function
    | 0 -> (0, [||])
    | 1 -> (1, one)
    | i -> (1 lsl (i-1), Array.make (1 lsl (i-1)) G.one)
end

type bm_stat = {
  time : float;
  minor_collections : int;
  major_collections : int
}
type bm_result = Result of bm_stat | Error of exn

let bm f v =
  try
    Gc.full_major ();
    Gc.compact ();
    let s0 = Gc.stat () in
    let t0 = Unix.gettimeofday () in
    let () = ignore (f v) in
    let t1 = Unix.gettimeofday () in
    let s1 = Gc.stat () in
    Result {
      time = t1 -. t0;
      minor_collections = s1.Gc.minor_collections - s0.Gc.minor_collections;
      major_collections = s1.Gc.major_collections - s0.Gc.major_collections;
    }
  with e -> Error e

module type MEASURER =
  sig
    type t

    val measure : (t -> 'a) -> (string * 'a) list
    val benchmark : (t -> 'a) -> (int * int * bm_result) list
  end;;

module Measurer (G : GENERATOR) : (MEASURER with type t = G.t) =
struct
  type t = G.t

  let measure f =
    List.map (fun (n,v) -> (G.name^"."^n, v)) [
      "normal", f G.normal;
      "zero", f G.zero;
      "one", f G.one;
      "minus_one", f G.minus_one;
      "even", f G.even;
      "odd", f G.odd;
      "min_val", f G.min_val;
      "max_val", f G.max_val
    ]

  let benchmark f =
    let l = Array.to_list (Array.init (G.value_limit+1) (fun i -> i)) in
    List.map (fun i ->
      let n, v = G.get_value i in
      (i, n, bm f v)
    ) l
end

