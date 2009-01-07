type ('a, 'b) types =
  | Int of int
  | Float of float
  | Char of char
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
  val negative : t
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

  let normal = 5
  let negative = -5
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


module FloatGen : (GENERATOR with type t = float) =
struct

  type t = float

  let name = "float"

  let normal = 5.
  let negative = 5.
  let zero = 0.
  let one = 1.
  let minus_one = -1.
  let even = 2.
  let odd = 3.
  let min_val = -.max_float
  let max_val = max_float

  let value_limit = 20
  let get_value = function
    | 0 -> (0, 0.0)
    | 1 -> (1, 0.0625)
    | i -> (1 lsl (i-1), 0.0625 *. float (1 lsl (i-1)))

end


module CharGen : (GENERATOR with type t = char) =
struct

  type t = char

  let name = "char"

  let normal = 'a'
  let negative = 'A'
  let zero = '\000'
  let one = '\001'
  let minus_one = '\n'
  let even = 'B'
  let odd = 'C'
  let min_val = '\000'
  let max_val = '\255'

  let value_limit = 255
  let get_value = function
    | 0 -> (0, '\000')
    | i -> (1, char_of_int i)

end


module StringGen : (GENERATOR with type t = string) =
struct

  type t = string

  let name = "string"

  let normal = "Foobar"
  let negative = "barFoo"
  let zero = ""
  let one = "a"
  let minus_one = "A"
  let even = "aB"
  let odd = "Abc"
  let min_val = "-25.0"
  let max_val =
    let s = String.create 256 in
    for i=0 to 255 do s.[i] <- char_of_int i done;
    s


  let value_limit = 20
  let get_value = function
    | 0 -> (0, "")
    | 1 -> (1, "a")
    | i -> (1 lsl (i-1), String.make (1 lsl (i-1)) 'a')

end


module OptionGen (G : GENERATOR) : (GENERATOR with type t = G.t option) =
struct

  type t = G.t option

  let name = "(" ^ G.name ^ " option)"

  let normal = Some G.normal
  let negative = Some G.negative
  let zero = Some G.zero
  let one = Some G.one
  let minus_one = Some G.minus_one
  let even = Some G.even
  let odd = Some G.odd
  let min_val = None
  let max_val = Some G.max_val

  let value_limit = 15
  let get_value = function
    | 0 -> (0, None)
    | i -> let n,v = G.get_value i in (n, Some v)
end


module ArrayGen (G : GENERATOR) : (GENERATOR with type t = G.t array) =
struct

  type t = G.t array

  let name = "(" ^ G.name ^ " array)"

  let normal = [|G.zero; G.negative; G.one; G.minus_one; G.even; G.odd; G.normal|]
  let negative =
    let l = Array.length normal in
    Array.init l (fun i -> normal.(l-1-i))
  let zero = [||]
  let one = [|G.one|]
  let minus_one = [|G.minus_one|]
  let even = [|G.one; G.even|]
  let odd = [|G.minus_one; G.zero; G.one|]
  let min_val = [|G.min_val|]
  let max_val = [|G.max_val|]

  let value_limit = 15
  let get_value = function
    | 0 -> (0, [||])
    | 1 -> (1, one)
    | i -> (1 lsl (i-1), Array.make (1 lsl (i-1)) G.one)
end


module ListGen (G : GENERATOR) : (GENERATOR with type t = G.t list) =
struct

  type t = G.t list

  let name = "(" ^ G.name ^ " list)"

  let normal = [G.zero; G.negative; G.one; G.minus_one; G.even; G.odd; G.normal]
  let negative = List.rev normal
  let zero = []
  let one = [G.one]
  let minus_one = [G.minus_one]
  let even = [G.one; G.even]
  let odd = [G.minus_one; G.zero; G.one]
  let min_val = [G.min_val]
  let max_val = [G.max_val]

  let value_limit = 15
  let get_value = function
     | 0 -> (0, [])
     | 1 -> (1, one)
     | i -> (1 lsl (i-1), Array.to_list (Array.make (1 lsl (i-1)) G.one))
end


type bm_stat = {
  time : float;
  minor_collections : int;
  major_collections : int;
  allocated_bytes : float;
}
type 'a exn_result = Result of 'a | Error of exn

let ex f v = try Result (f v) with e -> Error e
let ex_map f v = match v with Result x -> Result (f x) | Error e -> Error e

let alloc_diff =
  let b0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let t1 = Unix.gettimeofday () in
  let b1 = Gc.allocated_bytes () in
  ignore (t1 -. t0);
  b1 -. b0

let bm f = ex (fun v ->
    Gc.full_major ();
    Gc.compact ();
    let s0 = Gc.stat () in
    let b0 = Gc.allocated_bytes () in
    let t0 = Unix.gettimeofday () in
    let r = f v in
    let t1 = Unix.gettimeofday () in
    let b1 = Gc.allocated_bytes () in
    let s1 = Gc.stat () in
    ignore r;
    {
      time = t1 -. t0;
      minor_collections = s1.Gc.minor_collections - s0.Gc.minor_collections;
      major_collections = s1.Gc.major_collections - s0.Gc.major_collections;
      allocated_bytes = b1 -. b0 -. alloc_diff;
    }
  )

module type MEASURER =
  sig
    type t

    val measure : (t -> 'a) -> (string * t * 'a exn_result) list
    val benchmark : (t -> 'a) -> (int * int * bm_stat exn_result) list
  end;;

module Measurer (G : GENERATOR) : (MEASURER with type t = G.t) =
struct
  type t = G.t

  let measure f =
    List.map (fun (n,v,r) -> (G.name^" "^n, v, r)) [
      "normal", G.normal, ex f G.normal;
      "negative", G.negative, ex f G.negative;
      "zero", G.zero, ex f G.zero;
      "one", G.one, ex f G.one;
      "minus_one", G.minus_one, ex f G.minus_one;
      "even", G.even, ex f G.even;
      "odd", G.odd, ex f G.odd;
      "min_val", G.min_val, ex f G.min_val;
      "max_val", G.max_val, ex f G.max_val
    ]

  let benchmark f =
    let l = Array.to_list (Array.init (G.value_limit+1) (fun i -> i)) in
    List.map (fun i ->
      let n, v = G.get_value i in
      (i, n, bm f v)
    ) l
end

