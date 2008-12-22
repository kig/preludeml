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
(* Parallel combinators *)

open PreCombinators
open PreExceptions
open PreOption
open PreConversions

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
