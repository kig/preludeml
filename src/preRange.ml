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
module Range =
struct
  open PreCombinators
  open PreList
  open PreArray

  let create s e = (s,e)

  let length (s,e) = max 0 (e-s+1)
  let len = length

  let to_list = uncurry List.range
  let to_array = uncurry Array.range

  let iter f (s, e) = for i = s to e do f i done
  let map f (s,e) = List.init (fun i -> f (s+i)) (len (s,e))
  let zipWith f (s,e) (s2,e2) =
    let l = min (len (s,e)) (len (s2,e2)) in
    List.init (fun i -> f (s+i) (s2+i)) l
  (**T
    Range.zipWith (+) (1-->10) (1-->11) = zipWith (+) (1--10) (1--11)
    Range.zipWith (/) (1-->10) (2-->11) = repeat 0 10
  **)
  let map2 = zipWith

  let zipWith3 f (s,e) (s2,e2) (s3,e3) =
    let l = min (min (len (s,e)) (len (s2,e2))) (len (s3,e3)) in
    List.init (fun i -> f (s+i) (s2+i) (s3+i)) l
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
      if s > e then List.rev res
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

  let pfoldl r f init = pmapReduce (List.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (List.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (List.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (List.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce List.concat (map f)
  let pfilter f = pmapReduce List.concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    List.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    List.iter (fun l -> List.iter r (pmap ?process_count f l)) (groupsOf n l)

  let pzipWith ?process_count f a b =
    let process_count = process_count |? !global_process_count in
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    List.concat (par_map ~process_count (uncurry (zipWith f)) (List.zip aspl bspl))

end

let (-->) = Range.create
