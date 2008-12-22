module Bytestring =
struct
  include PreString.String
  open PreOption
  open PreExceptions
  open PreCombinators
  open PreTuple
  open PreUnfolds
  open PreComparisons
  open PreInt
  open PreFloat
  open PreConversions
  open PreList
  open PreParallel


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

  let to_array s = Array.init (len s) (unsafe_get s)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)

  let to_list s = List.init (unsafe_get s) (len s)
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
    let j = max (i+len-1) (slen-1) in
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
    let j = max (i+len-1) (slen-1) in
    init (fun j -> f (unsafe_get s (i+j))) (j-i)

  let mapSlice i j f s =
    let i, len = slice_to_sub i j s in
    mapSub i len f s

  let foldlSub i len f init s =
    let rec aux f s v i j =
      if i > j then v else aux f s (f v (unsafe_get s i)) (i+1) j in
    let i = normalizeIndex i s in
    let slen = length s in
    let j = max (i+len-1) (slen-1) in
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
    let j = max (i+len-1) (slen-1) in
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
    if List.exists (gte l) indices then invalid_arg "pick: Index out of bounds";
    List.map (fun i -> unsafe_get s i) indices

  let pickWith funcs s = List.map (fun f -> f s) funcs

  let concat = String.concat ""


  (* Parallel combinators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (List.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (List.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (List.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (List.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce concat (map f)
  let pfilter f = pmapReduce concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    List.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    List.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)

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
end
