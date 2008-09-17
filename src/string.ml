module String =
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

  let times n a = replicate n a |> concat ""

  (* Iterators *)

  let iter f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) done
  let iterWithIndex f s =
    let l = len s in
    for i = 0 to l - 1 do f (unsafe_get s i) i done

  let map f s = init (fun i -> f (unsafe_get s i)) (len s)
  let mapWithIndex f s = init (fun i -> f (unsafe_get s i) i) (len s)

  let mapToList f s = List.init (fun i -> f (unsafe_get s i)) (len s)
  let mapToArray f s = Array.init (fun i -> f (unsafe_get s i)) (len s)

  (* Conversions *)

  let to_array s = Array.init (len s) (unsafe_get s)
  let of_array arr = init (Array.unsafe_get arr) (Array.length arr)

  let to_list s = List.init (unsafe_get s) (len s)
  let of_list l = of_array (Array.of_list l)

  let to_byte_array s = Array.init (len s) (fun i -> ord (unsafe_get s i))
  let of_byte_array a = init (fun i -> chr (Array.unsafe_get a i)) (Array.length a)

  let to_bigarray s = bainit Bigarray.char (unsafe_get s) (len s)
  let of_bigarray ba = init (baget ba) (balen ba)

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
    if List.exists (gte l) indices then invalid_arg "pick: Index out of bounds";
    List.map (fun i -> unsafe_get s i) indices

  let pickWith funcs s = List.map (fun f -> f s) funcs


  (* String specific *)

  let strip = Pcre.replace ~rex:(Pcre.regexp "^\\s+|\\s+$") ~templ:""

  let split ?n sep s = Pcre.split ?max:n ~pat:sep s
  let rsplit ?n sep s = List.rev (List.map rev (split ?n sep (rev s)))
  let nsplit sep n s = split ~n sep s
  let nrsplit sep n s = rsplit ~n sep s

  let rx = Pcre.regexp
  let rex = Pcre.regexp
  let escape_rex = Pcre.quote

  let rexsplit ?n rex s =
    List.map (function Pcre.Text s -> s | _ -> "") @@
    List.filter (function Pcre.Text _ -> true | _ -> false) @@
    Pcre.full_split ?max:n ~rex s
  let rexrsplit ?n rex s = List.rev (List.map rev (rexsplit ?n rex (rev s)))
  let xsplit ?n rexs s = rexsplit ?n (rx rexs) s
  let xrsplit ?n rexs s = rexrsplit ?n (rx rexs) s
  let xnsplit rexs n s = xsplit ~n rexs s
  let xnrsplit rexs n s = xrsplit ~n rexs s

  let rexscan rex s =
    try Array.to_list (Array.map Array.to_list (Pcre.extract_all ~rex s))
    with _ -> []
  let scan rexs s = rexscan (rx rexs) s

  let rexscan_nth rex n s =
    try
      let arr = Pcre.extract_all ~rex s in
      list (amap (fun a ->
        if alen a <= n
        then invalid_arg "Prelude.rexscan_nth: index out of bounds";
        a.(n)
      ) arr)
    with _ -> []
  let scan_nth rexs n s = rexscan_nth (rx rexs) n s

  let xfind x s = first (scan_nth x 0 s)
  let xfindOpt x s = optNF first (scan_nth x 0 s)

  let smatch pat = Pcre.pmatch ~pat
  let rexmatch rex = Pcre.pmatch ~rex
  let xmatch s = rexmatch (rx s)

  let replace pat templ = Pcre.replace ~pat ~templ
  let rexreplace rex templ = Pcre.replace ~rex ~templ
  let xreplace s = rexreplace (rx s)

  let frexreplace f rex s =
    let split = Pcre.full_split ~rex s in
    let processed = List.map (function
      | Pcre.Text s -> s
      | Pcre.Delim s -> f s
      | _ -> "") split in
    String.concat "" processed
  let fxreplace f s = frexreplace f (rx s)

  let quote l r s = l ^ s ^ r

  let join = String.concat
  let join_array s a = join s (Array.to_list a)

  let xreplaceMulti x_rep s =
    let pat = x_rep |> List.map (quote "(" ")" @. fst) |> join "|" in
    frexreplace (fun p -> assocBy (fun x -> xmatch x p) x_rep) (rex pat) s
  (**
    xreplaceMulti ["f.o","bar"; "b.r","foo"] "foobar" = "barfoo"
    xreplaceMulti ["f.o","bar"; "bar","foo"] "foobar" = "barfoo"
  **)

  let replaceMulti pat_rep s =
    let pat = pat_rep |> List.map fst |> List.map escape_rex |> join "|" in
    frexreplace (flip assoc pat_rep) (rex pat) s
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
      | [] -> (List.rev l, None)
      | (a::[]) -> (List.rev l, Some a)
      | (a::b::t) -> aux t ((a,b)::l) in
    let cleaned_split =
      Pcre.full_split ~rex s |>
      List.filter (function Pcre.Text _ | Pcre.Delim _ -> true | _ -> false) in
    let padded_split = match cleaned_split with
      | (Pcre.Delim _ :: t) -> (Pcre.Text "") :: cleaned_split
      | _ -> cleaned_split in
    let string_split =
      List.map (function Pcre.Text s | Pcre.Delim s -> s | _ -> "") padded_split in
    aux string_split []
  let xsplitPartition x s = rexsplitPartition (rex x) s


  (* Parallel operations *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (PList.foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (PList.foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (PList.foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (PList.foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce (concat "") (map f)
  let pfilter f = pmapReduce (concat "") (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    PList.foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let piterSeqN ?process_count n r f l =
    PList.iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)

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

end

let ( ^* ) = String.times
