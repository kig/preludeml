module List =
struct
  include List

  let reverse = rev

  let nth i l = List.nth l i
  let ($$) = List.nth

  let cons x xs = x::xs
  let head = function [] -> raise Not_found | (h::_) -> h
  let tail = function [] -> raise Not_found | (_::t) -> t
  let pop l =
    let rec aux l res =
      match l with
        | [] -> raise Not_found
        | (h::[]) -> (rev res, h)
        | (h::t) -> aux t (h :: res) in
    aux l []
  (**T
    pop [1;2;3] = ([1;2], 3)
  **)
  let popped l = fst (pop l)
  (**T
    popped [1; 2; 3] = [1; 2]
  **)
  let last l = snd (pop l)
  (**T
    last [1; 2; 3] = 3
  **)
  let first = head

  let shift l = (tail l, head l)
  let unshift = cons

  let map f l = rev (rev_map f l)

  let rec assocBy f l =
    match l with
      | [] -> raise Not_found
      | (k,v)::t when f k -> v
      | _::t -> assocBy f t

  let lookup e l = optNF (assoc e) l
  let lookupBy f e l = optNF (assocBy f e) l

  let len = length

  let all = for_all
  let any = exists

  let allEqual l = match l with
    | [] -> true
    | (h::t) -> all ((=) h) t

  let includes x = exists (fun y -> x = y)
  let has = includes
  let elem = includes
  let notElem x lst = not @@ elem x lst

  let indexOf x lst =
    let rec aux x c l = match l with
      | [] -> raise Not_found
      | (h::t) when x = h -> c
      | (h::t) -> aux x (c+1) t in
    aux x 0 lst
  (**T
    indexOf 'a' (explode "foobar") = 4
  **)
  let findIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h then c else aux p (c+1) t in
    aux p 0 lst
  (**T
    findIndex (gt 4) (0--9) = 5
  **)
  let findWithIndex p lst =
    let rec aux p c l = match l with
      | [] -> raise Not_found
      | (h::t) -> if p h then (h,c) else aux p (c+1) t in
    aux p 0 lst
  (**T
    findWithIndex (gt 4) (2--9) = (5,3)
  **)

  let null = function [] -> true | _ -> false

  let concatMap f l = concat (map f l)
  (**T
    concatMap ((--) 1) [1;2;3] = [1; 1; 2; 1; 2; 3]
  **)

  let pick indices l = map (flip nth l) indices
  (**T
    pick [2; 3] (explode "foobar") = ['o'; 'b']
  **)
  let pickWith funcs l = map ((|>) l) funcs
  (**T
    pickWith [first; last] (explode "foobar") = ['f'; 'r']
  **)

  let span f lst =
    let rec aux f res l = match l with
      | (h::t) when f h -> aux f (h::res) t
      | x -> (rev res, x) in
    aux f [] lst
  (**T
    span id [true; false; false; true] = ([true], [false; false; true])
    span (lessOrEqualTo 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
  **)
  let break p = span (not @. p)
  (**T
    break id [false; false; true; false] = ([false; false], [true; false])
    break (greaterThan 5) (1--10) = ([1; 2; 3; 4; 5], [6; 7; 8; 9; 10])
  **)

  let takeWhile f lst = fst @@ span f lst
  let take n lst =
    let rec aux c res l = match c, l with
        | x, (h::t) when x > 0 -> aux (c-1) (h::res) t
        | _ -> rev res in
    aux n [] lst

  let rec dropWhile f lst = match lst with
    | (h::t) when f h -> dropWhile f t
    | _ -> lst
  let rec drop n lst = match n, lst with
    | x, (h::t) when x > 0 -> drop (n-1) t
    | _ -> lst

  let rec dropWhile2 f a b = match a,b with
    | (x::xs), (y::ys) when f x y -> dropWhile2 f xs ys
    | _ -> a,b
  let rec drop2 n a b = match n,a,b with
    | c, (x::xs), (y::ys) when c > 0 -> drop2 c xs ys
    | _ -> a,b

  let splitAt n xs = (take n xs, drop n xs)
  (**T
    splitAt 3 (explode "foobar") = (['f'; 'o'; 'o'], ['b'; 'a'; 'r'])
  **)

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
  **)
  let slice first last lst =
    let len = if first < 0 || last < 0 then length lst else 0 in
    let first = if first < 0 then len + first else first in
    let last = if last < 0 then len + last else last in
    sub first (last-first+1) lst
  (**T
    slice 2 3 (explode "foobar") = ['o'; 'b']
    slice (-3) (-1) (explode "foobar") = ['b'; 'a'; 'r']
  **)

  let interlace elem l =
    let rec aux l l2 = match l with
        | [] -> (match l2 with [] -> [] | (h::t) -> List.rev t)
        | (h::t) -> aux t (elem :: h :: l2) in
    aux l []
  (**T
    interlace 0 [1; 2; 3] = [1; 0; 2; 0; 3]
    implode @@ interlace '-' @@ explode "abcde" = "a-b-c-d-e"
  **)

  let compact l = map (function Some x -> x | _ -> failwith "compact")
                      (filter ((!=) None) l)
  (**T
    compact [None; Some 10; Some 5; None; None; Some 8] = [10; 5; 8]
    compact @@ map (optIf (greaterThan 0) id) (-3--3) = [1; 2; 3]
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
  **)

  let sort ?(cmp=compare) l = List.sort cmp l
  let sortBy ?(cmp=compare) f l =
    map (fupler f) l |> sort ~cmp:(fun (_,a) (_,b) -> cmp a b) |> map fst
  let uniq ?cmp l = squeeze (sort ?cmp l)
  (**T
    uniq [3;1;2;2;2;3;3;1] = [1; 2; 3]
  **)

  let reject f l = filter (not @. f) l
  (**T
    reject (gt 4) (1--5) = (1--4)
  **)

  let without x l = filter ((<>) x) l
  (**T
    without 4 [1; 2; 4; 1; 2; 4] = [1; 2; 1; 2]
  **)

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
  **)

  let mapWindow f n l =
    let rec aux f wnd lst res =
      match lst with
        | [] -> rev res
        | (h::t) ->
          let wnd = tail wnd @ [h] in
          aux f wnd t ((f wnd) :: res) in
    let wnd, t = splitAt n l in
    aux f wnd t [f wnd]
  (**T
    mapWindow sum 1 (1--4) = (1--4)
    mapWindow sum 2 (1--4) = [3; 5; 7]
    mapWindow sum 3 (1--4) = [6; 9]
  **)

  let foldl = fold_left
  (**T
    foldl (+) 0 (1--10) = 55
    foldl (fun s b -> s ^ (string_of_int b)) "--" (1--3) = "--123"
  **)
  let foldl1 f l = foldl f (head l) (tail l)
  (**T
    foldl1 (+) (1--10) = 55
    foldl1 (fun s i -> s ^ i) ["foo"; "bar"; "baz"] = "foobarbaz"
  **)

  let foldr f s l = fold_right f l s
  (**T
    foldr (+) 0 (1--10) = 55
    foldr (fun a s -> s ^ (string_of_int a)) "--" (1--3) = "--321"
  **)
  let foldr1 f l = let l,i = pop l in foldr f i l
  (**T
    foldr1 (+) (1--10) = 55
    foldr1 (fun a s -> s ^ a) ["foo"; "bar"; "baz"] = "bazbarfoo"
  **)

  let scanl f init lst = rev @@ snd @@
    foldl (fun (s,l) i -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanl multiply 1 (2--5) = [1; 2; 6; 24; 120]
  **)
  let scanl1 f l = scanl f (head l) (tail l)
  (**T
    scanl1 multiply (1--5) = [1; 2; 6; 24; 120]
  **)

  let scanr f init lst = snd @@
    foldr (fun i (s,l) -> let s' = f s i in (s', s'::l)) (init, [init]) lst
  (**T
    scanr multiply 1 @@ [5;4;3;2] = [120; 24; 6; 2; 1]
  **)
  let scanr1 f l = let l,i = pop l in scanr f i l
  (**T
    scanr1 multiply @@ [5;4;3;2;1] = [120; 24; 6; 2; 1]
  **)


  let zipWith f a b =
    let rec aux f a b l = match a,b with
        | (x::xs), (y::ys) -> aux f xs ys ((f x y)::l)
        | _ -> l in
    rev @@ aux f a b []
  let zip a b = zipWith tuple a b
  let unzip = split

  let rec zipWith3 f a b c = match a,b,c with
    | (h1::t1), (h2::t2), (h3::t3) -> (f h1 h2 h3) :: (zipWith3 f t1 t2 t3)
    | _ -> []
  let zip3 a b c = zipWith3 tuple3 a b c
  let unzip3 l =
    foldr (fun (a,b,c) (t1,t2,t3) -> (a::t1, b::t2, c::t3)) ([],[],[]) l

  let iterWithIndex f l = ignore (foldl (fun j i -> f i j; j+1) 0 l)
  let each = iter
  let eachWithIndex = iterWithIndex
  let mapWithIndex f l =
    rev (snd (foldl (fun (j,r) i -> (j+1, (f i j)::r)) (0, []) l))

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
  **)

  let product lst = foldl ( * ) 1 lst
  let productf lst = foldl ( *. ) 1. lst
  let sum lst = foldl (+) 0 lst
  let sumf lst = foldl (+.) 0. lst
  let average lst = (sum lst) / (length lst)
  let averagef lst = (sumf lst) /. (float (length lst))

  let cycle n l =
    let rec aux c lst res =
      if c == 0 then res
      else match lst with
            | [] -> aux c l res
            | (h::t) -> aux (c-1) t (h::res) in
    match l with
      | [] -> invalid_arg "cycle"
      | _ -> reverse @@ aux n l []
  (**T
    cycle 5 (1--3) = [1; 2; 3; 1; 2]
    cycle 3 (1--10) = [1; 2; 3]
  **)

  let range s e =
    if s <= e
    then generateR (greaterOrEqualTo s) pred e
    else generateR (lessOrEqualTo s) succ e
  (**T
    range 1 3 = [1; 2; 3]
    range 1 1 = [1]
    range 1 0 = [1; 0]
  **)
  let init f n =
    let rec aux f n res =
      if n < 0 then res
      else aux f (n-1) ((f n) :: res) in
    aux f (n-1) []
  (**T
    init succ 10 = (1--10)
    init pred 10 = ((-1)--8)
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
  **)
  let (--) = range
  (**T
    (1--3) = [1; 2; 3]
    (1--1) = [1]
    (1--0) = [1; 0]
  **)

  let replicate n v = init (const v) n
  (**T
    replicate 5 '-' = ['-'; '-'; '-'; '-'; '-']
    replicate 0 '-' = []
    replicate (-1) '-' = []
  **)
  let times n l = concat (replicate n l)
  (**T
    times 3 [1; 2; 3] = [1; 2; 3; 1; 2; 3; 1; 2; 3]
  **)

  let maximum lst = foldl1 max lst
  (**T
    maximum [1;2;3;0;1;4;3;1] = 4
  **)
  let maxBy f a b = if (f a) >= (f b) then a else b
  let maximumBy f lst = foldl1 (maxBy f) lst
  let maximumByWith f lst = maximumBy snd (map (fupler f) lst)
  let minimum lst = foldl1 min lst
  (**T
    minimum [1;2;3;0;1;4;3;1] = 0
  **)
  let minBy f a b = if (f a) <= (f b) then a else b
  let minimumBy f lst = foldl1 (minBy f) lst
  let minimumByWith f lst = minimumBy snd (map (fupler f) lst)

  let groupsOf n l = if n <= 1 then [l]
    else unfoldlUntil null (splitAt n) l
  (**T
    groupsOf 3 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
  **)
  let splitInto n l = if n <= 1 then [l]
    else groupsOf (int (ceil (float (len l) /. float n))) l
  (**T
    splitInto 4 (1--10) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
  **)
  let groupBy p l =
    let rec aux p v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when p v h -> aux p v t (h::rl) res
      | (h::t) -> aux p h t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev (aux p h t [h] [])
  (**T
    groupBy (fun x y -> x*x = y*y) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
  **)
  let groupAs f l =
    let rec aux f v l rl res = match l with
      | [] -> (rev rl) :: res
      | (h::t) when (f h) = v -> aux f v t (h::rl) res
      | (h::t) -> aux f (f h) t [h] ((rev rl) :: res) in
    match l with [] -> []
      | (h::t) -> rev @@ aux f (f h) t [h] []
  (**T
    groupAs (fun x -> x*x) [-1; 1; -2; 2; 2; 1] = [[-1;1]; [-2;2;2]; [1]]
  **)
  let group l = groupAs id l
  (**T
    group [1;1;2;2;3;1] = [[1;1]; [2;2]; [3]; [1]]
  **)
  let count p l =
    let rec aux c p l = match l with
      | [] -> c
      | (h::t) -> aux (c + (if p h then 1 else 0)) p t in
    aux 0 p l
  (**T
    count (gt 5) (0--10) = 5
  **)
  let rotate n l =
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
  **)



  (* Parallel iterators *)

  let par_mapReduce ?process_count ~combine ~process l =
    let process_count = process_count |? !global_process_count in
    splitInto process_count l |> par_map ~process_count process |> combine

  let pmapReduce combine process = par_mapReduce ~combine ~process

  let pfoldl r f init = pmapReduce (foldl1 r) (foldl f init)
  let pfoldl1 f = pmapReduce (foldl1 f) (foldl1 f)
  let pfoldr r f init = pmapReduce (foldr1 r) (foldr f init)
  let pfoldr1 f = pmapReduce (foldr1 f) (foldr1 f)

  let piter f = pmapReduce ignore (iter f)
  let pmap f = pmapReduce concat (map f)
  let pfilter f = pmapReduce concat (filter f)

  let pfoldlSeqN ?process_count n r f init l =
    foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
          init (groupsOf n l)

  let pfoldl1SeqN ?process_count n f l =
    pfoldlSeqN ?process_count n f f (first l) (tail l)

  let piterSeqN ?process_count n r f l =
    iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)


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
    let len = min (len a) (len b) in
    let plen = int (ceil (float len /. float process_count)) in
    let aspl = groupsOf plen a in
    let bspl = groupsOf plen b in
    concat (par_map ~process_count (uncurry (zipWith f)) (zip aspl bspl))
end

let (--) = List.range
let (@*) l n = List.times n l
(**T
  [1; 2; 3] @* 3 = [1; 2; 3; 1; 2; 3; 1; 2; 3]
**)
