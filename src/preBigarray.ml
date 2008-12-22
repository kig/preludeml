open PreIo
open PreString

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

let of_string s = bainit Bigarray.char (String.unsafe_get s) (String.len s)
let to_string ba = String.init (baget ba) (balen ba)

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

