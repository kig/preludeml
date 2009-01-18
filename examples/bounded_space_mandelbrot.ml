open Prelude

let niter = 64
let limit = 4.0

let mandelbrot z x y =
  let cr = 2.0 *. float x /. z -. 1.5 in
  let ci = 2.0 *. float y /. z -. 1.0 in
  let rec aux i zr zi = if i >= niter then 0 else
    let nzr = zr *. zr -. zi *. zi +. cr
    and nzi = 2.0 *. zr *. zi +. ci in
    if nzr *. nzr +. nzi *. nzi > limit
    then int (ceil (255. *. (float (niter-i) /. float niter)))
    else aux (i+1) nzr nzi in
  aux 0 0.0 0.0

let draw_scanline z w y =
  binit (fun x -> mandelbrot z x y) w

(*
  Computes batches of 256 scanlines in parallel,
  printing out each batch when done.

  Memory use here is bound to 256 * w
*)
let draw_fractal oc z w h =
  piterSeqN 256
    (fun s -> output_string oc s; flush oc)
    (fun y -> draw_scanline z w y)
    (0--(h-1))

let print_fractal w h oc =
  output_string oc (sprintf "P5 %i %i 255\n" w h); flush oc;
  draw_fractal oc (float (min w h)) w h

let () =
  let d = if alen Sys.argv > 1 then parseInt Sys.argv.(1) else 1000 in
  withFileOut "mandelbrot.pgm" (print_fractal d d)
