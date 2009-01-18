open Prelude

let niter = 255
let limit = 4.0

let mandelbrot x y w h =
  let cr = 2.0 *. x /. w -. 1.5 in
  let ci = 2.0 *. y /. h -. 1.0 in
  let rec aux i zr zi = if i >= niter then 0 else
    let nzr = zr *. zr -. zi *. zi +. cr
    and nzi = 2.0 *. zr *. zi +. ci in
    if nzr *. nzr +. nzi *. nzi > limit then 255-i
    else aux (i+1) nzr nzi in
  aux 0 0.0 0.0

(*
  Draw a mandelbrot fractal onto a bytestring in parallel.
  The image is offset by xoff and yoff and has pixel dimensions of w * h.
*)
let draw_fractal xoff yoff w h =
  bpinit (fun i ->
    let y, x = quot_rem i w in
    mandelbrot (float (xoff+x)) (float (yoff+y)) (float w) (float h)
  ) (w*h)

(*
  Blur the bytestring image a using a two-pass box blur.
  The size of the box is controlled by the image b.
  Both passes are done with a parallel bytestring init.

  The time here seems to be dominated by IPC,
  so expect very small speedups (<10%.)
*)
let blend a b w h =
  let c = bpinit (fun i ->
    let y, x = quot_rem i w in
    let r = buget b i / 32 in
    let x1 = max (x-r) 0
    and x2 = min (x+r) (w-1) in
    baverageSub (y*w+x1) (x2-x1+1) a
  ) (slen a) in
  bpinit (fun i ->
    let y, x = quot_rem i w in
    let r = buget b i / 32 in
    let y1 = max (y-r) 0
    and y2 = min (y+r) (h-1) in
    baverage (bsubStride w (y1*w+x) (y2-y1+1) c)
  ) (slen a)

(*
  Draw two mandelbrot fractals and blend them together.
*)
let square_fractal d =
  let frac1 = draw_fractal 0 0 d d in
  let frac2 = draw_fractal (d/4) (-d/4) d d in
  blend frac1 frac2 d d

let bimg_to_arrays img w h =
  ainit (fun y ->
    ainit (fun x ->
      let c = buget img (y*w+x) in
      Graphics.rgb c c c
    ) w
  ) h

let () =
  let img = square_fractal 400 in
  Graphics.open_graph " 400x400";
  let gimg = Graphics.make_image (bimg_to_arrays img 400 400) in
  Graphics.draw_image gimg 0 0;
  puts "Press enter to exit";
  ignore (readLine stdin);
  Graphics.close_graph ()
