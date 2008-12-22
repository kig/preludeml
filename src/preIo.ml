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
(* File and IO operations *)

open PreString
open PreExceptions
open PreCombinators
open PreConversions
open PreUnfolds
open PreTuple
open PrePath
open PreUser
open PreTime
open PreList
open PreList.List
open Printf

let putStr = print_string
let putStrLn = print_endline
let puts s = if String.rexmatch (String.rx "\n$") s
             then print_string s
             else print_endline s
let output_line oc line =
  output_string oc line;
  output_char oc '\n'

let readLine = input_line
let readChar = input_char
let readByte = input_byte
let readInt = readLine |>. parseInt
let readFloat = readLine |>. parseFloat

let open_append = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666
let open_append_bin = open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o666

let fileExists = Sys.file_exists

let withFile filename f = finally close_in f (open_in_bin filename)
let withFileOut filename f = finally close_out f (open_out_bin filename)
let withFileAppend filename f = finally close_out f (open_append_bin filename)

let withUnixFile ?(flags=[Unix.O_RDONLY]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileOut ?(flags=[Unix.O_WRONLY;Unix.O_TRUNC;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)
let withUnixFileAppend ?(flags=[Unix.O_APPEND;Unix.O_CREAT]) ?(perm=0o644) fn f =
  finally Unix.close f (Unix.openfile fn flags perm)


let read ?buf bytes ch =
  let rec aux ch bytes c buf =
    match input ch buf c (bytes-c) with
      | 0 when c = 0 -> raise End_of_file
      | 0 -> String.sub 0 c buf
      | b when c + b = bytes -> buf
      | b -> aux ch bytes (c+b) buf in
  let buf = match buf with
    | None -> String.create bytes
    | Some s ->
      if String.len s = bytes then s
      else invalid_arg (sprintf
                        "Prelude.read: buffer size %d differs from read size %d"
                        (String.len s) bytes) in
  aux ch bytes 0 buf

let write = output_string

let readAll ch =
  let rec aux ch ret buf =
    match input ch buf 0 4096 with
      | 0 -> Buffer.contents ret
      | b -> Buffer.add_substring ret buf 0 b;
             aux ch ret buf in
  let ret = Buffer.create 4096 in
  let buf = String.create 4096 in
  aux ch ret buf

let stat = Unix.stat

let fileSize filename = (stat filename).Unix.st_size

let fileKind fn = (stat fn).Unix.st_kind
let isKind kind fn = fileKind fn = kind
let isDir = isKind Unix.S_DIR
let isFile = isKind Unix.S_REG
let isLink = isKind Unix.S_LNK
let isFIFO = isKind Unix.S_FIFO
let isSocket = isKind Unix.S_SOCK
let isCharDev = isKind Unix.S_CHR
let isBlockDev = isKind Unix.S_BLK

let fileInode fn = (stat fn).Unix.st_ino
let filePermissions fn = (stat fn).Unix.st_perm
let fileDevice fn = (stat fn).Unix.st_dev
let fileUid fn = (stat fn).Unix.st_uid
let fileOwner fn = userName (fileUid fn)
let fileGid fn = (stat fn).Unix.st_gid
let fileGroup fn = groupName (fileGid fn)

let atime fn = (stat fn).Unix.st_atime
let mtime fn = (stat fn).Unix.st_mtime
let ctime fn = (stat fn).Unix.st_ctime

let readFile filename = withFile filename readAll
let writeFile filename str = withFileOut filename (flip output_string str)
let appendFile filename str = withFileAppend filename (flip output_string str)

let readLines = String.lines @. readFile

let tokenize t ic = unfoldlOpt (maybeEOF None (fun ic -> Some (t ic, ic))) ic
let tokenizeN t n ic = unfoldlN t n ic
let tokenizeIter t f ic = maybeEOF () (loop (f @. t)) ic
let tokenizeMap t f ic = tokenize (f @. t) ic
let tokenizeFile t filename = withFile filename (tokenize t)
let tokenizeFileN t n fn = withFile fn (tokenizeN t n)

let icEachLine f ic = tokenizeIter input_line f ic
let icMapLines f ic = tokenizeMap input_line f ic
let eachLine f = flip withFile (icEachLine f)
let mapLines f = flip withFile (icMapLines f)

let output_line_flush oc s = output_line oc s; flush oc


let withTempFile suffix f =
  let tmpfilename _ =
    "/tmp" ^/ (showInt (Random.int 1000000) ^ showFloat (timeNow ()) ^ "." ^ suffix) in
  let fn = (0--1000)
    |> find (fun i -> not (fileExists (tmpfilename i)))
    |> tmpfilename in
  finally (fun fn -> if fileExists fn then Sys.remove fn else ()) f fn


let pipeWith f init i o = recurseOpt (f i o) init
let pipeChan f = pipeWith (optEOF @.. f)
let unitPipe t f = t (fun ic () -> f ic, ())
let pipeTokenizer input output f ic oc init =
  let line, acc = f (input ic) init in
  output oc line;
  acc

let linePiper = pipeTokenizer input_line output_line_flush
let blockPiper ?buf block_sz = pipeTokenizer (read ?buf block_sz) write

let pipeLines f = pipeChan (linePiper f)
let pipeBlocks block_sz f =
  let buf = String.create block_sz in
  pipeChan (blockPiper ~buf block_sz f)

let withFiles f infile outfile =
  withFile infile (fun ic -> withFileOut outfile (fun oc -> f ic oc))
let withFilesAppend f infile outfile =
  withFile infile (fun ic -> withFileAppend outfile (fun oc -> f ic oc))

let pipeFiles f init = withFiles (pipeChan f init)
let pipeFileLines f init = withFiles (pipeLines f init)
let pipeFileBlocks block_sz f init = withFiles (pipeBlocks block_sz f init)

let pipeAppend f init = withFilesAppend (pipeChan f init)
let pipeAppendLines f init = withFilesAppend (pipeLines f init)
let pipeAppendBlocks block_sz f init = withFilesAppend (pipeBlocks block_sz f init)

let interactWith f = pipeChan (unitPipe linePiper f) ()
let interact f = interactWith f stdin stdout
let interactFiles f = pipeFiles (unitPipe linePiper f) ()
let interactAppend f = pipeAppend (unitPipe linePiper f) ()

let appendFileTo oc filename =
  withFile filename (fun ic -> pipeBlocks 4096 tuple () ic oc)

let cp s d = pipeFileBlocks 4096 tuple () s d
let mv s d =
  try Sys.rename s d
  with Sys_error "Invalid cross-device link" -> cp s d; Sys.remove s

let prependFile filename str =
  if fileSize filename > 32000000 (* use temp file if larger than 32 megs *)
  then withTempFile filename (fun fn ->
    withFileOut fn (fun oc -> write oc str; appendFileTo oc filename);
    mv fn filename)
  else writeFile filename (str ^ readFile filename)
