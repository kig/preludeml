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
(* Filesystem paths *)

open PreOption
open PreExceptions
open PreList
open PreString.String
open PreFilesystem
open PreUnfolds
open PreComparisons
open PreCombinators

(**T
(* Simple relative *)
  expandPath "foo" = (Filename.concat (Unix.getcwd ()) "foo")

(* Absolute *)
  expandPath "/foo" = "/foo"

(* /./ *)
  expandPath "/foo/./bar/./baz/./" = "/foo/bar/baz"

(* /. *)
  expandPath "/foo/bar/." = "/foo/bar"

(* /../ *)
  expandPath "/foo/../bar/../baz" = "/baz"

(* /../ 2 *)
  expandPath "/foo/../bar/../baz/../" = "/"

(* /.. *)
  expandPath "/foo/bar/.." = "/foo"

(* Mixed /./ and /../ *)
  expandPath "/foo/../bar/./baz/qux/./.." = "/bar/baz"

(* Trailing / (absolute) *)
  expandPath "/foo/" = "/foo"

(* Trailing / (relative) *)
  expandPath "foo/" = (Filename.concat (Unix.getcwd ()) "foo")

(* Root *)
  expandPath "/" = "/"

(* Current dir *)
  expandPath "" = (Unix.getcwd ())
**)
let expandPath path =
  let rec replace re tmpl s =
    let s' = Pcre.replace ~rex:(Pcre.regexp re) ~templ:tmpl s in
    if s = s' then s
              else replace re tmpl s' in
  let p1 = if not (Filename.is_relative path) then path
           else Filename.concat (Sys.getcwd ()) path in
  let p2 = replace "/\\.(/|$)" "/" p1 in
  let p3 = replace "/[^/]+/\\.\\.(/|$)" "/" p2 in
  if String.length p3 > 1
  then replace "/$" "" p3
  else p3

module Path =
struct
  type t = Path of string list

  let absolute a =
    let rec aux a lst = match a with
      | [] -> List.rev lst
      | (""::t) -> aux t [""]
      | (".."::t) -> aux t (maybeNF [] List.tail lst)
      | (h::t) -> aux t (h::lst) in
    aux a []
  let make s =
    let s = xreplace "/+" "/" s in
    let s = xreplace "/$" "" s in
    Path (split "/" s)
  let to_s (Path a) = if a = [""] then "/" else join "/" a

  let join_path (Path a) (Path b) = Path (absolute (a @ b))

  let join_list path ss = List.foldl join_path path (List.map make ss)
  let join path s = join_path path (make s)

  let join_list_to_s path ss = to_s (join_list path ss)
  let join_to_s path s = to_s (join path s)

  let expand path = make (expandPath (to_s path))
end
(**T
  Path.to_s (Path.make "/home") = "/home"
  Path.to_s (Path.make "/home/foo") = "/home/foo"
  Path.to_s (Path.make "/home/") = "/home"
  Path.to_s (Path.join (Path.make "/home/") "foo") = "/home/foo"
  Path.to_s (Path.join (Path.make "/home/") "/foo") = "/foo"
  Path.to_s (Path.join (Path.make "/home/") "..") = "/"
  Path.to_s (Path.join_list (Path.make "/home/") [".."; "tmp"]) = "/tmp"
  Path.join_to_s (Path.make "/home/") "/foo" = "/foo"
  Path.join_to_s (Path.make "/home/") ".." = "/"
  Path.join_list_to_s (Path.make "/home/") [".."; "tmp"] = "/tmp"
**)

let (^/) = Filename.concat
let dirExists d = Sys.file_exists d && Sys.is_directory d
let isRoot d =
  let fileInode fn = (Unix.stat fn).Unix.st_ino in
  let fileDevice fn = (Unix.stat fn).Unix.st_dev in
  fileInode d = fileInode "/" && fileDevice d = fileDevice "/"
let parentDirs d =
  generateUntil (eq "") (nrsplit "/" 2 |>. List.first) (expandPath d) @ ["/"]

let dirSeparator = slice 1 (-2) ("a" ^/ "b")
let splitPath p = match p with
  | "/" -> ["/"]
  | p ->
    begin match split dirSeparator p with
      | (""::t) -> "/"::t
      | ps -> ps
    end
let joinPath ps = List.foldl1 (^/) ps
(**T
  joinPath (splitPath "/foo/bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/") = "/foo"
  joinPath (splitPath "/foo") = "/foo"
  joinPath (splitPath "/") = "/"
**)
let relativePath path =
  let cp = splitPath (expandPath ".") in
  let pp = splitPath (expandPath path) in
  let cp, pp = List.dropWhile2 (=) cp pp in
  joinPath (List.replicate (List.len cp) ".." @ pp)

let dirname = Filename.dirname
let basename = Filename.basename

let mkdir_p ?(perm=0o755) s =
  let nex, ex = List.span (not @. Sys.file_exists) (parentDirs s) in
  List.iter (mkdir ~perm) (List.reverse nex)

