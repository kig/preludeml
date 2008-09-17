(* Filesystem paths *)

(**T
(* Simple relative *)
  expand_path "foo" = (Filename.concat (Unix.getcwd ()) "foo")

(* Absolute *)
  expand_path "/foo" = "/foo"

(* /./ *)
  expand_path "/foo/./bar/./baz/./" = "/foo/bar/baz"

(* /. *)
  expand_path "/foo/bar/." = "/foo/bar"

(* /../ *)
  expand_path "/foo/../bar/../baz" = "/baz"

(* /../ 2 *)
  expand_path "/foo/../bar/../baz/../" = "/"

(* /.. *)
  expand_path "/foo/bar/.." = "/foo"

(* Mixed /./ and /../ *)
  expand_path "/foo/../bar/./baz/qux/./.." = "/bar/baz"

(* Trailing / (absolute) *)
  expand_path "/foo/" = "/foo"

(* Trailing / (relative) *)
  expand_path "foo/" = (Filename.concat (Unix.getcwd ()) "foo")

(* Root *)
  expand_path "/" = "/"

(* Current dir *)
  expand_path "" = (Unix.getcwd ())
**)
let expand_path path =
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
      | [] -> rev lst
      | (""::t) -> aux t [""]
      | (".."::t) -> aux t (maybeNF [] tail lst)
      | (h::t) -> aux t (h::lst) in
    aux a []
  let make s =
    let s = xreplace "/+" "/" s in
    let s = xreplace "/$" "" s in
    Path (split "/" s)
  let to_s (Path a) = if a = [""] then "/" else join "/" a

  let join_path (Path a) (Path b) = Path (absolute (a @ b))

  let join_list path ss = foldl join_path path (map make ss)
  let join path s = join_path path (make s)

  let join_list_to_s path ss = to_s (join_list path ss)
  let join_to_s path s = to_s (join path s)

  let expand path = make (expand_path (to_s path))
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

let expandPath = expand_path

let (^/) = Filename.concat
let dirExists d = fileExists d && isDir d
let isRoot d = fileInode d = fileInode "/" && fileDevice d = fileDevice "/"
let parentDirs d =
  generateUntil (eq "") (nrsplit "/" 2 |>. first) (expandPath d) @ ["/"]

let dirSeparator = sslice 1 (-2) ("a" ^/ "b")
let splitPath p = match p with
  | "/" -> ["/"]
  | p ->
    begin match split dirSeparator p with
      | (""::t) -> "/"::t
      | ps -> ps
    end
let joinPath ps = foldl1 (^/) ps
(**T
  joinPath (splitPath "/foo/bar/baz") = "/foo/bar/baz"
  joinPath (splitPath "/foo/") = "/foo"
  joinPath (splitPath "/foo") = "/foo"
  joinPath (splitPath "/") = "/"
**)
let relativePath path =
  let cp = splitPath (expandPath ".") in
  let pp = splitPath (expandPath path) in
  let cp, pp = dropWhile2 (=) cp pp in
  joinPath (replicate (len cp) ".." @ pp)

let dirname = Filename.dirname
let basename = Filename.basename

