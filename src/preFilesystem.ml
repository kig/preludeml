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
(* Common filesystem operations *)

open PreCombinators
open PreList.List
open PreString

let rename = Sys.rename

let ls d = Array.to_list (Sys.readdir d)
let rm = Sys.remove
let ln_s = Unix.symlink
let ln = Unix.link
let mkdir ?(perm=0o755) s = Unix.mkdir s perm
let rmdir = Unix.rmdir

let getcwd = Sys.getcwd
let pwd = Sys.getcwd
let chdir = Unix.chdir
let cd = Unix.chdir

let chmod perm filename = Unix.chmod filename perm

let fileUid fn = (Unix.stat fn).Unix.st_uid
let fileGid fn = (Unix.stat fn).Unix.st_gid

let chownUid ?gid uid fn =
  let gid = match gid with None -> fileGid fn | Some gid -> gid in
  Unix.chown fn uid gid

let chown ?group user fn =
  let gid = optMap groupGid group in
  chownUid ?gid (userUid user) fn

let chgrpGid gid fn = chownUid ~gid (fileUid fn) fn
let chgrp group fn = chgrpGid (groupGid group) fn
