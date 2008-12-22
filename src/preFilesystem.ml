(* Common filesystem operations *)

open PreList.List
open PreCombinators
open PreUnfolds
open PreString
open PreOption
open PreUser

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
