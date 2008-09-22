(* Common filesystem operations *)

let rename = Sys.rename

let ls d = Array.to_list (Sys.readdir d)
let rm = Sys.remove
let cp s d = pipeFileBlocks 4096 tuple () s d
let mv s d =
  try rename s d
  with Sys_error "Invalid cross-device link" -> cp s d; rm s
let ln_s = Unix.symlink
let ln = Unix.link
let mkdir ?(perm=0o755) s = Unix.mkdir s perm
let rmdir = Unix.rmdir
let mkdir_p ?(perm=0o755) s =
  let nex, ex = span (not @. fileExists) (parentDirs s) in
  iter (mkdir ~perm) (reverse nex)

let getcwd = Sys.getcwd
let pwd = Sys.getcwd
let chdir = Unix.chdir
let cd = Unix.chdir

let chmod perm filename = Unix.chmod filename perm

let chownUid ?gid uid fn = 
  let gid = match gid with None -> fileGid fn | Some gid -> gid in
  Unix.chown fn uid gid

let chown ?group user fn =
  let gid = optMap groupGid group in
  chownUid ?gid (userUid user) fn

let chgrpGid gid fn = chownUid ~gid (fileUid fn) fn
let chgrp group fn = chgrpGid (groupGid group) fn
