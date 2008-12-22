let groupGid name = (Unix.getgrnam name).Unix.gr_gid
let groupName gid = (Unix.getgrgid gid).Unix.gr_name
let gidMembers gid = (Unix.getgrgid gid).Unix.gr_mem
let groupMembers name = (Unix.getgrnam name).Unix.gr_mem

let userUid name = (Unix.getpwnam name).Unix.pw_uid
let userGid name = (Unix.getpwnam name).Unix.pw_gid
let userGroup name = groupName (userGid name)
let userGecos name = (Unix.getpwnam name).Unix.pw_gecos
let userDir name = (Unix.getpwnam name).Unix.pw_dir
let userShell name = (Unix.getpwnam name).Unix.pw_shell
let userName uid = (Unix.getpwuid uid).Unix.pw_name
