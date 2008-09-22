groupGid name = (Unix.getgrnam name).Unix.gr_gid
groupName gid = (Unix.getgrgid gid).Unix.gr_name
gidMembers gid = (Unix.getgrgid gid).Unix.gr_mem
groupMembers name = (Unix.getgrnam name).Unix.gr_mem

userUid name = (Unix.getpwnam name).Unix.pw_uid
userGid name = (Unix.getpwnam name).Unix.pw_gid
userGroup name = groupName (userGid name)
userGecos name = (Unix.getpwnam name).Unix.pw_gecos
userDir name = (Unix.getpwnam name).Unix.pw_dir
userShell name = (Unix.getpwnam name).Unix.pw_shell
userName uid = (Unix.getpwuid uid).Unix.pw_name
