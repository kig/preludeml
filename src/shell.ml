(* Running commands *)

let shell_escape =
  let re = Pcre.regexp "(?=[^a-zA-Z0-9._+/-])" in
  Pcre.replace ~rex:re ~templ:"\\"

let escape_cmd args = join " " (map shell_escape args)

exception Command_error of int * string
let command args =
  let cmd = escape_cmd args in
  let retcode = Sys.command cmd in
  if retcode <> 0 then
    raise (Command_error (retcode, (sprintf "Command failed with %d: %S" retcode cmd)))
  else
    ()

let runCmd = command
let cmdCode args = try command args; 0 with Command_error (rv,_) -> rv

let withRawCmd cmd f =
  let ic,oc = Unix.open_process cmd in
  finally (fun _ -> maybeE () close_out oc; maybeE () close_in ic)
          (f ic) oc
let withRawCmdStdin args f =
  withRawCmd args (fun ic oc -> maybeE () close_in ic; f oc)
let withRawCmdStdout args f =
  withRawCmd args (fun ic oc -> maybeE () close_out oc; f ic)

let withCmd args = withRawCmd (escape_cmd args)
let withCmdStdin args = withRawCmdStdin (escape_cmd args)
let withCmdStdout args = withRawCmdStdout (escape_cmd args)

let readCmd args = withCmdStdout args readAll
let readRawCmd args = withRawCmdStdout args readAll

