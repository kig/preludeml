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
(* Running commands *)

open Printf
open PreExceptions
open PreIo

let shell_escape =
  let re = Pcre.regexp "(?=[^a-zA-Z0-9._+/-])" in
  Pcre.replace ~rex:re ~templ:"\\"

let escape_cmd args = String.concat " " (List.map shell_escape args)

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

let pipeCmd f init args = withCmd args (pipeChan f init)
let pipeCmdLines f init args = withCmd args (pipeLines f init)

let pipeRawCmd f init args = withRawCmd args (pipeChan f init)
let pipeRawCmdLines f init args = withRawCmd args (pipeLines f init)

let interactWithRawCmd f args = withRawCmd args (interactWith f)
let interactWithCmd f args = withCmd args (interactWith f)
