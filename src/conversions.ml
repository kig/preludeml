(* Conversions *)

let array = Array.of_list
let list = Array.to_list
let int = int_of_float
let char = char_of_int
let parseInt = int_of_string
let parseFloat = float_of_string
let showInt = string_of_int
let showFloat f =
  Pcre.replace ~rex:(Pcre.regexp "\\.$") ~templ:".0" (string_of_float f)
let charCode = int_of_char
let ord = int_of_char
let chr = char_of_int
