(* Time *)

let timeNow = Unix.gettimeofday
let timeZone = Netdate.localzone
let formatTime ?(zone=timeZone) fmt f = Netdate.format ~fmt (Netdate.create ~zone f)
let showTime = formatTime "%Y-%m-%d %H:%M:%S%z"
let showDate = formatTime "%Y-%m-%d"
let httpDate = formatTime ~zone:0 "%a, %d %b %Y %H:%M:%S GMT"

let second = 1.0
let minute = 60.0 *. second
let hour = 60.0 *. minute
let day = 24.0 *. hour
let week = 7.0 *. day
let month = 31.0 *. day
let year = 365.0 *. day

let sleep = Unix.sleep
