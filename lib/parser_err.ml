open Base

type t = { msg : string; loc : Loc.t }

let err_msg { msg; loc } = Printf.sprintf "%s\n%s" (Loc.err_line loc) msg
let from msg loc = { msg; loc }
