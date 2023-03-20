open Base

type t = { input : string; offset : int }

let remain { input; offset } = String.subo input ~pos:offset
let from_str input = { input; offset = 0 }
let advance t i = { t with offset = t.offset + i }

let slice ~before ~after =
  let min_pos = min before.offset after.offset in
  let max_pos = max before.offset after.offset in
  String.sub before.input ~pos:min_pos ~len:(max_pos - min_pos)
