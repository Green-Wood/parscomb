open Base

type t = { input : string; offset : int }

let remain { input; offset } = String.subo input ~pos:offset
let from_str input = { input; offset = 0 }
let advance t i = { t with offset = t.offset + i }

let slice ~before ~after =
  let min_pos = min before.offset after.offset in
  let max_pos = max before.offset after.offset in
  String.sub before.input ~pos:min_pos ~len:(max_pos - min_pos)

let err_line { input; offset } =
  let str_to_err =
    String.subo input ~len:(min (String.length input) (offset + 1))
  in
  let line_no = String.count str_to_err ~f:(Char.equal '\n') + 1 in
  let line_start =
    String.rindex str_to_err '\n' |> Option.value_map ~f:(( + ) 1) ~default:0
  in
  let line_end =
    String.index_from input line_start '\n'
    |> Option.value ~default:(String.length input)
  in
  let line = String.sub input ~pos:line_start ~len:(line_end - line_start) in
  Printf.sprintf "%d| %s" line_no line