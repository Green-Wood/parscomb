open Parscomb
open Stdio

let js_str =
  {|{
    "name": "greenwood",
  "age" : 18,
  "male":true,
  "degrees": [
        "Bachelor",
    "Master",
      "No PhD"
  ]
}|}

let () =
  match Parser.run Json.json_parser js_str with
  | Ok t -> t |> Json.to_json_string |> print_endline
  | Error e -> e.msg |> print_endline
