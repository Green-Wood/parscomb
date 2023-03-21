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
  Parser.run Json.json_parser js_str
  |> Result.get_ok |> Json.to_json_string |> print_endline
