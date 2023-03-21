open Parscomb
open Core_bench

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

let t1 =
  Bench.Test.create ~name:"bench json parse" (fun () ->
      Parser.run Json.json_parser js_str)

let () = [ t1 ] |> Bench.make_command |> Command_unix.run