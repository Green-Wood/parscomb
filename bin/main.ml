open Parscomb.Json
open Stdio

let () =
  let js =
    Object
      [
        ("name", String "greenwood");
        ("age", Number 18.);
        ("male", Boolean true);
        ("degrees", Array [ String "Bachelor"; String "Master" ]);
      ]
  in
  js |> to_json_string |> print_endline
