open Parscomb
open Core_bench
open Core

let json_folder = "./benchmark/json"

let build_json_test filename =
  let content = Stdio.In_channel.read_all filename in
  Bench.Test.create ~name:filename (fun () ->
      Parser.run Json.json_parser content)

let () =
  Sys_unix.ls_dir json_folder
  |> List.map ~f:(fun fname ->
         build_json_test (Filename.concat json_folder fname))
  |> Bench.make_command |> Command_unix.run
