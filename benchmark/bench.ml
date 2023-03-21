open Core_bench
open Core

let json_folder = "./benchmark/json"

let build_json_test filename json_convert =
  let content = Stdio.In_channel.read_all filename in
  Bench.Test.create ~name:filename (fun () -> json_convert content)

let build_bench_command json_convert =
  Sys_unix.ls_dir json_folder
  |> List.map ~f:(fun fname ->
         build_json_test (Filename.concat json_folder fname) json_convert)
  |> Bench.make_command |> Command_unix.run
