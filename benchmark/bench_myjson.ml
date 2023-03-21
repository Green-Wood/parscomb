open Parscomb

let () = Bench.build_bench_command (Parser.run Json.json_parser)
