open Parscomb.Parser
open Base
open Stdio

let int_num = integer >>| Int.of_string
let add = str "+" *> success ( + )
let minus = str "-" *> success ( - )
let mul = str "*" *> success ( * )
let div = str "/" *> success ( / )

let expr =
  fix ~f:(fun expr ->
      let factor = int_num <|> (str "(" *> expr <* str ")") in
      let term = chainl1 factor (mul <|> div) in
      chainl1 term (add <|> minus))
  <* eof

let rec main_loop () =
  printf ">>> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some s ->
      (match run expr s with
      | Error e -> print_endline e
      | Ok res -> printf "%d\n" res);
      main_loop ()

let () = main_loop ()