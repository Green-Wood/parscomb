open Base
open Base_quickcheck
open Parser

type t =
  | Null
  | Boolean of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list
[@@deriving sexp, compare, quickcheck]

let token s = lexeme (str s)
let null_parser = token "null" *> success Null

let bool_parser =
  token "false" *> success (Boolean false)
  <|> token "true" *> success (Boolean true)

let num_parser = lexeme number >>| fun s -> Number (Float.of_string s)

let str_value =
  let json_str_char =
    str "\\\"" <|> slice (satisfy ~f:(Fn.non (Char.equal '"')))
  in
  let core_str =
    between (str "\"") (str "\"") (json_str_char |> many |> slice)
  in
  lexeme core_str

let str_parser = str_value >>| fun s -> String s

let json_value =
  fix ~f:(fun json ->
      let array_parser =
        token "[" *> sep_by ~sep:(str ",") json <* token "]" >>| fun ls ->
        Array ls
      in
      let obj_parser =
        let kv =
          let* key = str_value in
          let* _ = str ":" in
          let* value = json in
          success (key, value)
        in
        token "{" *> sep_by ~sep:(str ",") kv <* token "}" >>| fun ls ->
        Object ls
      in
      null_parser <|> bool_parser <|> num_parser <|> str_parser <|> array_parser
      <|> obj_parser)

let json_parser = json_value <* eof

let to_json_string ?(indent = 4) t =
  let rec aux n_indents = function
    | Null -> "null"
    | Boolean b -> Bool.to_string b
    | Number f -> Float.to_string f
    | String s -> "\"" ^ s ^ "\""
    | Array arr ->
        List.map arr ~f:(aux (n_indents + indent))
        |> format_ls ~n_indents ~op:"[" ~ed:"]"
    | Object obj ->
        List.map obj ~f:(fun (k, v) ->
            Printf.sprintf "\"%s\": %s" k (aux (n_indents + indent) v))
        |> format_ls ~n_indents ~op:"{" ~ed:"}"
  and format_ls ls ~n_indents ~op ~ed =
    let inner_indent = String.make (n_indents + indent) ' ' in
    let out_indent = String.make n_indents ' ' in
    let inner =
      ls |> List.map ~f:(fun s -> inner_indent ^ s) |> String.concat ~sep:",\n"
    in
    Printf.sprintf "%s\n%s\n%s%s" op inner out_indent ed
  in
  aux 0 t
