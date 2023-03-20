open Parser

type t =
  | Null
  | Boolean of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list
[@@deriving sexp, compare, quickcheck]

val json_parser : t parser
val to_json_string : ?indent:int -> t -> string
