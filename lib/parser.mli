type 'a parser

(* atomic operations *)
val str : string -> string parser
val bind : 'a parser -> f:('a -> 'b parser) -> 'b parser
val success : 'a -> 'a parser
val fail : string -> 'a parser
val slice : 'a parser -> string parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val any : char parser
val eof : unit parser
val fix : f:('a parser -> 'a parser) -> 'a parser

(* operators *)
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( <&> ) : 'a parser -> 'b parser -> ('a * 'b) parser

(* derived operations *)
val map : 'a parser -> f:('a -> 'b) -> 'b parser
val ( >>| ) : 'a parser -> ('a -> 'b) -> 'b parser
val satisfy : f:(char -> bool) -> char parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser
val ( <* ) : 'a parser -> 'b parser -> 'a parser
val between : 'a parser -> 'b parser -> 'c parser -> 'c parser
val choice : 'a parser list -> 'a parser
val opt : 'a parser -> default:'a -> 'a parser
val optional : 'a parser -> unit parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val sep_by1 : 'a parser -> sep:'b parser -> 'a list parser
val sep_by : 'a parser -> sep:'b parser -> 'a list parser

(* useful parsers *)
val space : char parser
val spaces : char list parser
val digit : char parser
val integer : string parser
val number : string parser
val lexeme : 'a parser -> 'a parser

(* run parser with given string *)
val run : 'a parser -> string -> ('a, string) Result.t
