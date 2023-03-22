type 'a t

(* atomic operations *)
val str : string -> string t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val success : 'a -> 'a t
val fail : string -> 'a t
val slice : 'a t -> string t
val ( <|> ) : 'a t -> 'a t -> 'a t
val any : char t
val eof : unit t
val fix : f:('a t -> 'a t) -> 'a t

(* operators *)
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( <&> ) : 'a t -> 'b t -> ('a * 'b) t

(* derived operations *)
val map : 'a t -> f:('a -> 'b) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val satisfy : f:(char -> bool) -> char t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <* ) : 'a t -> 'b t -> 'a t
val between : 'a t -> 'b t -> 'c t -> 'c t
val choice : 'a t list -> 'a t
val opt : 'a t -> default:'a -> 'a t
val optional : 'a t -> unit t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val sep_by1 : 'a t -> sep:'b t -> 'a list t
val sep_by : 'a t -> sep:'b t -> 'a list t

(* useful parsers *)
val space : char t
val spaces : char list t
val digit : char t
val integer : string t
val number : string t
val lexeme : 'a t -> 'a t

(* run parser with given string *)
val run : 'a t -> string -> ('a, string) Result.t
