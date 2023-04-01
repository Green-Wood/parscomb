(* This module defines a monadic parser combinator library in OCaml.
   It provides basic atomic operations, operators, and derived operations to create and combine parsers.
   The module also includes a set of useful parsers for common parsing tasks.
*)

type 'a t
(* The type of a parser which parses a value of type ['a]. *)

(* Atomic operations *)

val str : string -> string t
(* [str s] is a parser that succeeds if it encounters the string [s] and returns [s]. *)

val bind : 'a t -> f:('a -> 'b t) -> 'b t
(* [bind p f] is a parser that runs parser [p], and if it succeeds, applies function [f] to the result and continues with the parser returned by [f]. *)

val success : 'a -> 'a t
(* [success x] is a parser that always succeeds and returns the value [x]. *)

val fail : string -> 'a t
(* [fail msg] is a parser that always fails with the error message [msg]. *)

val slice : 'a t -> string t
(* [slice p] is a parser that runs parser [p] and returns the consumed input as a string. *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(* [p1 <|> p2] is a parser that first tries parser [p1], and if it fails, backtracks and tries parser [p2]. *)

val any : char t
(* [any] is a parser that succeeds if it encounters any character and returns the character. *)

val eof : unit t
(* [eof] is a parser that succeeds if it encounters the end of the input. *)

val fix : f:('a t -> 'a t) -> 'a t
(* [fix f] is a parser that applies function [f] to itself recursively to allow for recursive parsers, such as JSON parser. *)

(* Operators *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(* [p >>= f] is an alias for [bind p f]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(* [let* p f] is an alias for [bind p f], which can be used with the [let*] syntax. *)

val ( <&> ) : 'a t -> 'b t -> ('a * 'b) t
(* [p1 <&> p2] is a parser that runs parser [p1] and then parser [p2] sequentially, and returns their results as a tuple [(x, y)] where [x] is the result of [p1] and [y] is the result of [p2]. *)

(* Derived operations *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(* [map p f] is a parser that runs parser [p] and applies function [f] to its result. *)

val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
(* [p >>| f] is an alias for [map p f]. *)

val satisfy : f:(char -> bool) -> char t
(* [satisfy f] is a parser that succeeds if it encounters a character [c] that satisfies the predicate [f c] and returns [c]. *)

val ( *> ) : 'a t -> 'b t -> 'b t
(* [p1 *> p2] is a parser that runs parser [p1], discards its result, and then runs parser [p2] and returns its result. *)

val ( <* ) : 'a t -> 'b t -> 'a t
(* [p1 <* p2] is a parser that runs parser [p1], saves its result, then runs parser [p2], discards its result, and finally returns the result of [p1]. *)

val between : 'a t -> 'b t -> 'c t -> 'c t
(* [between open_p close_p p] is a parser that runs the parsers [open_p], [p], and [close_p] sequentially, and returns the result of [p]. *)

val choice : 'a t list -> 'a t
(* [choice ps] is a parser that tries each parser in the list [ps] in order, and succeeds with the result of the first successful parser. *)

val opt : 'a t -> default:'a -> 'a t
(* [opt p default] is a parser that tries parser [p], and if it fails, returns the [default] value. *)

val optional : 'a t -> unit t
(* [optional p] is a parser that tries parser [p], and if it fails, succeeds with [()] (unit). *)

val many : 'a t -> 'a list t
(* [many p] is a parser that applies parser [p] zero or more times and returns a list of the results. *)

val many1 : 'a t -> 'a list t
(* [many1 p] is a parser that applies parser [p] one or more times and returns a list of the results. *)

val sep_by1 : 'a t -> sep:'b t -> 'a list t
(* [sep_by1 p sep] is a parser that applies parser [p] one or more times, separated by parser [sep], and returns a list of the results of [p]. *)

val sep_by : 'a t -> sep:'b t -> 'a list t
(* [sep_by p sep] is a parser that applies parser [p] zero or more times, separated by parser [sep], and returns a list of the results of [p]. *)

val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
(* [chainl1 p op] is a parser that take an 'a t parser, a binary operator function of type ('a -> 'a -> 'a) t to simulate a fold_left *)

(* Useful parsers *)

val space : char t
(* [space] is a parser that succeeds if it encounters a whitespace character and returns it. *)

val spaces : char list t
(* [spaces] is a parser that succeeds if it encounters zero or more whitespace characters and returns them as a list. *)

val digit : char t
(* [digit] is a parser that succeeds if it encounters a digit character and returns it. *)

val integer : string t
(* [integer] is a parser that succeeds if it encounters an integer and returns it as a string. *)

val number : string t
(* [number] is a parser that succeeds if it encounters a number (integer or floating-point) and returns it as a string. *)

val lexeme : 'a t -> 'a t
(* [lexeme p] is a parser that first consumes any leading whitespace and then runs parser [p] before consuming any trailing whitespace. *)

(* Run parser with given string *)

val run : 'a t -> string -> ('a, string) Result.t
(* [run p input] runs parser [p] on the input string [input] and returns either [Ok result] if the parsing is successful or [Error msg] if it fails. *)