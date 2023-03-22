type t

val remain : t -> string
val from_str : string -> t
val advance : t -> int -> t
val slice : before:t -> after:t -> string
val err_line : t -> string
