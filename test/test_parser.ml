open Core
open Parscomb.Parser

let str_eq = [%test_eq: (string, string) Result.t]

let%test_unit "string ok" =
  str_eq (run (str "hello") "hello world") (Ok "hello");
  str_eq (run (str "\\\"") {|\"|}) (Ok "\\\"")

let%test_unit "string err" =
  str_eq
    (run (str "w") "hello world")
    (Error "1| hello world\nTried to match w, but got hello world")

let%test_unit "bind" =
  let parser =
    let* s1 = str "hello" in
    let* s2 = str "world" in
    success (s1 ^ s2)
  in
  str_eq (run parser "helloworld") (Ok "helloworld")

let%test_unit "slice" =
  let parser = "hi" |> str |> many |> slice in
  str_eq (run parser "hihihiaa") (Ok "hihihi")

let%test_unit "choice" =
  let parser = str "hello" <|> str "world" in
  str_eq (run parser "hello") (Ok "hello");
  str_eq (run parser "world") (Ok "world");
  str_eq (run parser "err") (Error "1| err\nTried to match world, but got err")

let%test_unit "any" =
  Quickcheck.test ~sexp_of:[%sexp_of: string] [%quickcheck.generator: string]
    ~f:(fun s ->
      [%test_eq: (char, string) Result.t] (run any s)
        (if String.is_empty s then
         Error "1| \nExpect to get any char, but got EOF"
        else Ok (String.get s 0)))

let%test_unit "eof" =
  Quickcheck.test ~sexp_of:[%sexp_of: string] [%quickcheck.generator: string]
    ~f:(fun s ->
      let res = run eof s in
      if String.is_empty s then assert (Result.is_ok res)
      else assert (Result.is_error res))

let%test_unit "map int string to int" =
  Quickcheck.test ~sexp_of:[%sexp_of: int] [%quickcheck.generator: int]
    ~f:(fun n ->
      let s = Int.to_string n in
      let parser = str s >>| Int.of_string in
      [%test_eq: (int, string) Result.t] (run parser s) (Ok n))

let%test_unit "many" =
  let parser = "hi" |> str |> many in
  let eq_fn = [%test_eq: (string list, string) Result.t] in
  eq_fn (run parser "") (Ok []);
  eq_fn (run parser "1") (Ok []);
  eq_fn (run parser "hi") (Ok [ "hi" ]);
  eq_fn (run parser "hihi") (Ok [ "hi"; "hi" ])

let%test_unit "many multi" =
  let parser = "hi" |> str |> many in
  Quickcheck.test ~sexp_of:[%sexp_of: int] (Int.gen_incl 0 1000) ~f:(fun n ->
      let ls = List.init n ~f:(fun _ -> "hi") in
      let s = String.concat ls in
      [%test_eq: (string list, string) Result.t] (run parser s) (Ok ls))

let%test_unit "many1" =
  let parser = "hi" |> str |> many1 in
  let eq_fn = [%test_eq: (string list, string) Result.t] in
  eq_fn (run parser "") (Error "1| \nTried to match hi, but got ");
  eq_fn (run parser "hi") (Ok [ "hi" ]);
  eq_fn (run parser "hihi") (Ok [ "hi"; "hi" ])

let%test_unit "sep" =
  let parser = sep_by ~sep:(str ",") (str "hi") in
  let eq_fn = [%test_eq: (string list, string) Result.t] in
  eq_fn (run parser "") (Ok []);
  eq_fn (run parser "hi") (Ok [ "hi" ]);
  eq_fn (run parser "hi,hi") (Ok [ "hi"; "hi" ]);
  eq_fn (run parser "hi,hi,hi") (Ok [ "hi"; "hi"; "hi" ])

let%test_unit "chainl1" =
  let i = number >>| Int.of_string in
  let plus = str "+" *> success ( + ) in
  let parser = chainl1 i plus in
  let eq_fn = [%test_eq: (int, string) Result.t] in
  eq_fn (run parser "1") (Ok 1);
  eq_fn (run parser "1+2+3") (Ok 6);
  eq_fn (run parser "") (Error "1| \nExpect to get any char, but got EOF")

let%test_unit "right" =
  let parser = str "a" *> str "b" in
  str_eq (run parser "ab") (Ok "b")

let%test_unit "left" =
  let parser = str "a" <* str "b" in
  str_eq (run parser "ab") (Ok "a")

let%test_unit "between" =
  let parser = between (str "a") (str "b") any in
  [%test_eq: (char, string) Result.t] (run parser "acb") (Ok 'c')

let%test_unit "satisfy digit" =
  let parser = satisfy ~f:Char.is_digit in
  [%test_eq: (char, string) Result.t] (run parser "1") (Ok '1');
  [%test_eq: (char, string) Result.t] (run parser "a")
    (Error "1| a\nChar a isn't satisfiy function")

let%test_unit "integer" =
  Quickcheck.test ~sexp_of:[%sexp_of: int] [%quickcheck.generator: int]
    ~f:(fun n ->
      let s = Int.to_string n in
      [%test_eq: (string, string) Result.t] (run integer s) (Ok s))

let%test_unit "float" =
  Quickcheck.test ~sexp_of:[%sexp_of: float] Float.gen_finite ~f:(fun n ->
      let s = Float.to_string n in
      [%test_eq: (string, string) Result.t] (run number s) (Ok s))

let%test_unit "number" =
  str_eq (run number "1") (Ok "1");
  str_eq (run number "1.") (Ok "1.");
  str_eq (run number "1.1") (Ok "1.1");
  str_eq (run number "11.12") (Ok "11.12");
  str_eq (run number "11.12a") (Ok "11.12");
  str_eq (run number "") (Error "1| \nExpect to get any char, but got EOF")
