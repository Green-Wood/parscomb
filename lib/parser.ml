open Base

type 'a t = Loc.t -> ('a * Loc.t, Parser_err.t) Result.t

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return a loc = Ok (a, loc)

  let bind p ~f loc =
    let%bind.Result a, loc' = p loc in
    f a loc'

  let map = `Define_using_bind
end)

(* atomic operations *)
let str s loc =
  let remain = Loc.remain loc in
  if String.is_prefix remain ~prefix:s then
    Ok (s, Loc.advance loc (String.length s))
  else
    let msg = Printf.sprintf "Tried to match %s, but got %s" s remain in
    Error (Parser_err.from msg loc)

let fail msg loc = Error (Parser_err.from msg loc)

let slice p loc =
  let%bind.Result _, loc' = p loc in
  return (Loc.slice ~before:loc ~after:loc') loc'

let ( <|> ) p1 p2 loc =
  match p1 loc with Error _ -> p2 loc | Ok _ as ret -> ret

let any loc =
  let remain = Loc.remain loc in
  if String.length remain > 0 then Ok (String.get remain 0, Loc.advance loc 1)
  else
    let msg = Printf.sprintf "Expect to get any char, but got EOF" in
    Error (Parser_err.from msg loc)

let eof loc =
  match any loc with
  | Ok _ ->
      let msg =
        loc |> Loc.remain |> Printf.sprintf "Expect to get EOF, but got %s"
      in
      Error (Parser_err.from msg loc)
  | Error _ -> Ok ((), loc)

let rec fix ~f loc = f (fix ~f) loc

(* operators *)
let ( let* ) = ( >>= )

let ( <&> ) p1 p2 =
  let* r1 = p1 in
  let* r2 = p2 in
  return (r1, r2)

(* derived operations *)

let satisfy ~f =
  let* c = any in
  if f c then return c
  else fail (Printf.sprintf "Char %c isn't satisfiy function" c)

let ( *> ) p1 p2 =
  let* _ = p1 in
  p2

let ( <* ) p1 p2 =
  let* r = p1 in
  let* _ = p2 in
  return r

let between op ed x = op *> x <* ed
let choice ps = List.fold ~init:(fail "") ~f:( <|> ) ps
let opt p ~default = p <|> return default
let optional p = opt ~default:() (p *> return ())

let rec many p =
  opt ~default:[]
    (let* r = p in
     let* rs = many p in
     return (r :: rs))

let many1 p =
  let* r = p in
  let* rs = many p in
  return (r :: rs)

let sep_by1 p ~sep =
  let* r = p in
  let* rs = many (sep *> p) in
  return (r :: rs)

let sep_by p ~sep = opt ~default:[] (sep_by1 ~sep p)

let chainl1 p op =
  let rec go acc =
    opt ~default:acc
      (let* f = op in
       let* a = p in
       go (f acc a))
  in
  p >>= go

(* useful parsers *)
let space = satisfy ~f:Char.is_whitespace
let spaces = many space
let digit = satisfy ~f:Char.is_digit
let integer = optional (str "-" <|> str "+") <&> many1 digit |> slice

let number =
  integer
  <&> optional (str ".")
  <&> many digit
  <&> optional (str "E" <|> str "e" <&> integer)
  |> slice

let lexeme p = between spaces spaces p

(* run parser with given string *)
let run par s =
  match s |> Loc.from_str |> par with
  | Ok (res, _) -> Ok res
  | Error e -> Error (Parser_err.err_msg e)
