open Base

type parser_err = { msg : string } [@@deriving sexp, compare]
type 'a parser = Loc.t -> ('a * Loc.t, parser_err) Result.t

(* atomic operations *)
let str s loc =
  let remain = Loc.remain loc in
  if String.is_prefix remain ~prefix:s then
    Ok (s, Loc.advance loc (String.length s))
  else
    let msg = Printf.sprintf "Tried to match %s, but got %s" s remain in
    Error { msg }

let bind p ~f loc =
  let open Result.Let_syntax in
  let%bind a, loc' = p loc in
  f a loc'

let success a loc = Ok (a, loc)
let fail msg _ = Error { msg }

let slice p loc =
  let open Result.Let_syntax in
  let%bind _, loc' = p loc in
  success (Loc.slice ~before:loc ~after:loc') loc'

let choice p1 p2 loc =
  match p1 loc with Error _ -> p2 loc | Ok _ as ret -> ret

let any loc =
  let remain = Loc.remain loc in
  if String.length remain > 0 then Ok (String.get remain 0, Loc.advance loc 1)
  else
    let msg = Printf.sprintf "Expect to get any char, but got EOF" in
    Error { msg }

let eof loc =
  match any loc with
  | Ok _ ->
      let msg =
        loc |> Loc.remain |> Printf.sprintf "Expect to get EOF, but got %s"
      in
      Error { msg }
  | Error _ -> Ok ((), loc)

(* operators *)
let ( >>= ) p f = bind p ~f
let ( let* ) = ( >>= )
let ( <|> ) = choice

let ( <&> ) p1 p2 =
  let* r1 = p1 in
  let* r2 = p2 in
  success (r1, r2)

(* derived operations *)
let map p ~f = bind p ~f:(Fn.compose success f)
let ( >>| ) p f = map p ~f

(* The following implementation will cause stack over flow, because of non-laziness *)
(* let rec many p = map2 p (many p) ~f:(fun (r, rs) -> r :: rs) <|> success [] *)

let satisfy ~f =
  let* c = any in
  if f c then success c
  else fail (Printf.sprintf "Char %c isn't satisfiy function" c)

let ( *> ) p1 p2 =
  let* _ = p1 in
  p2

let ( <* ) p1 p2 =
  let* r = p1 in
  let* _ = p2 in
  success r

let between op ed x = op *> x <* ed
let opt p ~default = p <|> success default
let optional p = opt ~default:() (p *> success ())

let rec many p =
  opt ~default:[]
    (let* r = p in
     let* rs = many p in
     success (r :: rs))

let many1 p =
  let* r = p in
  let* rs = many p in
  success (r :: rs)

let sep_by1 p ~sep =
  let* r = p in
  let* rs = many (sep *> p) in
  success (r :: rs)

let sep_by p ~sep = opt ~default:[] (sep_by1 ~sep p)

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
let run par s = s |> Loc.from_str |> par |> Result.map ~f:(fun (res, _) -> res)
