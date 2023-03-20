open Core
open Parscomb

let eq_json str js =
  [%test_eq: (Json.t, Parser.parser_err) Result.t]
    (Parser.run Json.json_parser str)
    js

let fail_json str =
  [%test_eq: bool] (Result.is_error (Parser.run Json.json_parser str)) true

open Json

let%test_unit "null" =
  eq_json "null" (Ok Null);
  eq_json "  null" (Ok Null);
  eq_json " \n null" (Ok Null);
  eq_json "null  " (Ok Null);
  fail_json "nuil"

let%test_unit "boolean" =
  eq_json "true" (Ok (Boolean true));
  eq_json "false" (Ok (Boolean false));
  eq_json " \t true " (Ok (Boolean true));
  eq_json "  false  " (Ok (Boolean false));
  fail_json "fal se"

let%test_unit "number" =
  Quickcheck.test ~sexp_of:[%sexp_of: float] Float.gen_finite ~f:(fun n ->
      let s = Float.to_string n in
      eq_json s (Ok (Number n)));
  eq_json " 3 " (Ok (Number 3.));
  eq_json " 3.1415926 " (Ok (Number 3.1415926));
  fail_json "3.E"

let check_str_leagal s =
  let res =
    String.fold_result s ~init:false ~f:(fun is_backslash c ->
        let open Char in
        if c = '"' && not is_backslash then Error () else Ok (c = '\\'))
  in
  match res with Error _ | Ok true -> false | _ -> true

let%test_unit "string" =
  Quickcheck.test ~sexp_of:[%sexp_of: string] String.quickcheck_generator
    ~f:(fun s ->
      let quote_s = "\"" ^ s ^ "\"" in
      if check_str_leagal s then eq_json quote_s (Ok (String s))
      else fail_json quote_s);
  eq_json {|"2ES8\"\000"|} (Ok (String {|2ES8\"\000|}));
  eq_json {|"\""|} (Ok (String {|\"|}));
  eq_json {|" 3 "|} (Ok (String " 3 "));
  eq_json {|""|} (Ok (String ""));
  fail_json {|"hello"world"|}

let%test_unit "array" =
  eq_json {|[1, 2, 3]|} (Ok (Array [ Number 1.; Number 2.; Number 3. ]));
  eq_json {| [ 1 ,2 ,3 ] |} (Ok (Array [ Number 1.; Number 2.; Number 3. ]));
  eq_json {|[1, "123", null, false]|}
    (Ok (Array [ Number 1.; String "123"; Null; Boolean false ]));
  eq_json {|[[1, 2, 3], 4, 5]|}
    (Ok
       (Array
          [ Array [ Number 1.; Number 2.; Number 3. ]; Number 4.; Number 5. ]));
  eq_json {|[]|} (Ok (Array []));
  eq_json "[\n\n]" (Ok (Array []));
  fail_json {|[1,]|}

let%test_unit "float array qtest" =
  let module G = Quickcheck.Generator in
  Quickcheck.test ~sexp_of:[%sexp_of: float list] (G.list Float.gen_finite)
    ~f:(fun ls ->
      let s =
        ls |> List.map ~f:Float.to_string |> String.concat ~sep:", " |> fun s ->
        "[" ^ s ^ "]"
      in
      let gt = ls |> List.map ~f:(fun n -> Number n) |> fun arr -> Array arr in
      eq_json s (Ok gt))

let%test_unit "object" =
  eq_json
    {|{
      "name": "greenwood",
      "age": 18,
      "male": true,
      "degrees": [
          "Bachelor",
          "Master"
      ]
    }|}
    (Ok
       (Object
          [
            ("name", String "greenwood");
            ("age", Number 18.);
            ("male", Boolean true);
            ("degrees", Array [ String "Bachelor"; String "Master" ]);
          ]))

let%expect_test "to json string" =
  Object
    [
      ("name", String "greenwood");
      ("age", Number 18.);
      ("male", Boolean true);
      ( "degrees",
        Array
          [
            String "Bachelor";
            Object
              [
                ("university", String "Peking University");
                ("years", Number 3.);
                ("isPhD", Boolean false);
              ];
          ] );
    ]
  |> to_json_string |> print_endline;
  [%expect
    {|
  {
      "name": "greenwood",
      "age": 18.,
      "male": true,
      "degrees": [
          "Bachelor",
          {
              "university": "Peking University",
              "years": 3.,
              "isPhD": false
          }
      ]
  }
  |}]

let%test_unit "json quicktest" =
  let rec leagal = function
    | Null -> true
    | Boolean _ -> true
    | Number n -> Float.is_finite n
    | String s -> check_str_leagal s
    | Array arr -> List.for_all arr ~f:leagal
    | Object obj ->
        List.for_all obj ~f:(fun (s, t) -> check_str_leagal s && leagal t)
  in
  Quickcheck.test ~sexp_of:[%sexp_of: t] [%quickcheck.generator: t] ~f:(fun t ->
      let s = to_json_string t in
      if leagal t then eq_json s (Ok t) else fail_json s)
