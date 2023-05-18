# Parscomb

![GitHub CI](https://github.com/Green-Wood/parscomb/actions/workflows/ci.yaml/badge.svg)

Parscomb is a lightweight, monadic parser combinator library for OCaml, created as a toy project to learn and explore monadic programming. It allows you to easily create, modify, and combine parsers for various text formats and languages. The library provides essential atomic operations, operators, and derived operations, as well as several useful parsers for common tasks.

## Features
- [x] Simple and composable parser combinators
- [x] Monadic and applicative interfaces for building complex parsers
- [x] A collection of useful parsers for common parsing tasks
- [x] Supports recursive parsing using the `fix` combinator
- [ ] Backtracking and error reporting (still working)

## Learning Monadic Programming
Parscomb serves as a practical example of monadic programming, showcasing the power and expressiveness of monads in the context of parser combinators. By studying the source code and examples, you can gain a deeper understanding of monads and how they can be used to model complex control flows and data manipulations in a functional programming language like OCaml.


## Install dependencies
```
opam switch create parscomb 4.13.1
eval $(opam env)
opam install --deps-only .
dune build
```

## Example

### Arithmetic Calculator
In `bin/main.ml`
```ocaml
open Parscomb.Parser
open Base
open Stdio

let int_num = integer >>| Int.of_string
let add = str "+" *> success ( + )
let minus = str "-" *> success ( - )
let mul = str "*" *> success ( * )
let div = str "/" *> success ( / )

let expr =
  fix ~f:(fun expr ->
      let factor = int_num <|> (str "(" *> expr <* str ")") in
      let term = chainl1 factor (mul <|> div) in
      chainl1 term (add <|> minus))
  <* eof

let rec main_loop () =
  printf ">>> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some s ->
      (match run expr s with
      | Error e -> print_endline e
      | Ok res -> printf "%d\n" res);
      main_loop ()

let () = main_loop ()
```
You can play with this simple calculator `./_build/default/bin/main.exe`:
```
>>> 1+2+3
6
>>> 1+2-3
0
>>> 1-2+3
2
>>> 1-(2+3)
-4
>>> 1+2*3
7
>>> (1+2)*3
9
>>> 1-2/3
1
>>> (1-2)/3
0
>>> -3/3
-1
```

### JSON Parser
Please refer to `lib/json.ml` for its implementation. 


## Acknowledges
We would like to acknowledge and express our gratitude to the authors and maintainers of the following libraries:
- [Base](https://github.com/janestreet/base): A standard library overlay provided by Jane Street, which offers a more extensive alternative to the default OCaml standard library.
- [Core](https://github.com/janestreet/core): A comprehensive system programming library provided by Jane Street, which extends Base with additional data structures, algorithms, and utilities.
- [ppx_jane](https://github.com/janestreet/ppx_jane): A set of syntax extensions provided by Jane Street, which includes various useful and convenient features, such as `let` syntax and deriving utilities.

## License
Parscomb is released under the MIT License.
