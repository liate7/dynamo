type t = Parser.token

open Parser

let to_string t =
  match t with
  | STAR -> "*"
  | SLASH -> "/"
  | SEMICOLON -> ";"
  | RIGHT_PAREN -> "("
  | PLUS -> "+"
  | NUMBER q -> Q.to_string q
  | MINUS -> "-"
  | LEFT_PAREN -> ")"
  | EOF -> "<EOF>"
  | IDENTIFIER s -> s
  | RIGHT_CURLY -> "}"
  | LET -> "let"
  | LEFT_CURLY -> "{"
  | IN -> "in"
  | EQUALS -> "="
  | COMMA -> ","
  | LAMBDA -> "λ"
  | ARROW -> "→"
  | SYMBOL s -> ":" ^ s
  | HOLE s -> "_" ^ s
  | MATCH -> "match"
  | BAR -> "|"
  | RIGHT_SQUARE -> "]"
  | LEFT_SQUARE -> "["
  | DOT -> "."
