open Parser
open Sedlexing
open! ContainersLabels

exception Lexer_error of Span.t * String.t

let digit = [%sedlex.regexp? '0' .. '9']
let decimal = [%sedlex.regexp? '.', Plus digit]

let num =
  [%sedlex.regexp?
    ( Opt ('-' | '+'), (Plus digit, (Opt decimal | '/', Plus digit) | "inf")
    | "undef" )]

let identifier = [%sedlex.regexp? (id_start | '-'), Star (id_continue | '-')]
let comment = [%sedlex.regexp? "--", Star (Compl '\n')]

let rec read lexbuf =
  match%sedlex lexbuf with
  | Opt comment, '\n' ->
      new_line lexbuf;
      read lexbuf
  | Opt comment, eof -> EOF
  | Plus white_space -> read lexbuf
  | '+' -> PLUS
  | '-' -> MINUS
  | '/' -> SLASH
  | '*' -> STAR
  | ';' -> SEMICOLON
  | ',' -> COMMA
  | '!' -> BANG
  | '=' -> EQUALS
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  | '[' -> LEFT_SQUARE
  | ']' -> RIGHT_SQUARE
  | '{' -> LEFT_CURLY
  | '}' -> RIGHT_CURLY
  | '|' -> BAR
  | '.' -> DOT
  | "match" -> MATCH
  | "let" -> LET
  | "in" -> IN
  | "raised" -> RAISED
  | "λ" | "\\" -> LAMBDA
  | "->" | "→" -> ARROW
  | num -> NUMBER (Q.of_string @@ Utf8.lexeme lexbuf)
  | ':', identifier -> SYMBOL (String.drop 1 @@ Utf8.lexeme lexbuf)
  | '_', Opt identifier -> HOLE (String.drop 1 @@ Utf8.lexeme lexbuf)
  | identifier -> IDENTIFIER (Utf8.lexeme lexbuf)
  | _ ->
      raise
        (Lexer_error
           ( Sedlexing.lexing_positions lexbuf,
             "Unexpected char: " ^ Utf8.lexeme lexbuf ))
