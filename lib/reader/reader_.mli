module Ast : module type of Ast
module Span : module type of Span

type checkpoint

exception Lexer_error of Span.t * string
exception Parser_error of Span.t * string

exception Needs_input of checkpoint
(** Needs_input errors can be continued by passing the checkpoint and
    a new lexbuf to [parse_from_checkpoint]. *)

type parser = Sedlexing.lexbuf -> Ast.t
(**
   @raise Lexer_error if a lexing error,
   @raise Parser_error if a parsing error,
   @raise Needs_input to register a need for extra input
*)

val parse : parser
(** Main parsing function. *)

val parse_from_checkpoint : checkpoint -> parser
(** Primarily for continuing a previous parse that threw [Needs_input _].
    See [Needs_input] documentation for details. *)
