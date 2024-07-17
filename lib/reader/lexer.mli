exception Lexer_error of Span.t * String.t

val read : Sedlexing.lexbuf -> Parser.token
