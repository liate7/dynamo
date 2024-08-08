module Span = Reader.Span

exception Compile_time_fatal of Span.t * String.t
exception Runtime_fatal of Span.t list * String.t
exception Miscompilation of Span.t * String.t
