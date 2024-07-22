module Span = Reader.Span

exception Runtime_fatal of Span.t * String.t
exception Miscompilation of Span.t * String.t
