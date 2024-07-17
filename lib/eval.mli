open ContainersLabels
module Id : module type of Reader.Ast.Id
module Span : module type of Reader.Span

exception Runtime_fatal of Span.t * String.t
exception Runtime_nonfatal of Span.t * String.t

module Bindings : Map.S with type key = Id.t

module rec Value : sig
  type t =
    | Unit
    | Number of Q.t
    | Symbol of Id.t
    | List of t List.t
    | Dict of t Dict.t
    | Builtin of { arity : int; name : string; actual : Span.t -> t list -> t }
    | Lambda of {
        name : string;
        closure : t Bindings.t;
        parameters : Reader.Ast.Pattern.t list;
        body : Reader.Ast.expr;
      }

  val to_string : t -> String.t
end

and Dict : (Map.S with type key = Value.t)

module Env : sig
  type bindings = Value.t Bindings.t
  type t = { bindings : bindings }

  val empty : t
end

val eval : Env.t -> Reader.Ast.t -> Value.t
(** @raise Runtime_fatal for fatal runtime errors (eg, undefined variable reference)
    @raise Runtime_nonfatal for nonfatal runtime errors (things you can catch inside the language)
*)

val std_prelude : Env.t
