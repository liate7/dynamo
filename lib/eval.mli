open ContainersLabels
module Id : module type of Reader.Ast.Id
module Span : module type of Reader.Span

exception Runtime_nonfatal of Span.t * String.t

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
        closure :
          (Id.t * [ `Recurse of Value.t Option.t Ref.t | `Value of Value.t ])
          List.t;
        parameters : Reader.Ast.Pattern.t list;
        body : Resolver.expr;
      }

  val to_string : t -> String.t
end

and Dict : (Map.S with type key = Value.t)

module Env : sig
  type bindings =
    (Id.t * [ `Recurse of Value.t Option.t Ref.t | `Value of Value.t ]) List.t

  type t = { bindings : bindings }

  val empty : t
  val create : (Id.t * Value.t) List.t -> t
  val get : span:Span.t -> t -> Int.t * Id.t -> Value.t Option.t
end

val eval : Env.t -> Resolver.output -> Value.t
(** @raise Errors.Runtime_fatal for fatal runtime errors (eg, undefined variable reference)
    @raise Runtime_nonfatal for nonfatal runtime errors (things you can catch inside the language)
    @raise Errors.Miscompilation for errors caused by a miscompilation (eg, letrec machinery breaks)
*)

val std_prelude : (Id.t * Value.t) List.t
