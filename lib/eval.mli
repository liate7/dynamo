open ContainersLabels
module Id : module type of Reader.Ast.Id
module Span : module type of Reader.Span

module rec Value : sig
  type t =
    | Unit
    | Number of Q.t
    | Symbol of Id.t
    | Exception of except
    | List of t List.t
    | Dict of t Dict.t
    | Builtin of { arity : int; name : string; actual : t Env.t -> t list -> t }
    | Lambda of {
        name : string;
        closure :
          (Id.t * [ `Recurse of Value.t Option.t Ref.t | `Value of Value.t ])
          List.t;
        parameters : Reader.Ast.Pattern.t list;
        body : Resolver.expr;
      }

  and except = { tag : t; value : t; stack : Span.t list option }

  val to_string : t -> String.t
end

and Dict : (Map.S with type key = Value.t)

and Env : sig
  type 'value bindings =
    (Id.t * [ `Recurse of 'value Option.t Ref.t | `Value of 'value ]) List.t

  type 'value t = { bindings : 'value bindings; stack : Span.t list }

  val empty : Value.t t
  val create : (Id.t * Value.t) List.t -> Value.t t
  val get : Value.t t -> Int.t * Id.t -> Value.t Option.t
end

exception Runtime_nonfatal of Value.except

val eval : Value.t Env.t -> Resolver.output -> Value.t
(** @raise Errors.Runtime_fatal for fatal runtime errors (eg, undefined variable reference)
    @raise Runtime_nonfatal for nonfatal runtime errors (things you can catch inside the language)
    @raise Errors.Miscompilation for errors caused by a miscompilation (eg, letrec machinery breaks)
*)

val std_prelude : (Id.t * Value.t) List.t
