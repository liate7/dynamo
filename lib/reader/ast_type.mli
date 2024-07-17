module Id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Literal : sig
  type 'elem t =
    | List of 'elem list
    | Dict of ([ `Bare of Id.t | `Computed of 'elem ] * 'elem) List.t
    | Number of Q.t
    | Symbol of Id.t
    | Unit

  val compare : ('elem -> 'elem -> int) -> 'elem t -> 'elem t -> int
  val equal : ('elem -> 'elem -> int) -> 'elem t -> 'elem t -> bool
  val to_string : ('elem -> string) -> 'elem t -> string
end

module Pattern : sig
  type t = Bind of Id.t | Hole of String.t | Literal of t Literal.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end

type expr = Span.t * expr'

and expr' =
  | Literal of expr Literal.t
  | Seq of expr list
  | Let of { bindings : (Pattern.t * expr) list; body : expr }
  | Var of Id.t
  | Appl of expr * expr list
  | Get of expr * expr
  | Lambda of { name : Id.t Option.t; params : Pattern.t list; body : expr }
  | Match of expr * (Pattern.t * expr) list

type t = expr
