open ContainersLabels

module Id = struct
  type t = String.t [@@deriving compare]

  let equal = String.equal
  let hash = String.hash
  let of_string str = str
  let to_string t = t
  let ( = ) = equal
end

module Literal = struct
  type 'elem t =
    | List of 'elem List.t
    | Dict of
        [ `Bare of Id.t * 'elem | `Computed of 'elem * 'elem | `Single of Id.t ]
        List.t
    | Number of Q.t
    | Symbol of Id.t
    | Unit
  [@@deriving compare]

  let equal elem_compare l r = compare elem_compare l r = 0

  let to_string elem_to_string t =
    match t with
    | List l ->
        "[" ^ (List.map ~f:elem_to_string l |> String.concat ~sep:", ") ^ "]"
    | Dict _d -> "brb"
    | Number q -> Q.to_string q
    | Symbol i -> Id.to_string i
    | Unit -> "()"
end

module Pattern = struct
  type t =
    | Bind of Id.t
    | Hole of String.t
    | Literal of t Literal.t
    | Exception of t * t
  [@@deriving compare]

  let equal l r = compare l r = 0

  let rec to_string t =
    match t with
    | Bind id -> Id.to_string id
    | Hole s -> "_" ^ s
    | Literal l -> Literal.to_string to_string l
    | Exception (tag, value) -> to_string tag ^ " ! " ^ to_string value
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
  | Match of expr * ([ `Value of Pattern.t | `Catch of Pattern.t ] * expr) list

type t = expr
