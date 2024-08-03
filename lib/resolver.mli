open Reader
module Literal = Ast.Literal
module Pattern = Ast.Pattern
module Id = Ast.Id

type input = Ast.expr

type expr = Span.t * expr'

and expr' =
  | Literal of expr Literal.t
  | Seq of expr list
  | Let of { bindings : binding list; body : expr }
  | Var of Int.t * Id.t
  | Appl of expr * expr list
  | Get of expr * expr
  | Lambda of { name : Id.t Option.t; params : Pattern.t list; body : expr }
  | Match of expr * (Pattern.t * expr) list

and binding =
  | Bind of Pattern.t * expr
  | Decl of Id.t
  | Resolve of { cell_idx : Int.t; name : Id.t; value_idx : Int.t }

type output = expr

val pass : (Id.t * 'a) List.t -> input -> output
