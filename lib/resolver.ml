open ContainersLabels
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
  | Match of expr * ([ `Value of Pattern.t | `Catch of Pattern.t ] * expr) list

and binding =
  | Bind of Pattern.t * expr
  | Decl of Id.t
  | Resolve of { cell_idx : Int.t; name : Id.t; value_idx : Int.t }

type output = expr

exception Binding_unbound of Span.t * Id.t

module Env = struct
  module Map = Map.Make (Id)

  type t = { depth : Int.t; bindings : int Map.t }

  let empty = { depth = 0; bindings = Map.empty }

  let bind_all t ids =
    ids
    |> Seq.fold_left
         (fun { depth; bindings } id ->
           { depth = depth + 1; bindings = Map.add id depth bindings })
         t

  let of_alist alist =
    alist |> List.to_seq |> Seq.map (fun (name, _) -> name) |> bind_all empty

  let get exn id t =
    match Map.get id t.bindings with
    | Some idx -> t.depth - idx - 1
    | None -> raise exn
end

module Id_set = Set.Make (Id)

let rec pattern_binds : Pattern.t -> Id.t Seq.t = function
  | _, Pattern.Bind id -> Seq.singleton id
  | _, Pattern.Literal lit -> literal_binds lit
  | _, Pattern.Hole _ -> Seq.empty
  | _, Pattern.Exception (tag, value) ->
      Seq.append (pattern_binds tag) (pattern_binds value)

and literal_binds : Pattern.t Literal.t -> Id.t Seq.t = function
  | Literal.List list -> List.to_seq list |> Seq.flat_map pattern_binds
  | Literal.Dict dict_rows ->
      List.to_seq dict_rows
      |> Seq.flat_map (function
           | `Bare (_, pat) -> pattern_binds pat
           | `Single (_, id) -> Seq.singleton id
           | `Computed (k, v) ->
               Seq.of_list [ k; v ] |> Seq.flat_map pattern_binds)
  | Literal.Unit | Literal.Number _ | Literal.Symbol _ -> Seq.empty

let rec map_expression : Env.t -> input -> output =
 fun env ast ->
  let span, ast = ast in
  ( span,
    match ast with
    | Var i -> Var (Env.get (Binding_unbound (span, i)) i env, i)
    | Lambda { name; params; body } ->
        let env =
          List.to_seq params |> Seq.flat_map pattern_binds |> Env.bind_all env
        in
        let body = map_expression env body in
        Lambda { name; params; body }
    | Match (to_match, clauses) ->
        let to_match = map_expression env to_match
        and clauses =
          List.map clauses ~f:(fun (pat, expr) ->
              ( pat,
                let pat = match pat with `Catch p -> p | `Value p -> p in
                map_expression (pattern_binds pat |> Env.bind_all env) expr ))
        in
        Match (to_match, clauses)
    | Let { bindings; body } ->
        let env, bindings = sequence_letrec ~span env bindings in
        Let { bindings; body = map_expression env body }
    (* Just recurse *)
    | Literal lit -> Literal (map_literal env lit)
    | Seq list -> Seq (List.map list ~f:(map_expression env))
    | Appl (f, args) ->
        let f = map_expression env f in
        Appl (f, List.map args ~f:(map_expression env))
    | Get (place, path) ->
        let place = map_expression env place in
        Get (place, map_expression env path) )

and sequence_letrec ~span env bindings =
  let duplicate_binds ids =
    List.fold_left ~init:(Id_set.empty, []) ids ~f:(fun (found, dups) id ->
        if Id_set.mem id found then (found, id :: dups)
        else (Id_set.add id found, dups))
  in
  let bound_ids =
    (* a list of lists of which items were bound by each pattern *)
    List.map bindings ~f:(fun (pat, _) -> pattern_binds pat |> Seq.to_list)
  in
  let bound_in_let, duplicates = List.concat bound_ids |> duplicate_binds in
  if not @@ List.is_empty duplicates then
    let duplicates =
      List.rev_map duplicates ~f:Id.to_string |> String.concat ~sep:", "
    in
    raise
      (Errors.Compile_time_fatal
         ( span,
           [%string
             {|Duplicate bindings for the following names: %{duplicates}|}] ))
  else map_bindings ~span env bindings bound_ids bound_in_let

and map_bindings ~span env bindings bound_ids bound_in_let =
  (* Both transform the binding values and remove all the recursive bindings *)
  let get_or_throw id env =
    let exn =
      Errors.Miscompilation
        ( span,
          [%string "Can't find %{Id.to_string id} to compile resolving it."] )
    in
    Env.get exn id env
  in
  let rec loop (env, declared, bindings) (pat, expr) bound_here =
    match map_expression env expr with
    | exception Binding_unbound (_, id) when Id_set.mem id bound_in_let ->
        let ret =
          ( Env.bind_all env (Seq.singleton id),
            Id_set.add id declared,
            Decl id :: bindings )
        in
        loop ret (pat, expr) bound_here
    | expr ->
        let to_resolve = Id_set.inter declared (Id_set.of_list bound_here) in
        let new_env = Env.bind_all env (Seq.of_list bound_here) in
        let unshadowed = { env with depth = new_env.depth } in
        let bindings =
          Id_set.fold
            (fun id bindings ->
              let cell_idx = get_or_throw id unshadowed
              and value_idx = get_or_throw id new_env in
              Resolve { cell_idx; name = id; value_idx } :: bindings)
            to_resolve
            (Bind (pat, expr) :: bindings)
        in
        (new_env, declared, bindings)
  in
  let env, _, bindings =
    List.fold_left2 ~init:(env, Id_set.empty, []) bindings bound_ids ~f:loop
  in
  (env, List.rev bindings)

and map_literal : Env.t -> input Literal.t -> output Literal.t =
 fun env -> function
  | Literal.List list -> Literal.List (List.map list ~f:(map_expression env))
  | Literal.Dict dict_rows ->
      Literal.Dict
        (List.map dict_rows ~f:(function
          | `Bare (id, value) -> `Bare (id, map_expression env value)
          | `Single (span, id) -> `Bare (id, map_expression env (span, Var id))
          | `Computed (key, value) ->
              `Computed (map_expression env key, map_expression env value)))
  (* Just remake *)
  | Literal.Number n -> Literal.Number n
  | Literal.Symbol s -> Literal.Symbol s
  | Literal.Unit -> Literal.Unit

let pass env ast =
  let env = Env.of_alist env in
  try map_expression env ast
  with Binding_unbound (span, id) ->
    raise
      (Errors.Compile_time_fatal
         (span, [%string "Variable %{Id.to_string id} is unbound."]))
