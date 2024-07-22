open ContainersLabels
open Reader

(* open Ast *)
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

  let get id { bindings; _ } = Map.get id bindings
  let depth { depth; _ } = depth
end

module Id_set = Set.Make (Id)

let rec pattern_binds : Pattern.t -> Id.t Seq.t = function
  | Pattern.Bind id -> Seq.singleton id
  | Pattern.Literal lit -> literal_binds lit
  | Pattern.Hole _ -> Seq.empty

and literal_binds : Pattern.t Literal.t -> Id.t Seq.t = function
  | Literal.List list -> List.to_seq list |> Seq.flat_map pattern_binds
  | Literal.Dict dict_rows ->
      List.to_seq dict_rows
      |> Seq.flat_map (function
           | `Bare (_, pat) -> pattern_binds pat
           | `Single id -> pattern_binds (Bind id)
           | `Computed (k, v) ->
               Seq.of_list [ k; v ] |> Seq.flat_map pattern_binds)
  | Literal.Unit | Literal.Number _ | Literal.Symbol _ -> Seq.empty

let rec remove_from_pattern : Id_set.t -> Pattern.t -> Pattern.t =
 fun to_remove pat ->
  match pat with
  | Pattern.Bind id when Id_set.mem id to_remove ->
      Pattern.Hole (Id.to_string id)
  | Pattern.Bind _ | Pattern.Hole _ -> pat
  | Pattern.Literal lit -> Pattern.Literal (remove_from_literal to_remove lit)

and remove_from_literal : Id_set.t -> Pattern.t Literal.t -> Pattern.t Literal.t
    =
 fun to_remove literal ->
  match literal with
  | Literal.Number _ | Literal.Symbol _ | Literal.Unit -> literal
  | Literal.List l ->
      Literal.List (List.map l ~f:(remove_from_pattern to_remove))
  | Literal.Dict dict_rows ->
      Literal.Dict
        (List.map dict_rows ~f:(function
          | `Bare (id, pat) -> `Bare (id, remove_from_pattern to_remove pat)
          | `Single id when Id_set.mem id to_remove ->
              `Bare (id, Pattern.Hole (Id.to_string id))
          | `Single id -> `Single id
          | `Computed (k, v) ->
              `Computed
                ( remove_from_pattern to_remove k,
                  remove_from_pattern to_remove v )))

let rec map_expression : Env.t -> input -> output =
 fun env ast ->
  let span, ast = ast in
  ( span,
    match ast with
    | Var i -> (
        match Env.get i env with
        | None -> raise (Binding_unbound (span, i))
        | Some idx -> Var (Env.depth env - idx - 1, i))
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
              (pat, map_expression (pattern_binds pat |> Env.bind_all env) expr))
        in
        Match (to_match, clauses)
    | Let { bindings; body } ->
        let env, bindings = sequence_letrec ~span env bindings in
        Let { bindings; body = map_expression env body }
    (* Just recurse *)
    | Literal lit -> Literal (map_literal ~span env lit)
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
      (Errors.Runtime_fatal
         ( span,
           [%string
             {|Duplicate bindings for the following names: %{duplicates}|}] ))
  else
    (* Both transform the binding values and remove all the recursive bindings *)
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
          (* let unshadowed = Env.t *)
          ( new_env,
            declared,
            Id_set.fold
              (fun id bindings ->
                Resolve
                  {
                    cell_idx =
                      (match Env.get id unshadowed with
                      | Some idx -> unshadowed.depth - idx - 1
                      | None ->
                          raise
                            (Errors.Miscompilation
                               ( span,
                                 [%string
                                   "Can't find %{Id.to_string id} to compile \
                                    resolving it."] )));
                    name = id;
                    value_idx =
                      (match Env.get id new_env with
                      | Some idx -> new_env.depth - idx - 1
                      | None ->
                          raise
                            (Errors.Miscompilation
                               ( span,
                                 [%string
                                   "Can't find %{Id.to_string id} to compile \
                                    resolving it."] )));
                  }
                :: bindings)
              to_resolve
              (Bind (pat, expr) :: bindings) )
    in
    let env, _, bindings =
      List.fold_left2 ~init:(env, Id_set.empty, []) bindings bound_ids ~f:loop
    in
    (env, List.rev bindings)

and map_literal : span:Span.t -> Env.t -> input Literal.t -> output Literal.t =
 fun ~span env -> function
  | Literal.List list -> Literal.List (List.map list ~f:(map_expression env))
  | Literal.Dict dict_rows ->
      Literal.Dict
        (List.map dict_rows ~f:(function
          | `Bare (id, value) -> `Bare (id, map_expression env value)
          | `Single id ->
              (* TODO: get a more precise span here *)
              `Bare (id, map_expression env (span, Var id))
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
      (Errors.Runtime_fatal
         (span, [%string "Variable %{Id.to_string id} is unbound."]))
