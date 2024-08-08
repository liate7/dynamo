open! ContainersLabels
module Span = Reader.Span
module Ast = Reader.Ast
module Id = Ast.Id
module Pattern = Ast.Pattern
module Literal = Ast.Literal

module rec Value_impl : sig
  type t =
    | Unit
    | Number of Q.t
    | Symbol of Id.t
    | Exception of except
    | List of t List.t
    | Dict of t Dict.t
    | Builtin of { arity : int; name : string; actual : env -> t list -> t }
    | Lambda of {
        name : string;
        closure :
          (Id.t * [ `Recurse of t Option.t Ref.t | `Value of t ]) List.t;
        parameters : Pattern.t list;
        body : Resolver.expr;
      }

  and except = { tag : t; value : t; stack : Span.t list option }

  and env = {
    bindings : (Id.t * [ `Recurse of t Option.t Ref.t | `Value of t ]) List.t;
    stack : Span.t list;
  }

  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t =
    | Unit
    | Number of Q.t
    | Symbol of Id.t
    | Exception of except
    | List of t List.t
    | Dict of t Dict.t
    (* TODO: make comparison of functions more sensible by adding an identity field or smthing *)
    | Builtin of {
        (* (* Like: *) identity_tag : Obj.raw_data *)
        arity : Int.t;
        name : String.t;
        actual : env -> t list -> t; [@compare.ignore]
      }
    | Lambda of {
        (* identity_tag : Obj.raw_data *)
        name : String.t;
        closure :
          (Id.t * [ `Recurse of t Option.t Ref.t | `Value of t ]) List.t;
        parameters : Pattern.t List.t;
        body : Resolver.expr; [@compare.ignore]
      }
  [@@deriving compare]

  and except = {
    tag : t;
    value : t;
    stack : Span.t list option; [@compare.ignore]
  }
  [@@deriving compare]

  and env = {
    bindings : (Id.t * [ `Recurse of t Option.t Ref.t | `Value of t ]) List.t;
    stack : Span.t list; [@compare.ignore]
  }

  let rec equal l r =
    match (l, r) with
    | Unit, Unit -> true
    | Number l, Number r -> Q.equal l r
    | Symbol l, Symbol r -> Id.equal l r
    | Exception l, Exception r -> equal l.tag r.tag && equal l.value r.value
    | List l, List r -> List.equal equal l r
    | Dict l, Dict r -> Dict.equal equal l r
    | Builtin l, Builtin r ->
        l.arity = r.arity && String.equal l.name r.name
        && Equal.physical l.actual r.actual
    | Lambda l, Lambda r ->
        String.equal l.name r.name
        && List.equal Ast.Pattern.equal l.parameters r.parameters
        && [%equal:
             (Id.t * [ `Recurse of t Option.t Ref.t | `Value of t ]) List.t]
             l.closure r.closure
        && Equal.physical l.body r.body
    | ( ( Unit | Number _ | Symbol _ | Exception _ | List _ | Dict _ | Builtin _
        | Lambda _ ),
        ( Unit | Number _ | Symbol _ | Exception _ | List _ | Dict _ | Builtin _
        | Lambda _ ) ) ->
        false
end

and Dict : (Map.S with type key = Value_impl.t) = Map.Make (Value_impl)

exception Runtime_nonfatal of Value_impl.except

module Value = struct
  include Value_impl

  let rec to_string t =
    let dict_entry_to_string (k, v) =
      (match k with
      | Symbol s -> Id.to_string s
      | k -> ".[" ^ to_string k ^ "]")
      ^ " = " ^ to_string v
    in

    match t with
    | Unit -> "()"
    | Number n -> Q.to_string n
    | Symbol s -> ":" ^ Id.to_string s
    | Exception { tag; value; stack = (* TODO: stringify this *) _ } ->
        [%string "#<exception (%{to_string tag} ! %{to_string value}) >"]
    | List l -> "[" ^ (List.map ~f:to_string l |> String.concat ~sep:", ") ^ "]"
    | Dict d ->
        "{"
        ^ (Dict.to_seq d
          |> Seq.map dict_entry_to_string
          |> String.concat_seq ~sep:", ")
        ^ "}"
    | Builtin { name; arity; _ } ->
        [%string {|#<builtin "%{name}"/%{Int.to_string arity}>|}]
    | Lambda { name; parameters; _ } ->
        let parameters =
          parameters |> List.map ~f:Pattern.to_string |> String.concat ~sep:" "
        in
        [%string {|#<lambda "%{name}" %{parameters}>|}]

  let symbol_of_string str = Symbol (Id.of_string str)

  let raise_type_error ~stack t type_name =
    raise
      (Runtime_nonfatal
         {
           stack = Some stack;
           tag = symbol_of_string "type";
           value =
             Dict
               (Dict.of_list
                  [
                    (symbol_of_string "type", symbol_of_string type_name);
                    (symbol_of_string "value", t);
                  ]);
         })

  let as_number ~stack t =
    match t with
    | Number n -> n
    | Unit | Symbol _ | Exception _ | List _ | Dict _ | Builtin _ | Lambda _ ->
        raise_type_error ~stack t "number"

  (* let as_bool ~span t = *)
  (*   let _ = span in *)
  (*   match t with *)
  (*   | Unit -> false *)
  (*   | Symbol s when Id.(s = of_string "false") -> false *)
  (*   | Number _ | List _ | Dict _ | Symbol _ | Builtin _ | Lambda _ -> true *)

  let get ~stack t idx =
    let raise_indexing_exn () =
      raise
        (Runtime_nonfatal
           {
             stack = Some stack;
             tag = symbol_of_string "index";
             value =
               Dict
                 (Dict.of_list
                    [
                      (symbol_of_string "value", t);
                      (symbol_of_string "index", idx);
                    ]);
           })
    in
    match t with
    | List l ->
        let idx' = as_number ~stack idx |> Q.to_int in
        List.nth_opt l idx' |> Option.get_lazy raise_indexing_exn
    | Dict d -> Dict.get idx d |> Option.get_lazy raise_indexing_exn
    | Unit | Number _ | Symbol _ | Exception _ | Builtin _ | Lambda _ ->
        raise_type_error ~stack t "indexable"
end
(*
  (* Standard error reporting infrastructure *)
  val raise_type_error : stack:Span.t list -> t -> string -> 'a

  (* Standard coercions.  All raise a non-fatal type error on coercion failure *)
  val as_number : stack:Span.t list -> t -> Q.t
  (** @raise Runtime_nonfatal if the value is not a number *)

  (* val as_bool : span:Span.t -> t -> Bool.t *)

  (* Nontrivial runtime functions *)
  val get : stack:Span.t list -> t -> t -> t
   *)

module Env = struct
  type t = Value.env

  let empty = Value.{ bindings = []; stack = [] }
  let push_call span t = Value.{ t with stack = span :: t.stack }

  exception Match_failure of Pattern.t * Value.t

  let rec add_binding pat value ({ Value.bindings; _ } as t) =
    let handle_dict_row (t, dict) row =
      let do_symbol_binding i pat =
        match Dict.get (Value.Symbol i) dict with
        | Some value ->
            let t = add_binding pat value t in
            (t, Dict.remove (Value.Symbol i) dict)
        | None -> raise (Match_failure (pat, value))
      in
      match row with
      | `Single i -> do_symbol_binding i (Bind i)
      | `Bare (i, pat) -> do_symbol_binding i pat
      | `Computed _ -> assert false
    in
    let bind_literal lit value =
      match (lit, value) with
      | Literal.Unit, Value.Unit -> t
      | Literal.Number l, Value.Number r when Q.(l = r) -> t
      | Literal.Symbol l, Value.Symbol r when Id.(l = r) -> t
      | Literal.List l, Value.List r when List.compare_lengths l r = 0 ->
          List.combine l r
          |> List.fold_left
               ~f:(fun t (pat, value) -> add_binding pat value t)
               ~init:t
      | Literal.Dict lit, Value.Dict d ->
          let t, _ = List.fold_left ~f:handle_dict_row ~init:(t, d) lit in
          t
      | ( ( Literal.Number _ | Literal.Symbol _ | Literal.List _
          | Literal.Dict _ | Literal.Unit ),
          ( Value.Unit | Value.Number _ | Value.Symbol _ | Value.Exception _
          | Value.List _ | Value.Dict _ | Value.Builtin _ | Value.Lambda _ ) )
        ->
          raise (Match_failure (pat, value))
    and bind_exception tag_pat value_pat =
      match value with
      | Value.Exception { tag; value; _ } ->
          add_binding tag_pat tag t |> add_binding value_pat value
      | Value.Unit | Value.Number _ | Value.Symbol _ | Value.List _
      | Value.Dict _ | Value.Builtin _ | Value.Lambda _ ->
          raise (Match_failure (Pattern.Exception (tag_pat, value_pat), value))
    in
    match pat with
    | Pattern.Bind id -> { t with bindings = (id, `Value value) :: bindings }
    | Pattern.Hole _ -> t
    | Pattern.Literal l -> bind_literal l value
    | Pattern.Exception (tag_pat, value_pat) -> bind_exception tag_pat value_pat

  let add_bindings_list kvs t =
    List.fold_left
      ~f:(fun t (pat, value) -> add_binding pat value t)
      ~init:t kvs

  let create kvs =
    List.fold_left
      ~f:(fun t (id, value) -> add_binding (Pattern.Bind id) value t)
      ~init:empty kvs

  let raise_binding_failure ~stack pat value =
    raise
      (Errors.Runtime_fatal
         ( stack,
           [%string
             "Can't match %{Value.to_string value} with %{Pattern.to_string \
              pat}"] ))

  let get ({ bindings; stack } : t) (idx, intended_id) =
    match List.nth_opt bindings idx with
    | None -> None
    | Some (found_id, value) -> (
        assert (Id.(intended_id = found_id));
        match value with
        | `Recurse { contents = None } ->
            raise
              (Errors.Runtime_fatal
                 ( stack,
                   [%string
                     "Tried to access unresolved variable %{Id.to_string \
                      found_id}"] ))
        | `Recurse { contents = Some value } -> Some value
        | `Value value -> Some value)

  let declare t id =
    Value.{ t with bindings = (id, `Recurse (Ref.create None)) :: t.bindings }

  let resolve t (idx, intended_id) value =
    let found_id, to_resolve = List.nth t.Value.bindings idx in
    assert (Id.(intended_id = found_id));
    match to_resolve with
    | `Value _ ->
        raise
          (Errors.Miscompilation
             ( List.hd t.stack,
               "Tried to resolve something other than a recursion cell." ))
    | `Recurse { contents = Some _ } ->
        raise
          (Errors.Miscompilation
             ( List.hd t.stack,
               "Tried to resolve an already-resolved recursion cell." ))
    | `Recurse ({ contents = None } as cell) ->
        cell := get t (value, intended_id)
end

let raise_arity_mismatch ~stack _name arity args =
  raise
    (Errors.Runtime_fatal
       ( stack,
         [%string
           "Arity mismatch: %{List.length args |> Int.to_string} ≠ %{arity |> \
            Int.to_string}."] ))

let rec eval env (ast : Resolver.output) : Value.t =
  match ast with
  | _, Resolver.Literal Literal.Unit -> Unit
  | _, Resolver.Literal (Literal.Number q) -> Number q
  | _, Resolver.Literal (Literal.Symbol s) -> Symbol s
  | _, Resolver.Literal (Literal.List l) -> List (List.map ~f:(eval env) l)
  | _, Resolver.Literal (Literal.Dict d) ->
      Dict
        (List.fold_left ~init:Dict.empty d ~f:(fun d row ->
             let k, v =
               match row with
               | `Bare (s, v) -> (Value.Symbol s, v)
               | `Single _ -> assert false
               | `Computed (ast, v) -> (eval env ast, v)
             in
             Dict.add k (eval env v) d))
  | _, Resolver.Seq asts ->
      List.map ~f:(eval env) asts |> List.last_opt |> Option.get_exn_or ""
  | span, Resolver.Let { bindings = kvs; body } ->
      let env = letrec_binds (Env.push_call span env) kvs in
      eval env body
  | span, Resolver.Var (idx, var_name) -> (
      let env = Env.push_call span env in
      match Env.get env (idx, var_name) with
      | Some v -> v
      | None ->
          raise
            (Errors.Runtime_fatal
               ( env.stack,
                 [%string "Variable %{Id.to_string var_name} is unbound."] )))
  | span, Resolver.Appl (f, args) ->
      let rec loop f args =
        let ret, rest = apply (Env.push_call span env) f args in
        match rest with [] -> ret | args -> loop ret args
      in
      List.map ~f:(eval env) args |> loop (eval env f)
  | _, Resolver.Get (from, idx) ->
      let from = eval env from and idx = eval env idx in
      Value.get ~stack:env.stack from idx
  | span, Resolver.Lambda { name; params; body } ->
      let name = name |> Option.map_or ~default:"%anon%" Id.to_string in
      Lambda
        {
          name = [%string "<%{name}: %{Span.to_string span}>"];
          closure = env.bindings;
          parameters = params;
          body;
        }
  | span, Resolver.Match (value, clauses) -> (
      let env = Env.push_call span env in
      let rec loop value exn = function
        | [] -> raise exn
        | (pat, expr) :: clauses -> (
            match Env.add_binding pat value env with
            | env -> eval env expr
            | exception Env.Match_failure _ -> loop value exn clauses)
      in
      match eval env value with
      | value ->
          clauses
          |> List.filter_map ~f:(function
               | `Value pat, expr -> Some (pat, expr)
               | _ -> None)
          |> loop value
               (Errors.Runtime_fatal
                  ( env.stack,
                    [%string
                      "Value %{Value.to_string value} matched no clauses"] ))
      | exception Runtime_nonfatal value ->
          clauses
          |> List.filter_map ~f:(function
               | `Catch pat, expr -> Some (pat, expr)
               | _ -> None)
          |> loop (Value.Exception value) (Runtime_nonfatal value))

and letrec_binds env kvs =
  let f env = function
    | Resolver.Bind (pat, expr) -> (
        try env |> Env.add_binding pat (eval env expr)
        with Env.Match_failure (pat, value) ->
          Env.raise_binding_failure ~stack:env.stack pat value)
    | Decl id -> Env.declare env id
    | Resolve { cell_idx; name; value_idx } ->
        Env.resolve env (cell_idx, name) value_idx;
        env
  in
  List.fold_left ~f ~init:env kvs

and apply env t args =
  match t with
  | Value.Unit | Number _ | Symbol _ | Exception _ | List _ | Dict _ ->
      Value.raise_type_error ~stack:env.stack t "function"
  | Builtin { arity; name; _ } when List.length args < arity ->
      raise_arity_mismatch ~stack:env.stack name arity args
  | Lambda { name; parameters; _ }
    when List.length args < List.length parameters ->
      raise_arity_mismatch ~stack:env.stack name (List.length parameters) args
  | Builtin { arity; actual; _ } ->
      let args, cont = List.take_drop arity args in
      (* (actual env args, cont) *)
      (actual env args, cont)
  | Lambda { closure; parameters; body; _ } -> (
      let args, cont = List.take_drop (List.length parameters) args in
      match
        { env with bindings = closure }
        |> Env.add_bindings_list (List.combine parameters args)
      with
      | env -> (eval env body, cont)
      | exception Env.Match_failure (pat, value) ->
          Env.raise_binding_failure ~stack:env.stack pat value)

let std_prelude =
  let binding name arity f =
    (Id.of_string name, Value.Builtin { name; arity; actual = f })
  in
  let make_arith_binop name f =
    let arith_binop_wrapper (env : Env.t) = function
      | [ l; r ] ->
          Value.Number
            (f
               (Value.as_number ~stack:env.stack l)
               (Value.as_number ~stack:env.stack r))
      | args -> raise_arity_mismatch ~stack:env.stack name 2 args
    in
    binding name 2 arith_binop_wrapper
  and equals_wrapper (env : Env.t) = function
    | [ l; r ] ->
        if Value.equal l r then Value.Symbol (Id.of_string "true")
        else Value.Symbol (Id.of_string "false")
    | args -> raise_arity_mismatch ~stack:env.stack "=" 2 args
  and make_exn (env : Env.t) = function
    | [ l; r ] -> Value.Exception { tag = l; value = r; stack = None }
    | args -> raise_arity_mismatch ~stack:env.stack "!" 2 args
  and raise_fn (env : Env.t) = function
    | [ Value.Exception ({ stack; _ } as exn) ] ->
        raise
          (Runtime_nonfatal
             { exn with stack = Option.or_ stack ~else_:(Some env.stack) })
    | args -> raise_arity_mismatch ~stack:env.stack "raise" 1 args
  in
  [
    make_arith_binop "+" Q.( + );
    make_arith_binop "-" Q.( - );
    make_arith_binop "*" Q.( * );
    make_arith_binop "/" Q.( / );
    binding "=" 2 equals_wrapper;
    binding "!" 2 make_exn;
    binding "raise" 1 raise_fn;
  ]
