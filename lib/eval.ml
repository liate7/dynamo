open! ContainersLabels
module Span = Reader.Span
module Ast = Reader.Ast
module Id = Ast.Id
module Pattern = Ast.Pattern
module Literal = Ast.Literal

exception Runtime_fatal of Span.t * String.t
exception Runtime_nonfatal of Span.t * String.t

module Bindings = Map.Make (Ast.Id)

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

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> String.t

  (* Standard error reporting infrastructure *)
  val raise_type_error : span:Span.t -> t -> string -> 'a

  (* Standard coercions.  All raise a non-fatal type error on coercion failure *)
  val as_number : span:Span.t -> t -> Q.t
  (** @raise Runtime_nonfatal if the value is not a number *)

  (* val as_bool : span:Span.t -> t -> Bool.t *)

  (* Nontrivial runtime functions *)
  val get : span:Span.t -> t -> t -> t
end = struct
  type t =
    | Unit
    | Number of Q.t
    | Symbol of Id.t
    | List of t List.t
    | Dict of t Dict.t
    (* TODO: make comparison of functions more sensible by adding an identity field or smthing *)
    | Builtin of {
        (* (* Like: *) identity_tag : Obj.raw_data *)
        arity : Int.t;
        name : String.t;
        actual : Span.t -> t list -> t; [@compare.ignore]
      }
    | Lambda of {
        (* identity_tag : Obj.raw_data *)
        name : String.t;
        closure : t Bindings.t;
        parameters : Pattern.t List.t;
        body : Ast.expr; [@compare.ignore]
      }
  [@@deriving compare]

  let rec equal l r =
    match (l, r) with
    | Unit, Unit -> true
    | Number l, Number r -> Q.equal l r
    | Symbol l, Symbol r -> Id.equal l r
    | List l, List r -> List.equal equal l r
    | Dict l, Dict r -> Dict.equal equal l r
    | Builtin l, Builtin r ->
        l.arity = r.arity && String.equal l.name r.name
        && Equal.physical l.actual r.actual
    | Lambda l, Lambda r ->
        String.equal l.name r.name
        && List.equal Ast.Pattern.equal l.parameters r.parameters
        && Bindings.equal equal l.closure r.closure
        && Equal.physical l.body r.body
    | ( (Unit | Number _ | Symbol _ | List _ | Dict _ | Builtin _ | Lambda _),
        (Unit | Number _ | Symbol _ | List _ | Dict _ | Builtin _ | Lambda _) )
      ->
        false

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

  let raise_type_error ~span t type_name =
    raise
      (Runtime_nonfatal (span, [%string "%{to_string t} is not a %{type_name}."]))

  let as_number ~span t =
    match t with
    | Number n -> n
    | Unit | Symbol _ | List _ | Dict _ | Builtin _ | Lambda _ ->
        raise_type_error ~span t "number"

  (* let as_bool ~span t = *)
  (*   let _ = span in *)
  (*   match t with *)
  (*   | Unit -> false *)
  (*   | Symbol s when Id.(s = of_string "false") -> false *)
  (*   | Number _ | List _ | Dict _ | Symbol _ | Builtin _ | Lambda _ -> true *)

  let get ~span t idx =
    match t with
    | List l ->
        let idx = as_number ~span idx |> Q.to_int in
        List.nth_opt l idx
        |> Option.get_lazy (fun () ->
               raise
                 (Runtime_nonfatal
                    ( span,
                      [%string
                        "%{to_string t} has no index %{Int.to_string idx}"] )))
    | Dict d ->
        Dict.get idx d
        |> Option.get_lazy (fun () ->
               raise
                 (Runtime_nonfatal
                    ( span,
                      [%string "%{to_string t} has no index %{to_string idx}"]
                    )))
    | Unit | Number _ | Symbol _ | Builtin _ | Lambda _ ->
        raise_type_error ~span t "indexable value"
end

and Dict : (Map.S with type key = Value.t) = Map.Make (Value)

module Env = struct
  type bindings = Value.t Bindings.t
  type t = { bindings : Value.t Bindings.t }

  let empty = { bindings = Bindings.empty }

  let rec add_binding pat value ({ bindings } as t) =
    let bind_literal lit value =
      let failure =
        Result.fail [%string "%{Value.to_string value} can't be bound here."]
      in
      match (lit, value) with
      | Literal.Unit, Value.Unit -> Ok t
      | Literal.Number l, Value.Number r when Q.(l = r) -> Ok t
      | Literal.Symbol l, Value.Symbol r when Id.(l = r) -> Ok t
      | Literal.List l, Value.List r when List.compare_lengths l r = 0 ->
          List.combine l r
          |> Result.fold_l (fun t (pat, value) -> add_binding pat value t) t
      | Literal.Dict lit, Value.Dict d ->
          lit
          |> Result.fold_l
               (fun t (key, pat) ->
                 match key with
                 | `Bare i -> (
                     match Dict.get (Value.Symbol i) d with
                     | Some value -> add_binding pat value t
                     | None -> failure)
                 | `Computed _p -> Result.fail "not implemented yet")
               t
      | ( ( Literal.Number _ | Literal.Symbol _ | Literal.List _
          | Literal.Dict _ | Literal.Unit ),
          ( Value.Unit | Value.Number _ | Value.Symbol _ | Value.List _
          | Value.Dict _ | Value.Builtin _ | Value.Lambda _ ) ) ->
          failure
    in
    match pat with
    | Pattern.Bind id -> Ok { bindings = Bindings.add id value bindings }
    | Pattern.Hole _ -> Ok t
    | Pattern.Literal l -> bind_literal l value

  let add_bindings_list kvs t =
    Result.fold_l (fun t (pat, value) -> add_binding pat value t) t kvs
end

let raise_arity_mismatch ~span _name arity args =
  raise
    (Runtime_fatal
       ( span,
         [%string
           "Arity mismatch: %{List.length args |> Int.to_string} ≠ %{arity |> \
            Int.to_string}."] ))

let rec eval ({ Env.bindings } as env) ast : Value.t =
  match ast with
  | _, Ast.Literal Literal.Unit -> Unit
  | _, Ast.Literal (Literal.Number q) -> Number q
  | _, Ast.Literal (Literal.Symbol s) -> Symbol s
  | _, Ast.Literal (Literal.List l) -> List (List.map ~f:(eval env) l)
  | _, Ast.Literal (Literal.Dict d) ->
      Dict
        (List.fold_left ~init:Dict.empty d ~f:(fun d (k, v) ->
             let k =
               match k with
               | `Bare s -> Value.Symbol s
               | `Computed ast -> eval env ast
             in
             Dict.add k (eval env v) d))
  | _, Ast.Seq asts ->
      List.map ~f:(eval env) asts |> List.last_opt |> Option.get_exn_or ""
  | span, Ast.Let { bindings = kvs; body } ->
      let env = env_with_bindings ~span env kvs in
      eval env body
  | span, Ast.Var v -> (
      match Bindings.get v bindings with
      | Some v -> v
      | None ->
          raise
            (Runtime_fatal
               (span, [%string "Variable %{Id.to_string v} is unbound."])))
  | span, Ast.Appl (f, args) ->
      let rec loop f args =
        let ret, rest = apply env f args ~span in
        match rest with [] -> ret | args -> loop ret args
      in
      List.map ~f:(eval env) args |> loop (eval env f)
  | span, Ast.Get (from, idx) ->
      let from = eval env from and idx = eval env idx in
      Value.get ~span from idx
  | span, Ast.Lambda { name; params; body } ->
      let name = name |> Option.map_or ~default:"%anon%" Id.to_string in
      Lambda
        {
          name = [%string "<%{name}: %{Span.to_string span}>"];
          closure = env.bindings;
          parameters = params;
          body;
        }
  | span, Ast.Match (value, clauses) ->
      let value = eval env value in
      let rec loop = function
        | [] ->
            raise
              (Runtime_fatal
                 ( span,
                   [%string "Value %{Value.to_string value} matched no clauses"]
                 ))
        | (pat, expr) :: clauses -> (
            match Env.add_binding pat value env with
            | Ok env -> eval env expr
            | Error _ -> loop clauses)
      in
      loop clauses

and env_with_bindings ~span env kvs =
  let f env (pat, expr) =
    env
    |> Env.add_binding pat (eval env expr)
    |> Result.get_lazy (fun str -> raise (Runtime_fatal (span, str)))
  in
  List.fold_left ~f ~init:env kvs

and apply ~span _env t args =
  match t with
  | Value.Unit | Number _ | Symbol _ | List _ | Dict _ ->
      Value.raise_type_error ~span t "function"
  | Builtin { arity; name; _ } when List.length args < arity ->
      raise_arity_mismatch ~span name arity args
  | Lambda { name; parameters; _ }
    when List.length args < List.length parameters ->
      raise_arity_mismatch ~span name (List.length parameters) args
  | Builtin { arity; actual; _ } ->
      let args, cont = List.take_drop arity args in
      (actual span args, cont)
  | Lambda { closure; parameters; body; _ } ->
      let args, cont = List.take_drop (List.length parameters) args in
      let env =
        { (* env with *) bindings = closure }
        |> Env.add_bindings_list (List.combine parameters args)
        |> Result.get_lazy (fun str -> raise (Runtime_fatal (span, str)))
      in
      (eval env body, cont)

let std_prelude =
  let binding name arity f =
    (Id.of_string name, Value.Builtin { name; arity; actual = f })
  in
  let make_arith_binop name f =
    let arith_binop_wrapper span = function
      | [ l; r ] ->
          Value.Number (f (Value.as_number ~span l) (Value.as_number ~span r))
      | args -> raise_arity_mismatch ~span name 2 args
    in
    binding name 2 arith_binop_wrapper
  and equals_wrapper span = function
    | [ l; r ] ->
        if Value.equal l r then Value.Symbol (Id.of_string "true")
        else Value.Symbol (Id.of_string "false")
    | args -> raise_arity_mismatch ~span "=" 2 args
  in
  {
    Env.bindings =
      Bindings.of_list
        [
          make_arith_binop "+" Q.( + );
          make_arith_binop "-" Q.( - );
          make_arith_binop "*" Q.( * );
          make_arith_binop "/" Q.( / );
          binding "=" 2 equals_wrapper;
        ];
  }
