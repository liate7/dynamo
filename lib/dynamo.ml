open! ContainersLabels
module Value = Eval.Value
module Eval = Eval
module Reader = Reader
module Span = Reader.Span

let span_substring ((startp, endp) : Lexing.position * Lexing.position) str =
  String.sub str ~pos:startp.pos_cnum ~len:(endp.pos_cnum - startp.pos_cnum)

let span_string str span = Span.to_string span ^ ": " ^ span_substring span str

let stack_trace_string stack str =
  List.map stack ~f:(span_string str) |> String.concat ~sep:"\n"

let eval ?name ?continue_from buf =
  (match name with Some name -> Sedlexing.set_filename buf name | None -> ());
  let parse =
    match continue_from with
    | None -> Reader.parse
    | Some checkpoint -> Reader.parse_from_checkpoint checkpoint
  in
  parse buf
  |> Resolver.pass Eval.std_prelude
  |> Eval.eval Eval.(Env.create std_prelude)

let eval_string ?name str = Sedlexing.Utf8.from_string str |> eval ?name

let eval_file ~name flow =
  let str = Eio.Buf_read.(parse_exn take_all) ~max_size:Int.max_int flow in
  match eval_string ~name str with
  | obj -> Ok (Value.to_string obj)
  | (exception Reader.Lexer_error (span, msg))
  | (exception Reader.Parser_error (span, msg)) ->
      Error
        (`Msg
          (String.concat ~sep:""
             [ "Parsing error: "; span_string str span; ": "; msg ]))
  | exception Reader.Needs_input chpt -> Error (`Checkpoint chpt)
  | (exception Errors.Compile_time_fatal (span, msg))
  | (exception Errors.Miscompilation (span, msg)) ->
      Error
        (`Msg
          (String.concat ~sep:""
             [ "Compilation error: "; span_string str span; ": "; msg ]))
  | exception Errors.Runtime_fatal (stack, msg) ->
      Error
        (`Msg
          (String.concat ~sep:""
             [ "Fatal error: "; msg; "\n"; stack_trace_string stack str ]))
  | exception Eval.Runtime_nonfatal { tag; value; stack } ->
      Error
        (`Msg
          (String.concat ~sep:""
             [
               "Uncaught exception: ";
               Value.to_string tag;
               " ! ";
               Value.to_string value;
               "\n";
               stack_trace_string (Option.get_or stack ~default:[]) str;
             ]))

let read_with_prompt input output prompt =
  Eio.Flow.copy_string prompt output;
  Eio.Buf_read.line input

let repl stdin stdout =
  let rec loop str pos checkpoint =
    let prompt = match checkpoint with Some _ -> "… " | None -> "> " in
    match read_with_prompt stdin stdout prompt with
    | exception End_of_file -> ()
    | line -> (
        let str = str ^ line ^ "\n"
        and buf =
          match pos with
          | Some pos ->
              let buf = Sedlexing.Utf8.from_string ("\n" ^ line) in
              Sedlexing.set_position buf pos;
              buf
          | None -> Sedlexing.Utf8.from_string line
        in
        match eval ~name:"<repl>" ?continue_from:checkpoint buf with
        | obj ->
            Eio.Flow.copy_string
              [%string "-- _ = %{Eval.Value.to_string obj}\n"] stdout;
            loop "" None None
        | exception End_of_file -> ()
        | (exception Reader.Lexer_error (span, msg))
        | (exception Reader.Parser_error (span, msg)) ->
            Eio.Flow.copy_string
              (String.concat ~sep:""
                 [ "Parsing error: "; span_string str span; ": "; msg; "\n" ])
              stdout;
            loop "" None None
        | exception Reader.Needs_input checkpoint ->
            loop str
              (Some
                 (let _, pos = Sedlexing.lexing_positions buf in
                  pos))
              (Some checkpoint)
        | (exception Errors.Compile_time_fatal (span, msg))
        | (exception Errors.Miscompilation (span, msg)) ->
            Eio.Flow.copy_string
              (String.concat ~sep:""
                 [
                   "Compilation error: "; span_string str span; ": "; msg; "\n";
                 ])
              stdout;
            loop "" None None
        | exception Errors.Runtime_fatal (stack, msg) ->
            Eio.Flow.copy_string
              (String.concat ~sep:""
                 [
                   "Fatal error: ";
                   msg;
                   "\n";
                   stack_trace_string stack str;
                   "\n\n";
                 ])
              stdout;
            loop "" None None
        | exception Eval.Runtime_nonfatal { tag; value; stack } ->
            Eio.Flow.copy_string
              (String.concat ~sep:""
                 [
                   "Uncaught exception: ";
                   Value.to_string tag;
                   " ! ";
                   Value.to_string value;
                   "\n";
                   stack_trace_string (Option.get_or stack ~default:[]) str;
                   "\n\n";
                 ])
              stdout;
            loop "" None None)
  in
  loop "" None None
