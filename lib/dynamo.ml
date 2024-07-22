open ContainersLabels
open Fun.Infix
module Value = Eval.Value
module Eval = Eval
module Reader = Reader

let eval_string =
  Sedlexing.Utf8.from_string %> Reader.parse
  %> Resolver.pass Eval.std_prelude
  %> Eval.eval Eval.(Env.create std_prelude)
