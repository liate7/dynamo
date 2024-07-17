type t = Lexing.position * Lexing.position

let to_string ((start, end_) : t) =
  assert (String.equal start.pos_fname end_.pos_fname);
  let line_col (pos : Lexing.position) =
    [%string
      {|(%{pos.pos_lnum |> Int.to_string},%{pos.pos_cnum - pos.pos_bol |> Int.to_string})|}]
  in
  [%string {|"%{start.pos_fname}": %{line_col start} - %{line_col end_}|}]
