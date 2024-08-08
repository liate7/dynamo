type t = Lexing.position * Lexing.position

let to_string ((start, end_) : t) =
  assert (String.equal start.pos_fname end_.pos_fname);
  assert (start.pos_cnum <= end_.pos_cnum);
  let pos_part =
    if start.pos_lnum = end_.pos_lnum then
      let line = Int.to_string start.pos_lnum
      and start_col = start.pos_cnum - start.pos_bol |> Int.to_string
      and end_col = end_.pos_cnum - end_.pos_bol |> Int.to_string in
      [%string "line %{line}, columns %{start_col} - %{end_col}"]
    else
      let do_position (pos : Lexing.position) =
        let line = Int.to_string pos.pos_lnum
        and col = pos.pos_cnum - pos.pos_bol |> Int.to_string in
        [%string "line %{line}, col %{col}"]
      in
      [%string "(%{do_position start}) - (%{do_position end_})"]
  in
  [%string {|"%{start.pos_fname}": %{pos_part}|}]
