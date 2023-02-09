open Chess

let () =
  match
    find_source_piece init_chessboard
      { from_col = 1; to_col = 1; from_row = 0; to_row = 1 }
  with
  | Some piece -> print_endline (show_piece piece)
  | None -> print_endline "Not found"

let () =
  match
    is_move_within_bounds
      { from_col = 1; to_col = 10; from_row = 0; to_row = 1 }
  with
  | Right move -> print_endline (show_move move)
  | Left a -> print_endline a

let () =
  match
    advance init_chessboard
      { from_col = 0; to_col = 0; from_row = 1; to_row = 3 }
  with
  | Right chessboard -> print_endline chessboard.as_ascii
  | Left a -> print_endline a

let () =
  match
    advance init_chessboard
      { from_col = 0; to_col = 0; from_row = 1; to_row = 3 }
  with
  | Right chessboard ->
      print_endline
        (show_piece
           (Option.get
              (find_source_piece chessboard
                 { from_col = 0; to_col = 0; from_row = 3; to_row = 4 })))
  | Left a -> print_endline a
