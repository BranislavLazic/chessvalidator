open Printf

type color = Black | White [@@deriving show]

type piece' = Pawn of bool | Rook | Bishop | Knight | Queen | King

and piece = { piece : piece'; color : color; row : int; col : int }
[@@deriving show]

let to_string piece =
  match piece with
  | Pawn _ -> "P"
  | Rook -> "R"
  | Bishop -> "B"
  | Knight -> "N"
  | Queen -> "Q"
  | King -> "K"

type move = { from_col : int; to_col : int; from_row : int; to_row : int }
[@@deriving show]

type chessboard = { pieces : piece option list; as_ascii : string }

let get_or_else alt opt = match opt with Some a -> a | None -> alt
let flatten opt = match opt with Some (Some a) -> Some a | _ -> None
let collect_either eiths = List.fold_left (fun _ e -> e) (Either.Left "") eiths

let find_source_piece chessboard move =
  List.find_opt
    (fun piece ->
      Option.map (fun p -> p.row = move.from_row && p.col = move.from_col) piece
      |> get_or_else false)
    chessboard.pieces
  |> flatten

let find_dest_piece chessboard move =
  List.find_opt
    (fun piece ->
      Option.map (fun p -> p.row = move.to_row && p.col = move.to_col) piece
      |> get_or_else false)
    chessboard.pieces
  |> flatten

let is_move_within_bounds move =
  if
    List.for_all
      (fun m -> m >= 0 && m <= 7)
      [ move.from_col; move.from_row; move.to_col; move.to_row ]
  then Either.Right move
  else Either.Left "The move is not within the chessboard bounds"

let has_moved move =
  if move.from_row != move.to_row || move.from_col != move.to_col then
    Either.Right move
  else Either.Left "Did not move"

let is_opponents_piece source_piece dest_piece move =
  if source_piece.color != dest_piece.color then Either.Right move
  else Either.Left "Not opponents piece"

let horizontal_move_diff move =
  let res = move.to_col - move.from_col in
  if res < 0 then -1 * res else res

let vertical_move_diff move =
  let res = move.to_row - move.from_row in
  if res < 0 then -1 * res else res

let moves_horizontally move max_field_index_to_move =
  move.to_row = move.from_row
  && horizontal_move_diff move <= max_field_index_to_move

let moves_vertically move max_field_index_to_move =
  move.to_col = move.from_col
  && vertical_move_diff move <= max_field_index_to_move

let moves_diagonally move max_field_index_to_move =
  vertical_move_diff move = horizontal_move_diff move
  && vertical_move_diff move <= max_field_index_to_move

let moves_omni_directionally move max_field_index_to_move =
  moves_horizontally move max_field_index_to_move
  || moves_vertically move max_field_index_to_move
  || moves_diagonally move max_field_index_to_move

let moves_forward_vertically move color max_field_index_to_move =
  let row_diff = move.from_row - move.to_row in
  let is_forward = if color = White then row_diff > 0 else row_diff < 0 in
  is_forward && moves_vertically move max_field_index_to_move

let moves_pawn move has_moved source_piece dest_piece =
  moves_forward_vertically move source_piece.color (if has_moved then 1 else 2)
  || (moves_diagonally move 1 && source_piece.color != dest_piece.color)

let is_direction_valid move source_piece dest_piece =
  let is_valid =
    match source_piece.piece with
    | Pawn has_moved -> moves_pawn move has_moved source_piece dest_piece
    | Rook -> moves_horizontally move 7 || moves_vertically move 7
    | Bishop -> moves_diagonally move 7
    | Knight ->
        (vertical_move_diff move = 2 && horizontal_move_diff move = 1)
        || (vertical_move_diff move = 1 && horizontal_move_diff move = 2)
    | Queen -> moves_omni_directionally move 7
    | King -> moves_omni_directionally move 1
  in
  if is_valid then Either.Right move else Either.Left "Invalid direction"

let validate_move chessboard move =
  match
    (find_source_piece chessboard move, find_dest_piece chessboard move)
  with
  | Some source_piece, Some dest_piece ->
      collect_either
        [
          is_move_within_bounds move;
          has_moved move;
          is_direction_valid move source_piece dest_piece;
          is_opponents_piece source_piece dest_piece move;
        ]
  | Some _, None ->
      collect_either [ is_move_within_bounds move; has_moved move ]
  | _ -> Left "Piece not found"

let lowercase_pieces p =
  if p.color = White then String.lowercase_ascii (to_string p.piece)
  else to_string p.piece

let to_ascii pieces =
  List.fold_left
    (fun a p ->
      if (String.length a + 2) mod 9 = 0 && String.length a != 0 then
        sprintf "%s%s\n" a (Option.map lowercase_pieces p |> get_or_else "*")
      else sprintf "%s%s" a (Option.map lowercase_pieces p |> get_or_else "*"))
    String.empty pieces

let init_chessboard =
  let add_pawns row color =
    List.init 8 (fun i -> Some { piece = Pawn false; color; row; col = i })
  in
  let add_empty = List.init 32 (fun _ -> None) in
  let add row color =
    [
      Some { piece = Rook; color; row; col = 0 };
      Some { piece = Knight; color; row; col = 1 };
      Some { piece = Bishop; color; row; col = 2 };
      Some { piece = Queen; color; row; col = 3 };
      Some { piece = King; color; row; col = 4 };
      Some { piece = Bishop; color; row; col = 5 };
      Some { piece = Knight; color; row; col = 6 };
      Some { piece = Rook; color; row; col = 7 };
    ]
  in
  let add_pawns row color =
    match color with
    | Black -> add_pawns row Black
    | White -> add_pawns row White
  in
  let pieces =
    add 0 Black @ add_pawns 1 Black @ add_empty @ add_pawns 6 White
    @ add 7 White
  in
  let as_ascii = to_ascii pieces in
  { pieces; as_ascii }

let update_chessboard chessboard move =
  let updated =
    List.mapi
      (fun idx piece ->
        match piece with
        | Some p ->
            if idx = (move.from_row * 8) + move.from_col then None else Some p
        | None ->
            if idx = (move.to_row * 8) + move.to_col then
              find_source_piece chessboard move
            else None)
      chessboard.pieces
  in
  { pieces = updated; as_ascii = to_ascii updated }

let advance chessboard move =
  Either.map_right
    (fun m -> update_chessboard chessboard m)
    (validate_move chessboard move)
