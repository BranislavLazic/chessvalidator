open OUnit2
open Chess

let test_init_chessboard_count_pieces _ =
  assert_equal (List.length init_chessboard.pieces) 64

let test_init_chessboard_as_ascii _ =
  assert_equal init_chessboard.as_ascii
    "RNBQKBNR\n\
     PPPPPPPP\n\
     ********\n\
     ********\n\
     ********\n\
     ********\n\
     pppppppp\n\
     rnbqkbnr\n"

let test_find_source_knight _ =
  assert_equal
    (find_source_piece
       { display = "B1F2"; from_col = 1; to_col = 2; from_row = 0; to_row = 1 }
       init_chessboard)
    (Some { piece = Knight; color = Black; row = 0; col = 1 })

let test_find_source_pawn _ =
  assert_equal
    (find_source_piece
       { display = ""; from_col = 1; to_col = 1; from_row = 1; to_row = 2 }
       init_chessboard)
    (Some { piece = Pawn false; color = Black; row = 1; col = 1 })

let test_validate_move _ =
  assert_equal
    (validate_move init_chessboard
       { display = ""; from_col = 1; to_col = 1; from_row = 1; to_row = 3 })
    (Ok { display = ""; from_col = 1; to_col = 1; from_row = 1; to_row = 3 })

let test_validate_move_invalid_pawn_move _ =
  assert_equal
    (validate_move init_chessboard
       { display = "B1B2"; from_col = 1; to_col = 1; from_row = 1; to_row = 6 })
    (Error "Invalid move -> B1B2")

let test_validate_move_invalid_rook_move _ =
  assert_equal
    (validate_move init_chessboard
       { display = "A8C6"; from_col = 0; to_col = 2; from_row = 0; to_row = 2 })
    (Error "Invalid move -> A8C6")

let test_validate_move_dest_piece_is_friendly _ =
  assert_equal
    (validate_move init_chessboard
       { display = "A8B8"; from_col = 0; to_col = 0; from_row = 0; to_row = 1 })
    (Error "Invalid move -> A8B8")

let test_find_dest_piece _ =
  assert_equal
    (find_dest_piece
       { display = ""; from_col = 1; to_col = 10; from_row = 0; to_row = 1 }
       init_chessboard)
    None

let test_is_move_within_bounds _ =
  assert_equal
    (is_move_within_bounds
       { display = ""; from_col = 1; to_col = 10; from_row = 0; to_row = 1 })
    false

let test_horizontal_move_diff _ =
  assert_equal
    (horizontal_move_diff
       { display = ""; from_col = 0; to_col = 2; from_row = 0; to_row = 1 })
    2

let test_horizontal_move_diff_back _ =
  assert_equal
    (horizontal_move_diff
       { display = ""; from_col = 2; to_col = 0; from_row = 0; to_row = 1 })
    2

let test_vertical_move_diff _ =
  assert_equal
    (vertical_move_diff
       { display = ""; from_col = 0; to_col = 2; from_row = 0; to_row = 3 })
    3

let test_vertical_move_diff_back _ =
  assert_equal
    (vertical_move_diff
       { display = ""; from_col = 2; to_col = 0; from_row = 0; to_row = 3 })
    3

let test_update_chessboard _ =
  assert_equal
    (update_chessboard init_chessboard
       { display = ""; from_col = 0; to_col = 0; from_row = 1; to_row = 3 })
      .as_ascii
    "RNBQKBNR\n\
     *PPPPPPP\n\
     ********\n\
     P*******\n\
     ********\n\
     ********\n\
     pppppppp\n\
     rnbqkbnr\n"

let test_update_chessboard_replace_piece _ =
  assert_equal
    (update_chessboard init_chessboard
       { display = ""; from_col = 0; to_col = 2; from_row = 1; to_row = 7 })
      .as_ascii
    "RNBQKBNR\n\
     *PPPPPPP\n\
     ********\n\
     ********\n\
     ********\n\
     ********\n\
     pppppppp\n\
     rnPqkbnr\n"

let test_update_chessboard_and_find _ =
  assert_equal
    (Option.get
       (find_source_piece
          { display = ""; from_col = 0; to_col = 0; from_row = 1; to_row = 3 }
          init_chessboard))
    { piece = Pawn false; color = Black; row = 1; col = 0 };
  let new_chessboard =
    update_chessboard init_chessboard
      { display = ""; from_col = 0; to_col = 0; from_row = 1; to_row = 3 }
  in
  assert_equal
    (Option.get
       (find_source_piece
          { display = ""; from_col = 0; to_col = 0; from_row = 3; to_row = 4 }
          new_chessboard))
    { piece = Pawn true; color = Black; row = 3; col = 0 }

let suite =
  "Chess"
  >::: [
         "test_init_chessboard_count_pieces"
         >:: test_init_chessboard_count_pieces;
         "test_init_chessboard_as_ascii" >:: test_init_chessboard_as_ascii;
         "test_find_source_knight" >:: test_find_source_knight;
         "test_find_source_pawn" >:: test_find_source_pawn;
         "test_validate_move" >:: test_validate_move;
         "test_validate_move_invalid_pawn_move"
         >:: test_validate_move_invalid_pawn_move;
         "test_validate_move_dest_piece_is_friendly"
         >:: test_validate_move_dest_piece_is_friendly;
         "test_find_dest_piece" >:: test_find_dest_piece;
         "test_is_move_within_bounds" >:: test_is_move_within_bounds;
         "test_horizontal_move_diff" >:: test_horizontal_move_diff;
         "test_horizontal_move_diff_back" >:: test_horizontal_move_diff_back;
         "test_vertical_move_diff" >:: test_vertical_move_diff;
         "test_vertical_move_diff_back" >:: test_vertical_move_diff_back;
         "test_update_chessboard" >:: test_update_chessboard;
         "test_update_chessboard_and_find" >:: test_update_chessboard_and_find;
         "test_update_chessboard_replace_piece"
         >:: test_update_chessboard_replace_piece;
         "test_validate_move_invalid_rook_move"
         >:: test_validate_move_invalid_rook_move;
       ]

let () = run_test_tt_main suite
