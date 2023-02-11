open Chess

let () =
  advance_all init_chessboard
    [
      { display = "C2C4"; from_col = 2; to_col = 2; from_row = 1; to_row = 3 };
      { display = "B8C6"; from_col = 1; to_col = 2; from_row = 0; to_row = 2 };
      { display = "B2B4"; from_col = 1; to_col = 1; from_row = 6; to_row = 4 };
      { display = "C2C3"; from_col = 2; to_col = 2; from_row = 6; to_row = 5 };
    ]
