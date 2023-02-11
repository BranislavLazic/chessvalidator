open Chess

let lines file =
  let contents =
    match file with
    | "-" -> In_channel.input_all In_channel.stdin
    | file -> In_channel.with_open_bin file In_channel.input_all
  in
  String.split_on_char '\n' contents

let read_moves file =
  List.filter (fun l -> String.length l != 0) (lines file)
  |> List.map String.lowercase_ascii
  |> List.map (fun l ->
         let chars = List.init (String.length l) (String.get l) in
         {
           display = l;
           from_col = Char.code (List.nth chars 0) - 97;
           to_col = Char.code (List.nth chars 2) - 97;
           from_row = 56 - Char.code (List.nth chars 1);
           to_row = 56 - Char.code (List.nth chars 3);
         })

let () =
  if Array.length Sys.argv - 1 < 1 then print_endline "Provide a file"
  else
    for i = 1 to Array.length Sys.argv - 1 do
      let file = Sys.argv.(i) in
      if Sys.file_exists file then advance_all init_chessboard (read_moves file)
      else print_endline "The file is not found"
    done
