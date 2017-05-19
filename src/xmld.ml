let () =
  let open Xmldiff in
  diff (xml_of_file Sys.argv.(1)) (xml_of_file Sys.argv.(2))
  |> string_of_patch
  |> print_endline
