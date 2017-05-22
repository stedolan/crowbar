open Crowbar
open Astring
open Fpath2
let fpath =
  Map ([bytes], fun s ->
    try
      v s
    with
      Invalid_argument _ -> bad_test ())


let () =
  add_test ~name:"segs" [fpath] @@ fun p ->
    let np = normalize p in
    assert (is_dir_path p = is_dir_path np);
    assert (is_file_path p = is_file_path np);
    assert (filename p = filename np);
    let vol, q = split_volume p in
    assert (equal p (v @@ vol ^ (to_string q)));
    let d, b = split_base p in
    check_eq ~pp ~eq:equal np (normalize (d // b));
(*
    check_eq ~eq:equal p (v @@ vol ^ (String.concat ~sep:dir_sep (segs p)))
*)
