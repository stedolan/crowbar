open Crowbar
open Astring
open Fpath
let fpath =
  map [bytes] (fun s ->
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
    check_eq ~eq:equal p (v @@ (fst @@ split_volume p) ^ (String.concat ~sep:dir_sep (segs p)))
