open Crowbar
open Astring
open Fpath
let fpath =
  Map ([bytes], fun s ->
    try
      v s
    with
      Invalid_argument _ -> bad_test ())


let () =
  add_test ~name:"segs" [fpath] @@ fun p ->
    check_eq ~eq:equal p (v @@ (fst @@ split_volume p) ^ (String.concat ~sep:dir_sep (segs p)))
