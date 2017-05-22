open Crowbar
let cmp = OpamVersionCompare.compare
let () =
  add_test ~name:"opam-version-compare" [bytes; bytes; bytes] @@ fun s1 s2 s3 ->
    let c12 = cmp s1 s2 in
    let c21 = cmp s2 s1 in
    let c23 = cmp s2 s3 in
    if c12 = c23 then
      check (c12 = cmp s1 s3)
    else
      check (c12 = -c21)
