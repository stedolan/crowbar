open Crowbar

let gen1 = choose [const "a"; const "b"]
let gen2 = choose [const "x"; const "y"]

let gens = choose [const gen1; const gen2]

let () =
  ["a"; "b"; "x"; "y"] |>
      List.iter @@ fun s ->
        add_test ~name:("not-"^s) [join gens] @@ fun s' -> check (s <> s')

let () =
  let n = 1000000 in
  add_test ~name:("choose") [range n] @@ fun s -> check (0 <= s && s < n)
