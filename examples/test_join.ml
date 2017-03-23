open Crowbar

let gen1 = Choose [Const "a"; Const "b"]
let gen2 = Choose [Const "x"; Const "y"]

let gens = Choose [Const gen1; Const gen2]

let () =
  ["a"; "b"; "x"; "y"] |>
      List.iter @@ fun s ->
        add_test ~name:("not-"^s) [Join gens] @@ fun s' -> check (s <> s')
