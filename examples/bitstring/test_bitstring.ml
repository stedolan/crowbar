open Crowbar

(*let () =
  add_test ~name:"bitstring" [list bool] @@ function
  | [true; true; false; true; false; false; false; true; true; false; true;
     false; true; true; true; false; false; true; true; false; true; false] -> fail "found it!"
  | _ -> ()
*)

let nth xs i = if i < List.length xs then List.nth xs i else true

let () =
  add_test ~name:"bitstring" [list bool] @@ fun xs ->
    if nth xs 0  = true
    && nth xs 1  = false
    && nth xs 2  = true
    && nth xs 3  = true
    && nth xs 4  = true
    && nth xs 5  = false
    && nth xs 6  = true
    && nth xs 7  = true
    && nth xs 8  = false
    && nth xs 9  = true
    && nth xs 10 = true
    && nth xs 11 = false
    && nth xs 12 = false
    && nth xs 13 = true
    && nth xs 14 = false
    && nth xs 15 = false
    && nth xs 16 = true
    && nth xs 17 = false
    && nth xs 18 = false
    && nth xs 19 = true
    then 
      fail "found it!"


(*
let () =
  add_test ~name:"bitstring" [list int8] @@ function
  | [42; 42; 17; 42; 17; 17; 17; 42; 42; 17; 42;
     42; 42; 17; 42; 17; 17; 17; 42; 42; 17; 42] -> fail "found it!"
  | _ -> ()
*)
