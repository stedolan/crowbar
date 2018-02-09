open Crowbar
let () =
  add_test ~name:"bitstring" [list bool] @@ function
  | [true; true; false; true; false; false; false; true; true; false; true;
     false; true; true; true; false; false; true; true; false; true; false] -> fail "found it!"
  | _ -> ()

(*
let () =
  add_test ~name:"bitstring" [list int8] @@ function
  | [42; 42; 17; 42; 17; 17; 17; 42; 42; 17; 42;
     42; 42; 17; 42; 17; 17; 17; 42; 42; 17; 42] -> fail "found it!"
  | _ -> ()
*)
