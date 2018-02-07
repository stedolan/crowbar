
(* module Gen = Gen
 * module Fuzz = Fuzz *)


include Printers
include Gen
include Fuzz
include Std_generators

let () = Printexc.record_backtrace true

let last_generated_name = ref 0
let generate_name () =
  incr last_generated_name;
  "test" ^ string_of_int !last_generated_name

let registered_tests = ref []

let add_test ?name gens f =
  let name = match name with
    | None -> generate_name ()
    | Some name -> name in
  registered_tests := Test (name, gens, f) :: !registered_tests


let () =
  at_exit (fun () ->
      let t = !registered_tests in
      registered_tests := [];
      t |> List.iter (fun (Test (name, _, _) as test) ->
        let ppf = Format.std_formatter in
        let say ?(clear=false) stat =
          pp ppf "%s: %s\n%!" name stat in
        match Fuzz.pqcorpus_fuzz test with
        | TestPass n -> pp ppf "%s: PASS (%d tests)@.%!" name n
        | GenFail (nt, e, bt) ->
           say "FAIL";
           pp ppf "%d The testcase generator threw an exception:@.@[<v 4>@,%a@,@]@.%!"
             nt
             pp_exn_bt (e, bt)
        | TestExn (nt, s, e, bt) ->
           say "FAIL";
           pp ppf "%d When given the input:@.@[<v 4>@,%a@,@]@.the test threw an exception:@.@[<v 4>@,%a@,@]@.%!"
             nt
             pp_sample s
             pp_exn_bt (e, bt)
        | TestFail (nt, s, pv) ->
           say "FAIL";
           pp ppf "%d When given the input:@.@[<v 4>@,%a@,@]@.the test failed:@.@[<v 4>@,%a@,@]@.%!"
             nt
             pp_sample s
             pp_printer pv))
