open Crowbar

module C = CalendarLib.Calendar.Precise

let time =
  map [int64] (fun a ->
    try
      C.from_mjd (Int64.to_float a /. 100_000_000_000_000.)
    with
      (* not all int64s represent a valid date; if this is detected properly
         by CalendarLib and signaled with the Out_of_bounds exception,
         don't proceed with a test. It will not be counted as a failure (in
         quickcheck mode) or a crash (in AFL mode). *)
      CalendarLib.Date.Out_of_bounds -> bad_test ())

let pp_time ppf t =
  pp ppf "%04d-%02d-%02d %02d:%02d:%02d" 
     (C.year t)
     (C.month t |> C.Date.int_of_month)
     (C.day_of_month t)
     (C.hour t)
     (C.minute t)
     (C.second t)

(* override the default autoconstructed printer, used for showing results when a
   test caused a failure. *)
let time = with_printer pp_time time

let () =
  add_test ~name:"calendar" [time; time] @@ fun t1 t2 ->
    (* only run the test if time 1 is before time 2 *)
  guard (C.compare t1 t2 < 0);
    (* subtracting a time and then adding it back should yield the original time *)
    check_eq ~pp:pp_time ~eq:C.equal (C.add t1 (C.precise_sub t2 t1)) t2
