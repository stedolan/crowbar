open Crowbar

module C = CalendarLib.Calendar.Precise

let time =
  map [int64] (fun a ->
    try
      C.from_mjd (Int64.to_float a /. 100_000_000_000_000.)
    with
      CalendarLib.Date.Out_of_bounds -> bad_test ())

let pp_time ppf t =
  pp ppf "%04d-%02d-%02d %02d:%02d:%02d"
     (C.year t)
     (C.month t |> C.Date.int_of_month)
     (C.day_of_month t)
     (C.hour t)
     (C.minute t)
     (C.second t)
let time = with_printer pp_time time

let period =
  map [const 0;const 0;int8;int8;int8;int8] C.Period.make


let () =
  add_test ~name:"calendar" [time; time] @@ fun t1 t2 ->
    guard (C.compare t1 t2 < 0);
    check_eq ~pp:pp_time ~eq:C.equal (C.add t1 (C.precise_sub t2 t1)) t2
