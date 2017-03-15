open Crowbar

module C = CalendarLib.Calendar.Precise

let time =
  Map ([int64], fun a ->
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
let time = Print (pp_time, time)

let period =
  Map ([Const 0;Const 0;int8;int8;int8;int8], C.Period.make)

let () =
  add_test ~name:"calendar" [time; time] @@ fun t1 t2 ->
    guard (C.compare t1 t2 < 0);
    check_eq ~pp:pp_time ~eq:C.equal (C.add t1 (C.precise_sub t2 t1)) t2




let ident = Choose [Const "a"; Const "b"; Const "c"]
let elem_name = Map ([ident], fun s -> ("", s))
let attrs =
  Choose [
    Const Xmldiff.Nmap.empty;
    Map ([elem_name; ident], Xmldiff.Nmap.singleton)
  ]

let rec xml =
  Choose [
    Const (`D "a");
    Map ([ident], fun s -> `D s);
    Map ([elem_name; attrs; List xml], fun s attrs elems ->
      let rec normalise = function
        | ([] | [_]) as x -> x
        | `E _ as el :: xs ->
           el :: normalise xs
        | `D s :: xs ->
           match normalise xs with
           | `D s' :: xs' ->
              `D (s ^ s') :: xs'
           | xs' -> `D s :: xs' in
      `E (s, attrs, normalise elems))
  ]

let xml = Map ([xml], fun d -> `E (("", "a"), Xmldiff.Nmap.empty, [d]))

let pp_xml ppf xml =
  pp ppf "%s" (Xmldiff.string_of_xml xml)
let xml = Print (pp_xml, xml)

let () =
  add_test ~name:"xmldiff" [xml; xml] @@ fun xml1 xml2 ->
    let (patch, xml3) = Xmldiff.diff_with_final_tree xml1 xml2 in
    check_eq ~pp:pp_xml xml2 xml3
