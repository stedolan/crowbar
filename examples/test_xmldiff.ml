open Crowbar

let ident = choose [const "a"; const "b"; const "c"]
let elem_name = map [ident] (fun s -> ("", s))


let attrs =
  choose [
    const Xmldiff.Nmap.empty;
    map [elem_name; ident] Xmldiff.Nmap.singleton
  ]

let rec xml = lazy (
  choose [
    const (`D "a");
    map [ident] (fun s -> `D s);
    map [elem_name; attrs; list (unlazy xml)] (fun s attrs elems ->
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
  ])

let lazy xml = xml

let xml = map [xml] (fun d -> `E (("", "a"), Xmldiff.Nmap.empty, [d]))

let pp_xml ppf xml =
  pp ppf "%s" (Xmldiff.string_of_xml xml)
let xml = with_printer pp_xml xml


let () =
  add_test ~name:"xmldiff" [xml; xml] @@ fun xml1 xml2 ->
    let (patch, xml3) = Xmldiff.diff_with_final_tree xml1 xml2 in
    check_eq ~pp:pp_xml xml2 xml3
