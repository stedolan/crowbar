open Crowbar

[%%code
open Crowbar
let rec normalise = function
  | ([] | [_]) as x -> x
  | `E _ as el :: xs ->
     el :: normalise xs
  | `D s :: xs ->
     match normalise xs with
     | `D s' :: xs' ->
        `D (s ^ s') :: xs'
     | xs' -> `D s :: xs'

let pp_xml ppf xml =
  pp ppf "%s" (Xmldiff.string_of_xml xml)
]

let ident = choose [const [%code "a"]; const [%code "b"]; const [%code "c"]]

let elem_name = map [ident] (fun%staged s -> "", s)

let attrs =
  choose [
    const [%code Xmldiff.Nmap.empty];
    map [elem_name; ident] (fun%staged e i -> Xmldiff.Nmap.singleton e i)
  ]

let rec xml = lazy (
  choose [
    const [%code `D "a"];
    map [ident] (fun%staged s -> `D s);
    map [elem_name; attrs; list (unlazy xml)] (fun%staged s attrs elems ->
          `E (s, attrs, normalise elems))
  ])

let lazy xml = xml
let xml = map [xml] (fun%staged d -> `E (("", "a"), Xmldiff.Nmap.empty, [d]))

let () =
  add_test ~name:"xmldiff" [xml; xml] @@ fun%staged xml1 xml2 ->
    let xml2 = xml2 in
    let (patch, xml3) = Xmldiff.diff_with_final_tree xml1 xml2 in
    check_eq ~pp:pp_xml xml2 xml3

