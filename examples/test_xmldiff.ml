open Crowbar

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
