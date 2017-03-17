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



module PP = struct
  open Crowbar
  open PPrint
  type t = (string * PPrint.document)
  let rec doc = Choose [
    Const ("", empty);
    Const ("a", char 'a');
    Const ("123", string "123");
    Const ("Hello", string "Hello");
    Const ("awordwhichisalittlebittoolong", 
           string "awordwhichisalittlebittoolong");
    Const ("", hardline);
    Map ([range 10], fun n -> ("", break n));
    Map ([range 10], fun n -> ("", break n));
    Map ([doc; doc], fun (sa,da) (sb,db) -> (sa ^ sb, da ^^ db));
    Map ([range 10; doc], fun n (s,d) -> (s, nest n d));
    Map ([doc], fun (s, d) -> (s, group d));
    Map ([doc], fun (s, d) -> (s, align d))
  ]

  let check (s, d) =
    let b = Buffer.create 100 in
    let w = 40 in
    ToBuffer.pretty 1.0 w b d;
    let text = Bytes.to_string (Buffer.to_bytes b) in
    let ws = Str.regexp "[ \t\n\r]*" in
    (* Printf.printf "doc2{\n%s\n}%!" text; *)
    let del_ws = Str.global_replace ws "" in
(*    Printf.printf "[%s] = [%s]\n%!" (del_ws s) (del_ws text);*)
    Str.split (Str.regexp "\n") text |> List.iter (fun s ->
      let mspace = Str.regexp "[^ ] " in
      if String.length s > w then
        match Str.search_forward mspace s w with
        | _ -> assert false
        | exception Not_found -> ());
    del_ws s = del_ws text
end

let () =
  add_test ~name:"pprint" [PP.doc] @@ fun doc ->
    check (PP.check doc)

(*
                                               try
    let js = Crowbar.run PP.doc stdin in
    (if Array.length Sys.argv > 1 then begin
       Printf.printf "[%s]\ndoc{%!" (fst js);
       PPrint.ToChannel.pretty 1.0 40 stdout (snd js);
       Printf.printf "\n}\n%!";
    end);
    if not (PP.check js) then begin
        failwith "boom"
      end
  with
    Crowbar.BadTest s -> ()


*)






module StdMap = struct
  module Map = Map.Make (struct
    type t = int
    let compare (i : int) (j : int) = compare i j
  end)

  type t = ((int * int) list * int Map.t)

  let check ((list, map) : t) =
    let rec dedup k = function
      | [] -> []
      | (k', v') :: rest when k = k' -> dedup k rest
      | (k', v') :: rest ->
         (k', v') :: dedup k' rest in
    let list = match List.stable_sort (fun a b -> compare (fst a) (fst b)) list with
      | [] -> []
      | (k, v) :: rest -> (k, v) :: dedup k rest in
    List.for_all (fun (k, v) -> Map.find k map = v) list && 
      list = Map.bindings map

  let rec map : t gen = Choose [
    Const ([], Map.empty);
    Map ([uint8; uint8; map], fun k v (l, m) ->
      (k, v) :: l, Map.add k v m);
    Map ([uint8; uint8], fun k v ->
      [k, v], Map.singleton k v);
    Map ([uint8; map], fun k (l, m) ->
      let rec rem_all k l =
        let l' = List.remove_assoc k l in
        if l = l' then l else rem_all k l' in
      rem_all k l, Map.remove k m);
    (* merge? *)
    Map ([map; map], fun (l, m) (l', m') ->
      l @ l', Map.union (fun k a b -> Some a) m m');
    Map ([uint8; map], fun k (list, map) ->
      let (l, v, r) = Map.split k map in
      let (l', vr') = List.partition (fun (kx,vx) -> kx < k) list in
      let r' = List.filter (fun (kx, vx) -> kx <> k) vr' in
      let v' = match List.assoc k vr' with n -> Some n | exception Not_found -> None in
      assert (v = v');
      (l' @ List.map (fun (k,v) -> k,v+42) r',
       Map.union (fun k a b -> assert false) l (Map.map (fun v -> v + 42) r)))]

end

let () =
  add_test ~name:"map" [StdMap.map] @@ fun m ->
    check (StdMap.check m)
