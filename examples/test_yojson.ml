type t = Yojson.Safe.json
open Crowbar
let rec json = Choose [
  Map ([bool], fun b -> `Bool b);
  Map ([int], fun i -> `Int i);
  Map ([List json], fun xs -> `List xs);
  Const `Null;
  Map ([float], fun f -> `Float f);
  Map ([str], fun s -> `String s);
  Map ([List field], fun xs -> `Assoc xs);
  Map ([List json], fun xs -> `Tuple xs);
  Map ([str; Option json], fun s e -> `Variant (s, e))]
and str = Choose [Const "a"; Const "b"; Const "c"] (* bytes? *)
and field = Map ([str; json], fun s e -> s, e)

let pp_json ppf json =
  pp_string ppf (Yojson.Safe.pretty_to_string json)

let () =
  add_test ~name:"yojson" [json] @@ fun (json : t) ->
    let s = Yojson.Safe.to_string json in
    check_eq ~pp:pp_json json (Yojson.Safe.from_string s)
