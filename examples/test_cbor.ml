module CS = CBOR.Simple
open Crowbar
let rec cbor = Choose [
  Const `Null;
  Const `Undefined;
  Map ([bool], fun b -> `Bool b);
  Map ([int], fun i -> `Int i);
  (*Map ([float], fun f -> `Float f);*)
  Map ([bytes], fun f -> `Bytes f);
  Map ([bytes], fun f -> `Text f);
  Map ([List cbor], fun xs -> `Array xs);
  Map ([List kv], fun xs -> `Map xs)]
and kv =
  Map ([cbor; cbor], fun k v -> k, v)

let pp_cbor ppf cbor =
  pp_string ppf (CS.to_diagnostic cbor)

let () =
  add_test ~name:"cbor" [cbor] @@ fun cbor ->
    let cbor' = cbor |> CS.encode |> CS.decode in
    check_eq ~pp:pp_cbor cbor cbor'

