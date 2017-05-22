open Crowbar
module B = Bencode
let rec bencode = Choose [
  Map ([int], fun i -> B.Integer i);
  Map ([bytes], fun i -> B.String i);
  Map ([List bencode], fun i -> B.List i);
  Map ([List kv], fun xs -> B.dict_of_list xs)]
and kv =
  Map ([bytes; bencode], fun k v -> k, v)

let pp_bencode ppf b =
  Format.fprintf ppf "%s" (B.pretty_print b)

module BS = Bencode_streaming
let check code str offs state =
  let rec run state str offs =
    let open BS.Decode in
    match next state with
    | ParseOk code' -> assert (str = ""); check_eq ~pp:pp_bencode code code'
    | ParseError s -> failwith s
    | ParseEnd -> failwith "string too long"
    | ParsePartial when str = "" -> failwith "string too short"
    | ParsePartial ->
       match offs with
       | [] ->
          feed state str 0 (String.length str);
          run state "" []
       | o :: offs ->
          let n = min o (String.length str) in
          feed state str 0 n;
          run state (String.sub str n (String.length str - n)) offs in
  run state str offs

let () =
  add_test ~name:"bencode" [bencode; List uint8] @@ fun b offs ->
    check b (BS.Encode.to_string b) offs (BS.Decode.create (Bencode_token.Decode.manual ()))

