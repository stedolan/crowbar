open Crowbar
open Bson

let rec doc = Choose [
  Const empty;
  Map ([ename; elem; doc], 
        fun s e d ->
        let s = String.map (fun c ->
                    Char.chr ((Char.code c land 0x7f) lor 1)) s in
        add_element s e d)]
and elem = Choose [
  Map ([float], create_double);
  (*Map ([bytes], create_string);*)
  Map ([doc], create_doc_element);
  Map ([List elem], create_list);
  Map ([List doc], create_doc_element_list);
  Map ([bytes], create_user_binary);
  Map ([bool], create_boolean);
  Map ([int64], create_utc);
  Const (create_null ());
  (*Map ([bytes; bytes], create_regex);*)
  (*Map ([bytes], create_jscode);*)
  (*Map ([bytes; doc], create_jscode_w_s);*)
  Map ([int32], create_int32);
  Map ([int64], create_int64)]
and ename = Map ([uint8], fun i -> "e" ^ string_of_int i)

let pp_bson ppf doc =
  Format.fprintf ppf "%s" (to_simple_json doc)

let () =
  add_test ~name:"bson" [doc] @@ fun doc ->
    check_eq ~pp:pp_bson doc (decode (encode (decode (encode doc))))
(*    let s = encode doc in
    check_eq ~pp:pp_string s (encode (decode s))
*)
