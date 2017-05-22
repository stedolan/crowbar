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
  (* Const ("", hardline); *)
  Map ([range 10], fun n -> ("", break n));
  Map ([range 10], fun n -> ("", break n));
  Map ([doc; doc], fun (sa,da) (sb,db) -> (sa ^ sb, da ^^ db));
  Map ([range 10; doc], fun n (s,d) -> (s, nest n d));
  Map ([doc], fun (s, d) -> (s, group d));
  Map ([doc], fun (s, d) -> (s, align d))
]

let ws = Str.regexp "[ \t\n\r]*"
let del_ws = Str.global_replace ws ""

let to_string w d =
  let b = Buffer.create 100 in
  ToBuffer.pretty 1.0 w b d;
  Bytes.to_string (Buffer.to_bytes b)

let check_doc (s, d) w =
  let req = requirement d in
  let text = to_string w d in
  Printf.printf "[%s] (%d){\n%s\n}\n%!" s (requirement d) text;
  assert (not (String.contains (to_string (requirement d) (group d)) '\n'));
  Str.split (Str.regexp "\n") text |> List.iter (fun s ->
    let mspace = Str.regexp "[^ ] " in
    if String.length s > w then
(*      if req <= w then
        failwith ("{" ^ s ^ "} is too long")
      else*)
        match Str.search_forward mspace s w with
        | _ -> assert false
        | exception Not_found -> ());
  check_eq s (del_ws text)

let () =
  add_test ~name:"pprint" [doc; uint8] check_doc

