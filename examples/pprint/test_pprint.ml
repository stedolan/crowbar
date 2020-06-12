open PPrint
open Crowbar
type t = (string * PPrint.document)
let doc = fix (fun doc -> choose [
  const ("", empty);
  const ("a", PPrint.char 'a');
  const ("123", string "123");
  const ("Hello", string "Hello");
  const ("awordwhichisalittlebittoolong",
         string "awordwhichisalittlebittoolong");
  const ("", hardline);
  map [range 10] (fun n -> ("", break n));
  map [range 10] (fun n -> ("", break n));
  map [doc; doc]
    (fun (sa,da) (sb,db) -> (sa ^ sb, da ^^ db));
  map [range 10; doc] (fun n (s,d) -> (s, nest n d));
  map [doc] (fun (s, d) -> (s, group d));
  map [doc] (fun (s, d) -> (s, align d))
])

let check_doc (s, d) =
  let b = Buffer.create 100 in
  let w = 40 in
  ToBuffer.pretty 1.0 w b d;
  let text = Bytes.to_string (Buffer.to_bytes b) in
  let ws = Str.regexp "[ \t\n\r]*" in
  (* Printf.printf "doc2{\n%s\n}%!" text; *)
  let del_ws = Str.global_replace ws "" in
  (*  Printf.printf "[%s] = [%s]\n%!" (del_ws s) (del_ws text);*)
  Str.split (Str.regexp "\n") text |> List.iter (fun s ->
    let mspace = Str.regexp "[^ ] " in
    if String.length s > w then
      match Str.search_forward mspace s w with
      | _ -> assert false
      | exception Not_found -> ());
  check_eq (del_ws s) (del_ws text)

let () =
  add_test ~name:"pprint" [doc] check_doc
