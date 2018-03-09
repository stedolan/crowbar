open Printers
open Gen

let bool =
  primitive ~size:1 Bytebuf.read_bool
  |> with_printer pp_bool

let uint8 =
  primitive ~size:1 Bytebuf.read_byte
  |> with_printer pp_int

let int8 =
  map [uint8] (fun n -> n - 128)
  |> with_printer pp_int

let int32 =
  primitive ~size:4 Bytebuf.read_int32
  |> with_printer pp_int32

let int64 =
  primitive ~size:8 Bytebuf.read_int64
  |> with_printer pp_int64

let int =
  (if Sys.word_size <= 32 then
     map [int32] Int32.to_int
   else
     map [int64] Int64.to_int)
  |> with_printer pp_int

let float =
  primitive ~size:8 Bytebuf.read_float
  |> with_printer pp_float

(* maybe print as a hexdump? *)
let bytes = primitive (fun src ->
  (* null-terminated, with '\001' as an escape code *)
  let buf = Bytes.make 64 '\255' in
  let rec read_bytes p =
    if p >= Bytes.length buf then p else
    match Bytebuf.read_char src with
    | '\000' -> p
    | '\001' ->
       Bytes.set buf p (Bytebuf.read_char src);
       read_bytes (p + 1)
    | c ->
       Bytes.set buf p c;
       read_bytes (p + 1) in
  let count = read_bytes 0 in
  Bytes.sub_string buf 0 count)
  |> with_printer pp_string

let bytes_fixed n = primitive (fun src ->
  Bytebuf.read_bytes src n)
  |> with_printer pp_string

let range n =
  if n <= 0 then
    raise (Invalid_argument "Crowbar.range: argument must be positive")
  else with_printer pp_int (
    if n = 1 then
      const 0
    else if n < 100 then
      map [uint8] (fun x -> x mod n)
    else if n < 0x1000000 then
      map [int32] (fun x ->
          Int32.(to_int (abs (rem x (of_int n)))))
    else
      map [int64] (fun x ->
          Int64.(to_int (abs (rem x (of_int n))))))


let fix f =
  let rec g = lazy (f (unlazy g)) in
  unlazy g

let option g =
  choose [const None; map [g] (fun x -> Some x)]
  |> with_component_printer
       (fun comps ppf x -> match x with
        | None -> pp ppf "None"
        | Some _ -> pp_printer ppf (List.hd comps))

let mk_list_gens g =
  let rec list = lazy (choose [
    const [];
    unlazy list1]
    |> with_component_printer (fun comps ppf xs ->
      match xs, comps with
      | [], _ -> ()
      | _, [pc] -> pc ppf ()
      | _ -> assert false))
  and list1 = lazy (
    map [g; unlazy list] (fun x xs -> x :: xs)
    |> with_component_printer (fun comps ppf xs ->
      match xs, comps with
      | [x], [px; _] ->
         px ppf ()
      | x :: xs, [px; pxs] ->
         pp ppf "%a;@ %a" pp_printer px pp_printer pxs
      | _ -> assert false)) in
  unlazy list, unlazy list1

let list g = mk_list_gens g |> fst |> with_component_printer (fun comps ppf list ->
  pp_list pp_printer ppf comps)

let list1 g = mk_list_gens g |> snd |> with_component_printer (fun comps ppf list ->
  pp_list pp_printer ppf comps)

let guard = function
  | true -> ()
  | false -> raise (Bad_test "guard failed")
let bad_test () = raise (Bad_test "bad test")
let nonetheless = function
  | None -> bad_test ()
  | Some a -> a

exception Failed_test of unit printer

let fail s = raise (Failed_test (fun ppf () -> pp_string ppf s))

let check = function
  | true -> ()
  | false -> raise (Failed_test (fun ppf () -> pp ppf "check false"))

let check_eq ?pp:pv ?cmp ?eq a b =
  let pass = match eq, cmp with
    | Some eq, _ -> eq a b
    | None, Some cmp -> cmp a b = 0
    | None, None ->
       Pervasives.compare a b = 0 in
  if pass then
    ()
  else
    raise (Failed_test (fun ppf () ->
      match pv with
      | None -> pp ppf "different"
      | Some pv -> pp ppf "@[<hv>%a@ !=@ %a@]" pv a pv b))
