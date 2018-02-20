external instrumentation_buffer_size : unit -> int = "caml_instrumentation_buffer_size"

let buffer_size = instrumentation_buffer_size ()

type buf = bytes

let create_buffer () =
  Bytes.make (instrumentation_buffer_size ()) '\000'

let copy_buffer b =
  Bytes.copy b

external gather_instrumentation : bytes -> unit = "caml_gather_instrumentation"
external reset_instrumentation : bool -> unit = "caml_reset_afl_instrumentation"

let with_instrumentation buf f =
  reset_instrumentation true;
  let v =
    (* Sys.opaque_identity inhibits inlining *)
    match (Sys.opaque_identity f) () with
    | x -> Ok x
    | exception e -> Error e in
  gather_instrumentation buf;
  v

external unsafe_get_32 : Bytes.t -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : Bytes.t -> int -> int64 = "%caml_string_get64u"
external unsafe_set_32 : Bytes.t -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

(* This function finds bits set in the bytestring `new_bits`
   which are not set in the bytestring `old_bits`.
   For performance, it's a bit nasty *)
let find_new_bits ~old_bits ~new_bits ~out =
  let r = ref 0 in
  assert (Bytes.length new_bits = Bytes.length old_bits);
  let i = ref 0 in
  let numwords = Bytes.length new_bits / 8 in
  while !i < numwords do
    let seen = unsafe_get_64 old_bits (!i * 8) in
    let newly_seen = Int64.logor (unsafe_get_64 new_bits (!i * 8)) seen in
    if seen = newly_seen then
      (* fast path *)
      incr i
    else begin
      for j = !i * 8 to !i * 8 + 7 do
        let seen = Char.code (Bytes.get old_bits j) in
        let newly_seen = Char.code (Bytes.get new_bits j) lor seen in
        if seen <> newly_seen then begin
          out.(!r) <- j;
          incr r
        end
      done;
      unsafe_set_64 old_bits (!i * 8) newly_seen;
      incr i
    end
  done;
  !r

type log_entry = Entry of {
  ntests : int;
  nbits : int;
  new_bits : int list;
  status : (string * Yojson.Basic.json) list;
  (* FIXME: add timings *)
}

let print_entry (Entry { ntests; nbits; new_bits; status }) =
  Printf.printf "%6d %4d" ntests nbits;
  status |> List.iter (fun (k, v) ->
    match v with
    | `Bool true -> Printf.printf "%s: t" k
    | `Bool false -> Printf.printf "%s: f" k
    | `Float f -> Printf.printf "%s:%6.2f" k f
    | `Int i -> Printf.printf "%s:%5d" k i
    | `Null -> ()
    | `String s -> Printf.printf "%s: %10s" k s
    | j -> Printf.printf "%s: %s" k (Yojson.Basic.to_string j));
  begin match new_bits with
  | [] -> ()
  | [b] -> Printf.printf " [%04x]" b
  | b :: bs ->
     Printf.printf " [%04x" b;
     bs |> List.iter (Printf.printf " %04x");
     Printf.printf "]"
  end;
  Printf.printf "\n%!"

type log = {
  debug : bool;
  params : (string * Yojson.Basic.json) list;
  mutable entries : log_entry list;
}

type loginfo = (string * Yojson.Basic.json) list

type accumulator = {
  (* temporary buffer, reused by every call to run *)
  ibuf : buf;

  (* counts.(i) is the number of times bit i has occurred *)
  counts : int array;
  (* nbits is the number of nonzero entries of counts *)
  mutable nbits : int;
  (* ntests is the number of tests that have been run *)
  mutable ntests : int;

  log : log;
}

let create_accumulator ?(debug_log=false) ?(params=[]) () =
  { ibuf = create_buffer ();
    counts = Array.make buffer_size 0;
    nbits = 0;
    ntests = 0;
    log = { debug = debug_log ; params; entries = [] } }

let log_to_json { log = { entries; params; _ }; _ } : Yojson.Basic.json =
  entries
  |> List.rev
  |> List.map (fun (Entry { ntests; nbits; new_bits; status }) ->
     `Assoc ([
        "tests", `Int ntests;
        "bits", `Int nbits;
        "new_bits", `List (List.map (fun i ->
                        `String (Printf.sprintf "%04x" i)) new_bits)]
        @ status))
  |> fun e -> `Assoc (params @ ["log", `List e])

type 'a run_result = {
  result : ('a, exn) result;
  rarest_bit : int;
  rarest_count : int;
  new_bits : int list;
}


let run ?status acc f =
  let result = with_instrumentation acc.ibuf f in
  let counts = acc.counts in
  let new_bits = ref [] in
  let rarest_bit = ref 0 in
  let rarest_count = ref max_int in
  assert (Bytes.length acc.ibuf = buffer_size);
  assert (Array.length counts = buffer_size);
  for i = 0 to buffer_size - 1 do
    if Bytes.unsafe_get acc.ibuf i <> '\000' then begin
      let c = Array.unsafe_get counts i in
      if c < !rarest_count then begin
        rarest_count := c;
        rarest_bit := i;
      end;
      if c = 0 then begin
        acc.nbits <- acc.nbits + 1;
        new_bits := i :: !new_bits;
      end;
      Array.unsafe_set counts i (c + 1);
    end
  done;
  new_bits := List.rev !new_bits;
  acc.ntests <- acc.ntests + 1;
  if acc.log.debug || !new_bits <> [] then begin
    let status = match status with Some f -> f () | _ -> [] in
    let entry = Entry { ntests = acc.ntests; nbits = acc.nbits;
                        new_bits = !new_bits; status } in
    if !new_bits <> [] then acc.log.entries <- entry :: acc.log.entries;
    print_entry entry;
  end;
  { result;
    rarest_bit = !rarest_bit;
    rarest_count = !rarest_count;
    new_bits = !new_bits }
