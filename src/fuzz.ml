external instrumentation_buffer_size : unit -> int = "caml_instrumentation_buffer_size"
external gather_instrumentation : bytes -> unit = "caml_gather_instrumentation"
external reset_instrumentation : bool -> unit = "caml_reset_afl_instrumentation"

let with_instrumentation buf f =
  reset_instrumentation true;
  let v =
    match (Sys.opaque_identity f) () with
    | x -> Ok x
    | exception e -> Error e in
  gather_instrumentation buf;
  v

external unsafe_get_32 : Bytes.t -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : Bytes.t -> int -> int64 = "%caml_string_get64u"
external unsafe_set_32 : Bytes.t -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

(* This function finds bits set in the bytestring `ibuf`
   which are not set in the bytestring `already_seen`.
   For performance, it's a bit nasty *)
let find_new_bits ibuf already_seen out =
  let r = ref 0 in
  assert (Bytes.length ibuf = Bytes.length already_seen);
  let i = ref 0 in
  let numwords = Bytes.length ibuf / 8 in
  while !i < numwords do
    let seen = unsafe_get_64 already_seen (!i * 8) in
    let newly_seen = Int64.logor (unsafe_get_64 ibuf (!i * 8)) seen in
    if seen = newly_seen then
      (* fast path *)
      incr i
    else begin
      for j = !i * 8 to !i * 8 + 7 do
        let seen = Char.code (Bytes.get already_seen j) in
        let newly_seen = Char.code (Bytes.get ibuf j) lor seen in
        if seen <> newly_seen then begin
          out.(!r) <- j;
          incr r
        end
      done;
      unsafe_set_64 already_seen (!i * 8) newly_seen;
      incr i
    end
  done;
  !r

let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
  done;
  Bytebuf.of_bytes buf


open Gen

let fuzz (gen : (unit -> unit) gen) =
  let () = (* Random.init 123 *) Random.self_init () in
  let ibuf = Bytes.create (instrumentation_buffer_size ()) in
  let seen = Bytes.create (instrumentation_buffer_size ()) in
  let new_hits = Array.make (instrumentation_buffer_size ()) 0 in
  let q = Queue.create () in
  let ntests = ref 0 in
  let nbugs = ref 0 in
  Queue.add (0, sample gen (mkbuf ()) 1000) q;
  while not (Queue.is_empty q) do
    let depth, tc = Queue.pop q in
    try
    for i = 0 to 10 do
      incr ntests;
      let tc = mutate tc (Random.int (sample_len tc)) (mkbuf ()) in
      with_instrumentation ibuf (sample_val tc) |> 
          (function Ok x -> () | Error e -> raise e);
      let count = find_new_bits ibuf seen new_hits in
      Printf.fprintf stdout "%d %d %d\n%!" depth count (Queue.length q);
      if count > 0 then Queue.add (depth + 1, tc) q
    done
    with e -> begin
      incr nbugs; if !nbugs = 1 then (Printf.printf "%d\n%!" !ntests; exit 0);
      Printf.printf "%d\n%!" !ntests; ntests := 0; Queue.clear q; Queue.add (0, sample gen (mkbuf ()) 1000) q; Bytes.fill seen 0 (Bytes.length seen) '\000' 
    end
  done

