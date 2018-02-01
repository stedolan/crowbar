external instrumentation_buffer_size : unit -> int = "caml_instrumentation_buffer_size"

type buf = bytes

let create_buffer () =
  Bytes.make (instrumentation_buffer_size ()) '\000'

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
