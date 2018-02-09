type buf = private bytes
val buffer_size : int
val create_buffer : unit -> buf
val copy_buffer : buf -> buf

(* [with_instrumentation f buf] runs f, collecting instrumentation in buf *)
val with_instrumentation : buf -> (unit -> 'a) -> ('a, exn) result

(* [find_new_bits ~old_bits ~new_bits ~output] finds the bits set
   in ~new_bits but not ~old_bits, writes them to ~output, and
   returns how many there are *)
val find_new_bits : old_bits:buf -> new_bits:buf -> out:int array -> int
