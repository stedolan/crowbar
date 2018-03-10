type buf = private bytes
type log


type accumulator = private {
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

type loginfo = (string * Yojson.Basic.json) list

val create_accumulator : ?debug_log:bool -> ?params:loginfo -> unit -> accumulator

type 'a run_result = private {
  result : ('a, exn) result;
  (* The index of the instrumentation bit triggered by this run,
     which has previously been seen the fewest times.
     Ties are broken in favour of the lower index *)
  rarest_bit : int;
  (* The number of times the rarest_bit has been triggered,
     including this time *)
  rarest_count : int;
  (* The bits triggered by this test case that were
     never previously seen, in ascending order.
     If nonempty, rarest_bit is the head, and rarest_count = 1. *)
  new_bits : int list;
}

val run : ?status:(unit -> loginfo) -> accumulator -> (unit -> 'a) -> 'a run_result

val log_to_json : accumulator -> Yojson.Basic.json

(* Low-level instrumenation API *)

val buffer_size : int
val create_buffer : unit -> buf
val copy_buffer : buf -> buf

(* [with_instrumentation f buf] runs f, collecting instrumentation in buf *)
val with_instrumentation : buf -> (unit -> 'a) -> ('a, exn) result

(* [find_new_bits ~old_bits ~new_bits ~output] finds the bits set
   in ~new_bits but not ~old_bits, writes them to ~output, and
   returns how many there are *)
val find_new_bits : old_bits:buf -> new_bits:buf -> out:int array -> int

type backtrace = Printexc.Slot.t list
val capture_backtrace_for_bit : int -> (unit -> 'a) -> string * string
val pp_backtrace_tree : Format.formatter -> backtrace list -> unit
