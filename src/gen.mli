type 'a gen

(* ('k, 'res) gens is an arbitrary legnth of generators,
   where 'k is the type of a function that combines the generators' results into a 'res *)
type ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

(* don't shadow list syntax *)
type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

(* if a function passed to map raises this, then the
   generator will be re-run on different data *)
exception Bad_test of string

(* see crowbar.mli for documentation on these combinators *)
val map : ('f, 'a) gens -> 'f -> 'a gen
val const : 'a -> 'a gen
val choose : 'a gen list -> 'a gen
val unlazy : 'a gen Lazy.t -> 'a gen

(* delay gens f is like map, but delays execution of f

       map gens f ==
       map [delay gens f] (fun c -> c ())

   This is weird and not terribly useful, but needed in fuzz.ml
   so that the testcase generation can be seperated from running
   the test.
 *)
val delay : ('f, 'a) gens -> 'f -> (unit -> 'a) gen


type 'a primitive_generator = Bytebuf.t -> 'a
val primitive : 'a primitive_generator -> 'a gen

(* there are two ways to customise printing of samples:
     - [with_printer p]
       samples are printed by passing the value to p
     - [with_component_printer p] 
       samples are printed by passing printers for each
       subcomponent to p
 *)
val with_printer : 'a Printers.printer -> 'a gen -> 'a gen
val with_component_printer : (unit Printers.printer list -> 'a Printers.printer) -> 'a gen -> 'a gen

type 'a sample

(* [sample] might raise:
     - any exception raised by a generator via [map gens f]
     - Bytebuf.Buffer_exhausted
 *)
val sample : 'a gen -> Bytebuf.t -> 'a sample

val sample_len : 'a sample -> int
val sample_val : 'a sample -> 'a
val pp_sample : 'a sample Printers.printer

(* [mutate s pos buf] mutates a sample at a given position
   (where 0 <= pos < sample_len s) using [buf] to generate
   the new part. Can raise the same exceptions as [sample] *)
val mutate : 'a sample -> int -> Bytebuf.t -> 'a sample

val serialize_into : 'a sample -> Bytebuf.t -> unit
