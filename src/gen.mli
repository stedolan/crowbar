type 'a gen

(* ('k, 'res) gens is an arbitrary legnth of generators,
   where 'k is the type of a function that combines the generators' results into a 'res *)
type ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

(* don't shadow list syntax *)
type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list


(* see crowbar.mli for documentation on these combinators *)
val map : ('f, 'a) gens -> 'f -> 'a gen
val const : 'a -> 'a gen
val choose : 'a gen list -> 'a gen
val unlazy : 'a gen Lazy.t -> 'a gen

val with_printer : 'a Printers.printer -> 'a gen -> 'a gen
val with_component_printer : (unit Printers.printer list -> 'a Printers.printer) -> 'a gen -> 'a gen

type 'a sample
val sample_len : 'a sample -> int
val sample_val : 'a sample -> 'a
val sample : 'a gen -> Bytebuf.t -> int -> 'a sample
val print : 'a sample Printers.printer
val mutate : 'a sample -> int -> Bytebuf.t -> 'a sample
val serialize_into : 'a sample -> Bytebuf.t -> unit
