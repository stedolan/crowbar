type 'a gen


type ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

val map : ('f, 'a) gens -> 'f -> 'a gen
val const : 'a -> 'a gen
val choose : 'a gen list -> 'a gen
val unlazy : 'a gen Lazy.t -> 'a gen



type 'a sample
val sample_len : 'a sample -> int
val sample_val : 'a sample -> 'a
val sample : 'a gen -> Bytebuf.t -> int -> 'a sample
val mutate : 'a sample -> int -> Bytebuf.t -> 'a sample
val serialize_into : 'a sample -> Bytebuf.t -> unit
