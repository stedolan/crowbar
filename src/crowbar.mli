type state

type 'a printer = Format.formatter -> 'a -> unit

type 'a code = 'a Ppx_stage.code

type 'a gen

type ('k, 'res) gens =
  | [] : ('res code, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a code -> 'k, 'res) gens
(* re-export stdlib's list
   We only want to override [] syntax in the argument to Map *)
type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

val map : ('f, 'a) gens -> 'f -> 'a gen

val join : 'a gen gen -> 'a gen

(* FIXME val bind : 'a gen -> ('a code -> 'b gen) -> 'b gen *)

val unlazy : 'a gen Lazy.t -> 'a gen

val const : 'a code -> 'a gen
val choose : 'a gen list -> 'a gen

val option : 'a gen -> 'a option gen
val list : 'a gen -> 'a list gen
val list1 : 'a gen -> 'a list gen

val with_printer : 'a printer -> 'a gen -> 'a gen

(* some builtin generators for primitive types *)

val int : int gen
val uint8 : int gen
val int8 : int gen
val int32 : Int32.t gen
val int64 : Int64.t gen
val float : float gen
val bytes : string gen
val bool : bool gen
val range : int -> int gen

val guard : bool -> unit
val bad_test : unit -> 'a
val nonetheless : 'a option -> 'a


(* helper functions for printing *)

(* Format.fprintf, renamed *)
val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val pp_int : int printer
val pp_int32 : Int32.t printer
val pp_int64 : Int64.t printer
val pp_float : float printer
val pp_bool : bool printer
val pp_string : string printer
val pp_list : 'a printer -> 'a list printer



val fail : string -> 'a
val check : bool -> unit
val check_eq : ?pp:('a printer) -> ?cmp:('a -> 'a -> int) -> ?eq:('a -> 'a -> bool) ->
               'a -> 'a -> unit

val add_test :
  ?name:string -> ('f, unit) gens -> 'f -> unit
