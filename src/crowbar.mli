type state

type 'a printer = Format.formatter -> 'a -> unit

type 'a gen =
  | Const of 'a
  | Choose of 'a gen list
  | Map : ('f, 'a) gens * 'f -> 'a gen
  | Option : 'a gen -> 'a option gen
  | List : 'a gen -> 'a list gen
  | List1 : 'a gen -> 'a list gen
  | Primitive of (state -> 'a)
  | Print of 'a printer * 'a gen

and ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

(* re-export stdlib's list
   We only want to override [] syntax in
   the argument to Map *)
type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

val int : int gen
val uint8 : int gen
val int8 : int gen
val int32 : Int32.t gen
val int64 : Int64.t gen
val float : float gen
val bytes : string gen
val bool : bool gen

val guard : bool -> unit
val bad_test : unit -> 'a


(* Format.fprintf *)
val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val pp_int : int printer
val pp_float : float printer
val pp_bool : bool printer
val pp_string : string printer
val pp_list : 'a printer -> 'a list printer

type test_result = (unit, unit printer) result
val check : bool -> test_result
val check_eq : ?pp:('a printer) -> ?cmp:('a -> 'a -> int) -> ?eq:('a -> 'a -> bool) ->
               'a -> 'a -> test_result


val add_test :
  ?name:string -> ('f, test_result) gens -> 'f -> unit
