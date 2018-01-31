(* Typed identifers.
   These are essentially just unique integers, allocated with fresh ().
   They can be compared with equal_t.
   If two typed identifiers are equal, then their type parameters must also be equal. *)
type 'a t

val fresh : unit -> 'a t

val to_int : 'a t -> int
val equal : 'a t -> 'b t -> bool

(* Note the type of Eq.
   If two identifiers are equal, they must have the same type. *)
type (_,_) eqp = Eq : ('a, 'a) eqp | Not_Eq : ('a, 'b) eqp
val equal_t : 'a t -> 'b t -> ('a, 'b) eqp
