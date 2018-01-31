(* To be able to write equal_t,
   identifiers are implemented as cases of an extensible GADT. *)
type _ tag = ..

(* The only way to carry around a case of an extensible GADT
   in such a way that it can be used in `match` is to wrap it up
   in a first-class module *)
module type T = sig 
  type a
  type _ tag += Tag : a tag
end
type 'a t = (module T with type a = 'a)

let fresh (type aa) () : aa t =
  (module struct 
    type a = aa
    type _ tag += Tag : a tag
  end)

type (_,_) eqp = Eq : ('a, 'a) eqp | Not_Eq : ('a, 'b) eqp

(* This involves Obj, but the safe parts: it converts Tags to their underlying ID *)
let to_int (type a) ((module X) : a t) =
  Obj.extension_id (Obj.extension_constructor X.Tag)

let equal_t (type a) (type b) ((module X) : a t) ((module Y) : b t) : (a, b) eqp =
  match X.Tag with
  | Y.Tag ->
     (* Since X.Tag matches Y.Tag, the types `a` and `b` 
        must be equal, so we can use Eq *)
     Eq
  | _ ->
     Not_Eq

let equal (type a) (type b) (a : a t) (b : b t) =
  match equal_t a b with Eq -> true | Not_Eq -> false
