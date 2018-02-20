type 'a t = {
  mutable elems : 'a array;
  mutable len : int;
  mutable cap : int;
}
let length { elems; len } = len
let add v x =
  if v.len = v.cap then begin
    let old_elems = v.elems in
    v.cap <- v.cap * 2 + 32;
    v.elems <- Array.make (v.cap * 2 + 32) x;
    Array.blit old_elems 0 v.elems 0 v.len;
  end;
  Array.unsafe_set v.elems v.len x; v.len <- v.len + 1
let create () = { elems = [| |]; len = 0; cap = 0 }
let to_array v =
  Array.init v.len (Array.get v.elems)
