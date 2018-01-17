type _ tag = ..
module type T = sig 
  type a
  type _ tag += Tag : a tag
end
type 'a gen_id = (module T with type a = 'a) Lazy.t

let fresh_gen_id (type aa) () : aa gen_id=
  Lazy.from_val (module struct 
     type a = aa
     type _ tag += Tag : a tag
   end : T with type a = aa)

type (_,_) eqp = Eq : ('a, 'a) eqp | NotEq : ('a, 'b) eqp

let compare_gen_id (type a) (type b) (lazy (module X) : a gen_id) (lazy (module Y) : b gen_id) : (a, b) eqp =
  match X.Tag with
  | Y.Tag -> Eq
  | _ -> NotEq


type 'a printer = Format.formatter -> 'a -> unit

type 'a primitive_generator = Bytebuf.t -> 'a



type 'a gen = {
  id : 'a gen_id;
  strategy : 'a gen_strategy;
  printer : 'a printer option;

  (* How large is the smallest value that this generator can make? *)
  small_example_size : int;
}

and 'a gen_strategy =
  | Map : int * ('f, 'a) gens * 'f -> 'a gen_strategy
  | Choose of 'a gen array       (* 1 <= length < 256 *)
  | Unlazy of 'a gen Lazy.t
  | Prim of 'a primitive_generator

(* The type ('func, 'res) gens is a tuple of generators, possibly of
   different types, from which a value of type 'res can be constructed
   using a function of type 'func. *)
and ('func, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('func, 'res) gens -> ('a -> 'func, 'res) gens

type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list


let rec gens_length :
  type k res . (k, res) gens -> int =
  function
  | [] -> 0
  | g :: gs -> 1 + gens_length gs

let map gens f =
  (* The small_example_size of `map gens f` is the product of the
     sizes of gens, plus 1, with a check to avoid overflow *)
  let max_small_size = 1000 in
  let rec compute_example_size :
    type k res . int -> (k, res) gens -> int =
    fun acc gens -> match gens with
    | [] -> acc + 1
    | { small_example_size = s; _ } :: gens ->
       if acc < max_small_size && s < max_small_size then
         compute_example_size (acc * s) gens
       else
         max_int in
  { id = fresh_gen_id ();
    strategy = Map (gens_length gens, gens, f);
    printer = None;
    small_example_size = compute_example_size 1 gens }


let const k = map [] k


let choose gens =
  (* To ensure that the all-zeros input generates small test cases,
     we sort the generators by small_example_size *)
  let arr = Array.of_list gens in
  Array.stable_sort 
    (fun g1 g2 -> compare g1.small_example_size g2.small_example_size)
    arr;

  let max_choose = 256 in
  let mk_choose =
    function
    | [| |] ->
       { id = fresh_gen_id ();
         strategy = Choose [| |];
         printer = None;
         small_example_size = max_int }
    | [| g |] ->
       g  (* hobson's optimisation *)
    | arr ->
       assert (Array.length arr <= max_choose);
       { id = fresh_gen_id ();
         strategy = Choose arr;
         printer = None;
         small_example_size = arr.(0).small_example_size } in

  (* The length of `gens` is arbitrary, but the Choose strategy only
     allows up to 256 generators. If there are more than 256, we need
     to divide it into nested chunks of Choose *)

  let rec div_choose arr =
    let len = Array.length arr in
    if len <= max_choose then
      mk_choose arr
    else begin
      let nchunks = (len + max_choose - 1) / max_choose in
      let chunks = Array.init nchunks (fun i ->
        let chunk_start = i * max_choose in
        let chunk_end = min len (chunk_start + max_choose) in
        let chunk = Array.init (chunk_end - chunk_start)
                               (fun j -> arr.(chunk_start + j)) in
        mk_choose chunk) in
      div_choose chunks
    end in
  div_choose arr



let unlazy gen =
  try Lazy.force gen with
  | Lazy.Undefined ->
     (* If this happens, then we're using `unlazy foo` inside the
        definition of foo, to build a recursive generator. This is
        fine, but since we should never follow the recursion when
        trying to generate a small value, we report the small example
        size as max_int *)
     { id = fresh_gen_id ();    (* FIXME *)
       strategy = Unlazy gen;
       printer = None;
       small_example_size = max_int }





type 'a sample = {
  generator : 'a gen;
  value : 'a;
  length : int;
  components : 'a sample_components;
}

and 'res sample_components =
  | SMap : ('f, 'res) sample_tuple * 'f -> 'res sample_components
  | SChoose of int * 'res sample
  | SPrim of Bytebuf.mark * 'res primitive_generator

and ('k, 'res) sample_tuple =
  | TNil : 'res -> ('res, 'res) sample_tuple
  | TCons : 'a sample * ('k, 'res) sample_tuple -> ('a -> 'k, 'res) sample_tuple


let sample_len s = s.length
let sample_val s = s.value


let mk_sample bytebuf generator components =
  match components with
  | SMap (sample_tuple, f) ->
     (* compute length and result value from sample_tuple *)
     let rec go : 
       type k res . res gen -> res sample_components -> int -> (k, res) sample_tuple -> res sample =
       fun generator components length samples -> match samples with
       | TNil value ->
          { generator; value; length; components }
       | TCons (x, sample_tuple) ->
          go generator components (length + x.length) sample_tuple in
     go generator components 0 sample_tuple
  | SChoose (tag, t) ->
     { generator; value = t.value; length = t.length + 1; components }
  | SPrim (mark, p) ->
     let value = p bytebuf in
     { generator; value; components;
       length = Bytebuf.extent_since bytebuf mark }


let rec sample :
  type a . a gen -> Bytebuf.t -> int -> a sample =
  fun gen bytebuf size ->
(*  if bytebuf.pos >= bytebuf.len then
    raise No_more_bytes
    match gen.small_examples with
    | [| |] -> failwith "No small examples for generator!"
    | sm -> sm.(0)
  else if size <= 1 then
    let b = read_byte bytebuf in
    match gen.small_examples with
    | [| |] -> failwith "No small examples for generator!"
    | sm -> sm.(b mod Array.length sm)
  else *)
    match gen.strategy with
    | Map (count, gens, f) ->
       mk_sample bytebuf gen (SMap (sample_gens gens f bytebuf size count, f))
    | Choose gens ->
       let b = Bytebuf.read_byte bytebuf in
       let value = sample gens.(b mod Array.length gens) bytebuf (size - 1) in
       (* FIXME: update? *)
       mk_sample bytebuf gen (SChoose (b, value))
    | Unlazy gen ->
       sample (Lazy.force gen) bytebuf size
    | Prim p ->
       mk_sample bytebuf gen (SPrim (Bytebuf.mark bytebuf, p))

and sample_gens :
  type f res . (f, res) gens -> f -> Bytebuf.t -> int -> int -> (f, res) sample_tuple =
  fun gens f state size count -> match gens with
  | [] ->
     TNil f
  | gen :: gens ->
     let x = sample gen state ((size - 1) / count) in
     let sample_tuple = sample_gens gens (f x.value) state size count in
     TCons (x, sample_tuple)

(*
let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
  done;
  Bytebuf.of_bytes buf
*)

let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
    (* Printf.printf "%02x" (Char.code (Bytes.get buf i)); *)
  done;
  (* Printf.printf "\n%!"; *)
  Bytebuf.of_bytes buf


let rec mutate :
  type a . a sample -> int ref -> Bytebuf.t -> a sample =
  fun s pos bytebuf ->
  match s.components with
  | _ when !pos = 0 ->
     (* found the spot to mutate *)
     pos := -s.length;
     sample s.generator bytebuf s.length
  | _ when !pos < 0 || !pos > s.length ->
     (* this subtree remains untouched *)
     pos := !pos - s.length;
     s
  | SChoose (ch, t) ->
     decr pos;  (* skip tag byte *)
     mk_sample bytebuf s.generator (SChoose (ch, mutate t pos bytebuf))
  | SMap (scases, f) ->
     mk_sample bytebuf s.generator (SMap (mutate_gens scases f pos bytebuf, f))
  | SPrim (mark, p) ->
     (* FIXME *)
     assert false

and mutate_gens :
  type f res . (f, res) sample_tuple -> f -> int ref -> Bytebuf.t -> (f, res) sample_tuple =
  fun scases f pos bytebuf -> match scases with
  | TNil _ ->
     TNil f
  | TCons (subcase, sample_tuple) ->
     let subcase' = mutate subcase pos bytebuf in
     let sample_tuple' = mutate_gens sample_tuple (f subcase'.value) pos bytebuf in
     TCons (subcase', sample_tuple')

let mutate sample pos bytebuf =
  if pos < 0 || pos > sample.length then
    raise (Invalid_argument "Gen.mutate: invalid position");
  mutate sample (ref pos) bytebuf


let rec serialize_into :
  type a . a sample -> Bytebuf.t -> unit =
  fun s b ->
  assert (b.len - b.pos >= s.length);
  match s.components with
  | SMap (subcases, f) ->
     serialize_tuple_into subcases b
  | SChoose (tag, tc) ->
     Bytebuf.write_char b (Char.chr tag);
     serialize_into tc b
  | SPrim (mark, p) ->
     Bytebuf.copy_since_mark b mark s.length

and serialize_tuple_into :
  type k res . (k, res) sample_tuple -> Bytebuf.t -> unit =
  fun ss b -> match ss with
  | TNil _ -> ()
  | TCons (s, rest) -> serialize_into s b; serialize_tuple_into rest b
