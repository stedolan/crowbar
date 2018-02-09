module Vec = struct
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
end


type corpus_entry = {
  sample : (unit -> unit) Gen.sample;
  (* The rarest instrumentation bit hit by this sample.
     If this sample hits several equally-rare bits, then pick the lowest.
     That way, several samples that all hit the same rare bits will agree on rarest_bit. *)
  rarest_bit : int;
}

type corpus = {
  generator : (unit -> unit) Gen.gen;
  entries : corpus_entry array;
  (* entries.(best_sample.(i)) is the shortest sample that hits bit i.
     If none of the samples hit bit i, best_sample.(i) = -1 *)
  best_sample : int array;
  (* rarity_count.(i) is the number of samples that have rarest_bit = i *)
  rarity_count : int array;
  (* number of bits hit by this corpus.
     equal to number of nonzero entries of rarity_count, or non-(-1) entries of best_sample *)
  nbits : int;
}

(* count of how often each bit has been seen *)
type counts = int array

let accum_counts (c : counts) (b : Instrumentation.buf) =
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Bytes.unsafe_get (b :> bytes) i <> '\000' then
      Array.unsafe_set c i (Array.unsafe_get c i + 1)
  done

let add_counts (c1 : counts) (c2 : counts) =
  for i = 0 to Instrumentation.buffer_size - 1 do
    Array.unsafe_set c1 i (Array.unsafe_get c1 i + Array.unsafe_get c2 i)
  done

let new_counts () = Array.make Instrumentation.buffer_size 0

let find_rarest_bit counts (b : Instrumentation.buf) =
  let bit = ref 0 in
  let occurrences = ref max_int in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Bytes.unsafe_get (b :> bytes) i <> '\000' then begin
      let c = Array.unsafe_get counts i in
      if c < !occurrences then begin
        occurrences := c;
        bit := i
      end
    end
  done;
  !bit

exception Fail of (unit -> unit) Gen.sample * exn * Printexc.raw_backtrace

let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
  done;
  Bytebuf.of_bytes buf

let rec mutate_sample s =
  let open Gen in
  try mutate s (Random.int (sample_len s)) (mkbuf ()) with
  | Bad_test _ -> mutate_sample s

let rec mk_sample g =
  try Gen.sample g (mkbuf ()) with
  | Gen.Bad_test _ -> mk_sample g

let rate_scale ntests total_counts { rarity_count ; _ } =
  let r = ref 0. in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Array.unsafe_get rarity_count i <> 0 then begin
      r := !r +. 1. /. float_of_int (Array.unsafe_get total_counts i)
    end
  done;
  1. /. (float_of_int !ntests *. !r)

let validate_corpus c =
  for i = 0 to Instrumentation.buffer_size - 1 do
    let n = ref c.rarity_count.(i) in
    c.entries |> Array.iter (fun e ->
      if e.rarest_bit = i then decr n);
    assert (!n = 0)
  done

let cycle ntests total_counts ({ generator; entries = prev_entries; best_sample = prev_best } as corpus) threshold =
  validate_corpus corpus;
  let samples = Vec.create () in
  let best = Array.make Instrumentation.buffer_size (-1) in
  let best_lens = Array.make Instrumentation.buffer_size max_int in
  let rarity_count = Array.make Instrumentation.buffer_size 0 in
  let ibuf = Instrumentation.create_buffer () in
  let counts = new_counts () in
  let rate_scale = rate_scale ntests total_counts corpus in
  Printf.printf "%f\n%!" rate_scale;
  let new_tests = ref 0 in
  let run s =
    (* FIXME: Bad_test handling. ntests? *)
    begin
      match
        Instrumentation.with_instrumentation ibuf (Gen.sample_val s)
      with
      | Ok x -> incr new_tests
      | Error (Gen.Bad_test _) -> ()
      | Error e -> () (* raise (Fail (s, e, Printexc.get_raw_backtrace ())) *) 
    end;
    accum_counts counts ibuf;
    let rarest_bit = find_rarest_bit total_counts ibuf in
    if total_counts.(rarest_bit) + int_of_float (log (float_of_int (Gen.sample_len s))) < threshold then begin
      let idx = Vec.length samples in
      Vec.add samples { sample = s; rarest_bit };
      rarity_count.(rarest_bit) <- rarity_count.(rarest_bit) + 1;
      for i = 0 to Instrumentation.buffer_size - 1 do
        if Bytes.unsafe_get (ibuf :> bytes) i <> '\000' then begin
          if Gen.sample_len s < best_lens.(i) then begin
            best_lens.(i) <- Gen.sample_len s;
            best.(i) <- idx;
          end
        end
      done
    end in
  prev_entries |> Array.iter (fun s ->
    let sharing = float_of_int (corpus.rarity_count.(s.rarest_bit)) in
    let rate = rate_scale *. (float_of_int !ntests /. float_of_int (total_counts.(s.rarest_bit))) in
    let prop = rate /. sharing in
    let count = int_of_float (0.5 +. prop *. float_of_int (Array.length prev_entries * 2)) in
    Printf.printf "%04x(%2d) %4d/%d - %f %d\n" s.rarest_bit corpus.rarity_count.(s.rarest_bit) total_counts.(s.rarest_bit) !ntests prop count;
    for i = 1 to count do
      run (mutate_sample s.sample)
    done);
  run (mk_sample generator);
  add_counts total_counts counts;
  ntests := !ntests + !new_tests;
  let nbits = ref 0 in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if best.(i) = -1 && prev_best.(i) <> -1 then begin
      (* This bit didn't come up at all this round, yet we saw it previously!
         Make sure we don't forget how to trigger this bit, by keeping a test case from the old corpus.
         The same test case might trigger many bits that didn't come up.
         So, we're careful to add it only once. *)
      (* Printf.printf "%d\n%!" i; *)
      let idx = Vec.length samples in
      let rarest_bit = ref 0 in
      let occurrences = ref max_int in
      for j = 0 to Instrumentation.buffer_size - 1 do
        if best.(j) = -1 && prev_best.(j) = prev_best.(i) then begin
          best.(j) <- idx;
          let c = Array.unsafe_get total_counts j in
          if c  < !occurrences then begin
            occurrences := c;
            rarest_bit := j;
          end
        end
      done;
      let rarest_bit = !rarest_bit in
      rarity_count.(rarest_bit) <- rarity_count.(rarest_bit) + 1;
      Vec.add samples { sample = prev_entries.(prev_best.(i)).sample; rarest_bit };
    end;
    if best.(i) <> -1 then incr nbits;
  done;
  { generator; entries = Vec.to_array samples; best_sample = best; nbits = !nbits; rarity_count }
