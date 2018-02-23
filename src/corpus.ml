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

let count_bits (c : counts) =
  let n = ref 0 in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Array.unsafe_get c i <> 0 then
      incr n
  done;
  !n

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
  if Random.int 100 < 5 then
    (*(Gen.Fragment_Pool.add tbl s; Gen.splice tbl s)*)
    match Gen.dup s with
    | Some s' -> 
       Format.printf "dup@.: %a@.: %a@." Gen.pp_sample s Gen.pp_sample s';
       s'
    | None -> mutate_sample s
  else
  let open Gen in
  match mutate s (Random.int (sample_len s)) (mkbuf ()) with
  | exception Bad_test _ -> mutate_sample s
  | s' ->
     if Random.int 100 < 90 then s' else mutate_sample s'

(*
let rec mutate_sample s =
  let open Gen in
  try mutate s (Random.int (sample_len s)) (mkbuf ()) with
  | Bad_test _ -> mutate_sample s
*)


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

let cycle ntests total_counts ({ generator; entries = prev_entries; best_sample = prev_best } as corpus) =
  validate_corpus corpus;
  let samples = Vec.create () in
  let best = Array.make Instrumentation.buffer_size (-1) in
  let best_lens = Array.make Instrumentation.buffer_size max_int in
  let rarity_count = Array.make Instrumentation.buffer_size 0 in
  let ibuf = Instrumentation.create_buffer () in
  let counts = new_counts () in
  let rate_scale = rate_scale ntests total_counts corpus in
  let num_bits_found =
    let n = ref 0 in
    for i = 0 to Instrumentation.buffer_size - 1 do
      if Array.unsafe_get corpus.rarity_count i <> 0 then incr n
    done;
    !n in
  (* Printf.printf "%d %f\n%!" num_bits_found rate_scale; *)
  let new_tests = ref 0 in
  let run s =
    (* FIXME: Bad_test handling. ntests? *)
    begin
      match
        Instrumentation.with_instrumentation ibuf (Gen.sample_val s)
      with
      | Ok x -> incr new_tests
      | Error (Gen.Bad_test _) -> ()
      | Error e -> raise (Fail (s, e, Printexc.get_raw_backtrace ()))
    end;
    accum_counts counts ibuf;
    let rarest_bit = find_rarest_bit total_counts ibuf in
    if true || total_counts.(rarest_bit) + int_of_float (log (float_of_int (Gen.sample_len s))) < max (!ntests / 100) 1000 then begin
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
    let fcount = prop *. 1000. (*float_of_int corpus.nbits *. 1.5 +. 1.*) (*float_of_int num_bits_found *. 20.*) (*float_of_int (Array.length prev_entries) *. 2.*) in
    (* random rounding *)
    let count =
      let frac, integer = modf fcount in
      int_of_float integer + (if Random.float 1. < frac then 1 else 0) in
    Printf.printf "%04x(%2d) %4d/%d - %f %d\n" s.rarest_bit corpus.rarity_count.(s.rarest_bit) total_counts.(s.rarest_bit) !ntests prop count;
    for i = 1 to count do
      run (mutate_sample s.sample)
    done);
  run (mk_sample generator);
  add_counts total_counts counts;
  ntests := !ntests + !new_tests;
  let nbits = ref 0 in
  let nkept = ref 0 in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if best.(i) = -1 && prev_best.(i) <> -1 then begin
      (* This bit didn't come up at all this round, yet we saw it previously!
         Make sure we don't forget how to trigger this bit, by keeping a test case from the old corpus.
         The same test case might trigger many bits that didn't come up.
         So, we're careful to add it only once. *)
      incr nkept;
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
  Printf.printf "kept: %d\n%!" !nkept;
  { generator; entries = Vec.to_array samples; best_sample = best; nbits = !nbits; rarity_count }



type entry = {
  sample : (unit -> unit) Gen.sample;
  (* The rarest instrumentation bit hit by this sample.
     If this sample hits several equally-rare bits, then pick the lowest.
     That way, several samples that all hit the same rare bits will agree on rarest_bit. *)
  rarest_bit : int;
  (* The number of times the rarest bit had previously been seen *)
  bit_occurrences : int;
  (* The number of tests that had previously been run *)
  ntests : int;


  (* For testing only: too expensive to keep forever *)
  instrumentation: Instrumentation.buf;
}


let run acc s =
  let res = Instrumentation.run acc (Gen.sample_val s) in

  res.result, { sample = s; rarest_bit = res.rarest_bit; bit_occurrences = res.rarest_count; ntests = acc.ntests; instrumentation = Instrumentation.copy_buffer acc.ibuf }

let update_stats (acc : Instrumentation.accumulator) e =
  { e with bit_occurrences = acc.counts.(e.rarest_bit); ntests = acc.ntests }
  


type qcorpus = {
  entries : entry Queue.t;
  (* rarity_count.(i) is the number of entries that have rarest_bit = i *)
  rarity_count : int array;
  (* num_rare_bits is the number of nonzero entries of rarity_count *)
  mutable num_rare_bits : int;
  (* Sum of (1/bit_occurrences) over the queue *)
  mutable rate_sum : float;
}


let qnbits q =
  let acc = new_counts () in
  q.entries |> Queue.iter (fun e ->
    accum_counts acc e.instrumentation);
  count_bits acc

let create_queue () =
  { entries = Queue.create ();
    rarity_count = Array.make Instrumentation.buffer_size 0;
    num_rare_bits = 0;
    rate_sum = 0. }

let validate_qcorpus q =
  let rarity_count = Array.copy q.rarity_count in
  let num_rare_bits = ref q.num_rare_bits in
  rarity_count |> Array.iter (fun n -> if n <> 0 then decr num_rare_bits);
  assert (!num_rare_bits = 0);
  q.entries |> Queue.iter (fun e ->
    rarity_count.(e.rarest_bit) <- rarity_count.(e.rarest_bit) - 1);
  rarity_count |> Array.iter (fun n -> assert (n = 0));
  let rsum = ref 0.0 in
  q.entries |> Queue.iter (fun e ->
    rsum := !rsum +. float_of_int e.ntests /. float_of_int e.bit_occurrences);
  assert (abs_float (!rsum -. q.rate_sum) < 1e-6)


let qcorpus_add q e =
  (* validate_qcorpus q; *)
  if q.rarity_count.(e.rarest_bit) = 0 then
    q.num_rare_bits <- q.num_rare_bits + 1;
  q.rarity_count.(e.rarest_bit) <- q.rarity_count.(e.rarest_bit) + 1;
  q.rate_sum <- q.rate_sum +. float_of_int e.ntests /. float_of_int e.bit_occurrences;
  Queue.add e q.entries
  (* validate_qcorpus q *)

let qcorpus_remove acc q work =
  (* validate_qcorpus q; *)
  let e = Queue.pop q.entries in

  let sharing = float_of_int (q.rarity_count.(e.rarest_bit)) in (* FIXME: keep old value? harmonic sum of shards here? *)

  (* FIXME: q.counts.(e.rarest_bit) ? *)
  (* let rate = q.rate_sum /. float_of_int acc.counts.(e.rarest_bit) in  *)
  let lambda = float_of_int e.ntests /. float_of_int e.bit_occurrences in
  let rate = (1./.q.rate_sum) *. lambda in

  let prop = rate /. sharing in
  (* Printf.printf "%f %f %d %f\n%!" q.rate_sum rate e.bit_occurrences prop; *)
  let fcount = prop *. work in

  q.rate_sum <- q.rate_sum -. float_of_int e.ntests /. float_of_int e.bit_occurrences;
  q.rarity_count.(e.rarest_bit) <- q.rarity_count.(e.rarest_bit) - 1;
  if q.rarity_count.(e.rarest_bit) = 0 then
    q.num_rare_bits <- q.num_rare_bits - 1;

  (* validate_qcorpus q; *)
  e, fcount


let qcycle acc q gen =

  let interest e =
    (* let work = (float_of_int acc.nbits *. 1.5 +. 1.) in *)
    let work = 1000. in
    
    let sharing = float_of_int (q.rarity_count.(e.rarest_bit)) in (* FIXME: keep old value? harmonic sum of shards here? *)
    (* FIXME: q.counts.(e.rarest_bit) ? *)
    (* let rate = q.rate_sum /. float_of_int acc.counts.(e.rarest_bit) in  *)
    let lambda = float_of_int e.ntests /. float_of_int e.bit_occurrences in
    let rate = (1./. (q.rate_sum +. lambda)) *. lambda in

    let prop = rate /. (1. +. sharing) in
    (* Printf.printf "%f %f %d %f\n%!" q.rate_sum rate e.bit_occurrences prop; *)
    prop *. work in
    

  let run_case s =
    let res, entry = run acc s in
    if interest entry > 2. then
      qcorpus_add q entry;
    match res with
    | Ok ()
    | Error (Gen.Bad_test _) -> ()
    | Error e -> raise (Fail (s, e, Printexc.get_raw_backtrace ())) in
  
  if Queue.is_empty q.entries then
    run_case (mk_sample gen)
  else begin
    let e, _ = qcorpus_remove acc q (float_of_int acc.nbits *. 1.5 +. 1.) in
    let fcount = interest e in
    let count =
      let frac, integer = modf fcount in
      int_of_float integer + (if Random.float 1. < frac then 1 else 0) in
    Printf.printf "%04x(%2d) %5d/%6d ~ %5d/%6d - %.2f (%d)\n"
      e.rarest_bit q.rarity_count.(e.rarest_bit)
      e.bit_occurrences e.ntests
      acc.counts.(e.rarest_bit) acc.ntests
      fcount count;
    for i = 1 to 1 + count do
      run_case (mutate_sample e.sample)
    done;

    let about_to_forget =
      (* false && *)
      q.rarity_count.(e.rarest_bit) = 0 (* &&
      acc.counts.(e.rarest_bit) = e.bit_occurrences *) in
    (* if about_to_forget then Printf.printf "remembering %02x\n%!" e.rarest_bit; *)
    if about_to_forget || count > 1 (*|| Random.float 1. < fcount *) then
      qcorpus_add q (update_stats acc e)
  end


type psq_entry = {
  rarest_bit : int;
  occurrences : int;
  ntests : int;
  amount_fuzzed : int;

  samples : (unit -> unit) Gen.sample Queue.t;
}

let interest e =
  0. -. (float_of_int (e.occurrences + e.amount_fuzzed))

module Psq = Psq.Make
  (struct type t = int let compare = compare end)
  (struct type t = psq_entry let compare a b = compare (interest b) (interest a) end)

type psq_corpus = {
  mutable entries : Psq.t;
  mutable count : int;
  mutable total_interest : float;
}

let create_pqueue () = { entries = Psq.empty; count = 0; total_interest = 0. }

open Instrumentation
let psq_take acc q =
  match Psq.pop q.entries with
  | None ->
     None
  | Some ((bit, e), entries) -> begin
     q.total_interest <- q.total_interest -. interest e;
     q.count <- q.count - 1;
     assert (bit = e.rarest_bit);
     let s = Queue.pop e.samples in
     Printf.printf "%04x(%2d) %5d/%6d ~ %5d/%6d - %.2f > %.2f %d\n"
       e.rarest_bit (Queue.length e.samples)
       e.occurrences e.ntests
       acc.counts.(e.rarest_bit) acc.ntests
       (interest e) (match Psq.min entries with None -> 0. | Some (bit, e') -> interest e') e.amount_fuzzed;
     if Queue.is_empty e.samples then
       q.entries <- entries
     else begin
       let e = { e with ntests = acc.ntests; occurrences = acc.counts.(bit) } in
       q.total_interest <- q.total_interest +. interest e;
       q.entries <- Psq.add bit e entries;
     end;
     Some (bit, s)
   end

let rec shrink acc bit s iters =
  if iters = 0 then s else begin
    let s' = try Gen.shrink s with Gen.Bad_test _ -> s in
    if Gen.sample_len s' >= Gen.sample_len s then
      shrink acc bit s (iters - 1)
    else begin
      Instrumentation.with_instrumentation acc.ibuf (Gen.sample_val s') |> ignore;
      if Bytes.get (acc.ibuf :> bytes) bit <> '\000' then
        shrink acc bit s' (max iters 100)
      else
        shrink acc bit s (iters - 1)
    end
  end
  

let psq_offer acc q bit s =
  let e = Psq.find bit q.entries in
  let e_interest = 1./. (float_of_int acc.counts.(bit) /. float_of_int acc.ntests) in
  let num_samples = match e with None -> 0 | Some e -> Queue.length e.samples in
  let s_interest = e_interest /. (1. +. float_of_int num_samples) in
  if s_interest > 10. then begin
    let e = match e with
      | None -> { rarest_bit = bit;
                  occurrences = acc.counts.(bit);
                  ntests = acc.ntests;
                  samples = Queue.create ();
                  amount_fuzzed = 0 }
      | Some e -> 
         q.total_interest <- q.total_interest -. interest e;
         { e with ntests = acc.ntests; occurrences = acc.counts.(bit) } in
    (* FIXME: randomise *)
    let shrunk = s (*shrink acc bit s 10*) in
    if shrunk == s then
      Format.printf "no shrink@.%a@." Gen.pp_sample s
    else
      Format.printf "shrunk@.%a@.%a@." Gen.pp_sample s Gen.pp_sample shrunk;
    Queue.add shrunk e.samples;
    q.entries <- Psq.add bit e q.entries;
    q.total_interest <- q.total_interest +. interest e;
    q.count <- q.count + 1;
    true
  end else false

let psq_mark_fuzz q bit =
  match Psq.find bit q.entries with
  | None -> ()
  | Some e ->
     q.entries <- Psq.add bit { e with amount_fuzzed = e.amount_fuzzed + 1 } q.entries

let psq_validate q =
  let count = ref q.count in
  let total_interest = ref q.total_interest in
  q.entries |> Psq.iter (fun b e ->
    assert (not (Queue.is_empty e.samples));
    count := !count - Queue.length e.samples;
    total_interest := !total_interest -. interest e);
  assert (!count = 0)
  (* assert (abs_float !total_interest < 1e-3) *)

let run_exn acc s =
  let res = run acc (Gen.sample_val s) in
  (*  res.rarest_bit *)
  match res.result with
  | Ok ()
  | Error (Gen.Bad_test _) -> res.rarest_bit
  | Error e -> raise (Fail (s, e, Printexc.get_raw_backtrace ()))

let pqcycle acc q gen =
  psq_validate q;
  match psq_take acc q with
  | None ->
     Printf.printf "empty queue\n%!";
     let s = mk_sample gen in
     let bit = run_exn acc s in
     psq_offer acc q bit s |> ignore
  | Some (bit, s) ->
     (* if Random.int 1 = 0 then Format.printf "%a@." Gen.pp_sample s; *)
     let s' = mutate_sample s in
     Format.printf "%a@." Gen.pp_sample s';
     let bit' = run_exn acc s' in
     let bit_still_present =
       Bytes.get (acc.ibuf :> bytes) bit <> '\000' in
     let accepted = psq_offer acc q bit' s' in
     if not accepted || not bit_still_present || Gen.sample_len s <= Gen.sample_len s' then begin
       let _ = psq_offer acc q bit s in
       psq_mark_fuzz q bit
     end else begin
       Format.printf "dropping for new test %b %b:@.%a@.%a@.%!" accepted bit_still_present Gen.pp_sample s Gen.pp_sample s';
     end





let pqsplice acc q gen =
  let pool = Gen.Fragment_Pool.create () in
  let max = ref 100 in
  let vec = Vec.create () in
  let shortest q =
    Queue.peek q in
(*    Queue.fold (fun s s' -> if Gen.sample_len s' < Gen.sample_len s then s' else s) (Queue.peek q) q in *) 
  let rec fill_pool q =
    decr max;
    if !max = 0 then () else
    match Psq.pop q with
    | Some ((bit, s), q) ->
       let s = shortest s.samples in
       Vec.add vec s;
       Gen.split_into pool s;
       fill_pool q
    | None -> () in
  fill_pool q.entries;
  Vec.to_array vec |> Array.iter (fun s ->
    for i = 1 to 10 do
      let s' = Gen.splice pool s in
      let bit' = run_exn acc s' in
      if psq_offer acc q bit' s' then begin
        Format.printf "splice: @.%a@.%a@." Gen.pp_sample s Gen.pp_sample s';
      end
    done)
