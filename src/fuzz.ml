let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
  done;
  Bytebuf.of_bytes buf


open Gen


let count_bits (ib : Instrumentation.buf) =
  let c = ref 0 in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Bytes.unsafe_get (ib :> bytes) i <> '\000' then incr c
  done;
  !c

type test = Test : string * ('f, unit) gens * 'f -> test

type test_status =
  | TestPass of int
  | GenFail of int * exn * Printexc.raw_backtrace
  | TestExn : int * 'a sample * exn * Printexc.raw_backtrace -> test_status
  | TestFail : int * 'a sample * unit Printers.printer -> test_status

let rec mutate_sample tbl s =
(*  if Random.int 100 < 5 then
    (Gen.Fragment_Pool.add tbl s; Gen.splice tbl s)
  else *)
  match mutate s (Random.int (sample_len s)) (mkbuf ()) with
  | exception Bad_test _ -> mutate_sample tbl s
  | s' ->
     if Random.int 100 < 10 then s' else mutate_sample tbl s'

exception Fail = Corpus.Fail
let fuzz (Test (name, gens, run)) =
  let gens = delay gens run in
  let () = (* Random.init 123 *) Random.self_init () in
  let ibuf = Instrumentation.create_buffer () in
  let seen = Instrumentation.create_buffer () in
  let new_hits = Array.make (Bytes.length (seen :> bytes)) 0 in
  let q = Queue.create () in
  let ntests = ref 0 in
  let rec mk_sample () =
    try sample gens (mkbuf ()) with
    | Bad_test _ -> mk_sample () in

  let pool = Fragment_Pool.create () in

  match
    while true do  (* not (Queue.is_empty q) || !ntests < 5000 do *)
      if Queue.is_empty q then Queue.add (0, mk_sample ()) q;
      let depth, tc = Queue.pop q in
      for i = 0 to 3 do
        let tc = mutate_sample pool tc in
        Instrumentation.with_instrumentation ibuf (fun () ->
            sample_val tc ())
        |> (function
            | Ok x -> incr ntests
            | Error (Bad_test _) -> incr ntests
            | Error e -> raise (Fail (tc, e, Printexc.get_raw_backtrace ())));
        let count = Instrumentation.find_new_bits seen ibuf new_hits in
        if count > 0 then Format.printf "% 4d % 4d@." !ntests (count_bits seen);
        let nbits = ref 0 in Bytes.iter (function '\000' -> () | _ -> incr nbits) (ibuf :> bytes);
        (* Format.printf "% 4d %a %d %d %d\n%!" !nbits pp_sample tc depth count (Queue.length q); *)
        if count > 0 then Queue.add (depth + 1, tc) q
      done
    done
  with
  | () ->
     TestPass !ntests
  | exception Fail(s, e, bt) ->
     TestExn (!ntests, s, e, bt)
  | exception e ->
     GenFail (!ntests, e, Printexc.get_raw_backtrace ())





let accum_instrumentation counts (ibuf : Instrumentation.buf) =
  assert (Array.length counts = Instrumentation.buffer_size);
  assert (Bytes.length (ibuf :> bytes) = Instrumentation.buffer_size);
  for i = 0 to Instrumentation.buffer_size - 1 do
    Array.unsafe_set counts i (Array.unsafe_get counts i + Char.code (Bytes.unsafe_get (ibuf :> bytes) i))
  done

let compute_interest counts (ibuf : Instrumentation.buf) =
  assert (Array.length counts = Instrumentation.buffer_size);
  assert (Bytes.length (ibuf :> bytes) = Instrumentation.buffer_size);
  let r = ref 0. in
  for i = 0 to Instrumentation.buffer_size - 1 do
    let k = Char.code (Bytes.unsafe_get (ibuf :> bytes) i) in
    if k <> 0 then begin
      let rr = float_of_int k /. float_of_int (Array.unsafe_get counts i) in
      if rr > !r then r := rr
    end
  done;
  !r

let discard = Array.make Instrumentation.buffer_size 0
  
let rec loop q seen counts curr curr_ibuf next_ibuf ntests : unit =
  let rec find_next () = mutate_sample q curr in
  let next = find_next () in
  let res = Instrumentation.with_instrumentation next_ibuf (sample_val next) in
  match res with 
  | Error (Bad_test _) -> Instrumentation.find_new_bits seen next_ibuf discard |> ignore; loop q seen counts curr curr_ibuf next_ibuf ntests
  | Error e -> raise (Fail (next, e, Printexc.get_raw_backtrace ()))
(*  | Ok () when !ntests > 200000 -> *)
  | Ok () ->
     incr ntests;
     accum_instrumentation counts next_ibuf;
     let new_count = Instrumentation.find_new_bits seen next_ibuf discard in
     if new_count > 0 then Format.printf "% 4d % 4d@." !ntests (count_bits seen);
     let int_curr = compute_interest counts curr_ibuf /. float_of_int (1 + sample_len curr) in
     let int_next = compute_interest counts next_ibuf /. float_of_int (1 + sample_len next) in
     (* Format.printf "%5.3f %5.3f %2.1f %d@." int_next int_curr (int_next /. int_curr) (sample_len next); *)
     if Random.int 1000 = 0 then
       Format.printf "% 4d %.2f %a\n%!" !ntests int_next pp_sample next;
     if
       (* always switch to a more interesting testcase *)
       int_next > int_curr
       ||
       (* sometimes switch to a less interesting testcase *)
       Random.float 1.0 <= 0.2 *. int_next /. int_curr
     then begin
       (* if int_next > int_curr && Random.int 100 > Queue.length q then Queue.push (next, Instrumentation.copy_buffer next_ibuf) q; *)
       if Random.int 100 < 1 then Fragment_Pool.add q next;
       loop q seen counts next next_ibuf curr_ibuf ntests
     end else
       loop q seen counts curr curr_ibuf next_ibuf ntests

let mcmc_fuzz (Test (name, gens, run)) =
  Random.self_init ();
  let counts = Array.make Instrumentation.buffer_size 0 in
  let curr_ibuf = Instrumentation.create_buffer () in
  let next_ibuf = Instrumentation.create_buffer () in
  let gens = delay gens run in
  let rec mk_sample () =
    try sample gens (mkbuf ()) with
    | Bad_test _ -> mk_sample () in
  let curr = mk_sample () in
  let ntests = ref 0 in
  match
    loop (Fragment_Pool.create ()) (Instrumentation.create_buffer ()) counts curr curr_ibuf next_ibuf ntests
  with
  | () ->
     TestPass !ntests
  | exception Fail(s, Std_generators.Failed_test p, _) ->
     TestFail (!ntests, s, p)
  | exception Fail(s, e, bt) ->
     TestExn (!ntests, s, e, bt)
  | exception e ->
     GenFail (!ntests, e, Printexc.get_raw_backtrace ())





let corpus_fuzz (Test (name, gens, run)) =
  Random.self_init ();
  let gens = delay gens run in
  let counts = Corpus.new_counts () in
  let corpus = ref Corpus.{ generator = gens; entries = [| |]; best_sample = Array.make Instrumentation.buffer_size (-1); nbits = 0; rarity_count = Array.make Instrumentation.buffer_size 0 } in
  let ntests = ref 0 in
  match

  while true do
    let oldbits = !corpus.nbits in
    Printf.printf "%d %d %d\n%!" !ntests (Array.length !corpus.entries) !corpus.nbits;
    corpus := Corpus.cycle ntests counts !corpus;
    (* if oldbits = !corpus.nbits then threshold := (!threshold * 5 + 2) / 3 *)
  done
  with
  | () ->
     TestPass !ntests
  | exception Fail(s, Std_generators.Failed_test p, _) ->
     TestFail (!ntests, s, p)
  | exception Fail(s, e, bt) ->
     TestExn (!ntests, s, e, bt)
  | exception e ->
     GenFail (!ntests, e, Printexc.get_raw_backtrace ())

let qcorpus_fuzz (Test (name, gens, run)) =
  Random.self_init ();
  let gens = delay gens run in
  let acc = Corpus.create_accumulator () in
  let q = Corpus.create_queue () in
  match
  while true do
    Corpus.qcycle acc q gens;
    if Random.int 100 = 0 then
      Printf.printf "%d %d %d (forgot %d)\n%!" acc.total_tests (Queue.length q.entries) acc.nbits (acc.nbits - Corpus.qnbits q);
  done
  with
  | () ->
     TestPass acc.total_tests
  | exception Fail(s, Std_generators.Failed_test p, _) ->
     TestFail (acc.total_tests, s, p)
  | exception Fail(s, e, bt) ->
     TestExn (acc.total_tests, s, e, bt)
  | exception e ->
     GenFail (acc.total_tests, e, Printexc.get_raw_backtrace ())

let pqcorpus_fuzz (Test (name, gens, run)) =
  Random.self_init ();
  let gens = delay gens run in
  let acc = Corpus.create_accumulator () in
  let q = Corpus.create_pqueue () in
  match
  while true do
    Corpus.pqcycle acc q gens;
      Printf.printf "%d %d %d\n%!" acc.total_tests q.count acc.nbits;
  done
  with
  | () ->
     TestPass acc.total_tests
  | exception Fail(s, Std_generators.Failed_test p, _) ->
     TestFail (acc.total_tests, s, p)
  | exception Fail(s, e, bt) ->
     TestExn (acc.total_tests, s, e, bt)
  | exception e ->
     GenFail (acc.total_tests, e, Printexc.get_raw_backtrace ())



let quickcheck (Test (name, gens, run)) =
  Random.self_init ();
  let gens = delay gens run in
  let ibuf = Instrumentation.create_buffer () in
  let ntests = ref 0 in
  let nbits = ref 0 in
  let rec go () =
    let s = Corpus.mk_sample gens in
    match Instrumentation.with_instrumentation ibuf (sample_val s) with
    | Error (Bad_test _) -> go ()
    | Error _ (*(Std_generators.Failed_test p) ->
       TestFail (!ntests, s, p)
    | Error (e) ->
       TestExn (!ntests, s, e, Printexc.get_raw_backtrace ())*)
    | Ok () ->
       incr ntests;
       let new_count = count_bits ibuf - !nbits in
       if new_count > 0 then begin
         nbits := !nbits + new_count;
         Printf.printf "%5d %4d\n%!" !ntests !nbits
       end;
       go () in
  go ()




(* for computing stats on instrumentation bits *)

let accum_instrumentation counts (ibuf : Instrumentation.buf) =
  assert (Array.length counts = Instrumentation.buffer_size);
  assert (Bytes.length (ibuf :> bytes) = Instrumentation.buffer_size);
  for i = 0 to Instrumentation.buffer_size - 1 do
    let c = Bytes.unsafe_get (ibuf :> bytes) i in
    if c <> '\000' then
      Array.unsafe_set counts i (Array.unsafe_get counts i + 1)
(*    Array.unsafe_set counts i (Array.unsafe_get counts i + Char.code (Bytes.unsafe_get (ibuf :> bytes) i)) *)
  done

let most_interesting_bit counts (ibuf : Instrumentation.buf) =
  let best = ref max_int and r = ref 0 in
  for i = 0 to Instrumentation.buffer_size - 1 do
    if Bytes.unsafe_get (ibuf :> bytes) i <> '\000' then begin
      let b = Array.unsafe_get counts i in
      if b < !best then (best := b; r := i)
    end
  done;
  !r

let compute_interest counts (ibuf : Instrumentation.buf) =
  assert (Array.length counts = Instrumentation.buffer_size);
  assert (Bytes.length (ibuf :> bytes) = Instrumentation.buffer_size);
  let b = most_interesting_bit counts ibuf in
  if b <> 0 then 1. /. float_of_int b else 0.
(*
  let r = ref 0. in
  for i = 0 to Instrumentation.buffer_size - 1 do
    let k = Char.code (Bytes.unsafe_get (ibuf :> bytes) i) in
    if k <> 0 then begin
      let rr = float_of_int k /. float_of_int (Array.unsafe_get counts i) in
      if rr > !r then r := rr
    end
  done;
  !r
*)
 

let compute_freqs (Test (name, gens, run)) =
  let counts = Array.make Instrumentation.buffer_size 0 in
  let ibuf = Instrumentation.create_buffer () in
  let gens = delay gens run in
  let t = ref 0 in
  for i = 0 to 100_000 do
    match sample gens (mkbuf ()) with
    | exception (Bad_test _) -> ()
    | s ->
       incr t;
       Instrumentation.with_instrumentation ibuf (sample_val s) |> ignore;
       accum_instrumentation counts ibuf
  done;
  for i = 0 to Instrumentation.buffer_size - 1 do
    Printf.printf "%d\n" counts.(i)
  done;
  Printf.printf " %d\n%!" !t;
  ()
