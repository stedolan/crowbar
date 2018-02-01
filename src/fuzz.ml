let mkbuf () =
  let buf = Bytes.make 500 '\000' in
  for i = 0 to Bytes.length buf - 1 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xff));
  done;
  Bytebuf.of_bytes buf


open Gen


type test = Test : string * ('f, unit) gens * 'f -> test

type test_status =
  | TestPass of int
  | GenFail of exn * Printexc.raw_backtrace
  | TestExn : 'a sample * exn * Printexc.raw_backtrace -> test_status
  | TestFail : 'a sample * unit Printers.printer -> test_status

exception Fail : 'a sample * exn * Printexc.raw_backtrace -> exn
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
  let rec mutate_sample s =
    try mutate s (Random.int (sample_len s)) (mkbuf ()) with
    | Bad_test _ -> mutate_sample s in

  match
    while not (Queue.is_empty q) || !ntests < 5000 do
      if Queue.is_empty q then Queue.add (0, mk_sample ()) q;
      let depth, tc = Queue.pop q in
      for i = 0 to 10 do
        let tc = mutate_sample tc in
        Instrumentation.with_instrumentation ibuf (fun () ->
            sample_val tc ())
        |> (function
            | Ok x -> incr ntests
            | Error (Bad_test _) -> ()
            | Error e -> raise (Fail (tc, e, Printexc.get_raw_backtrace ())));
        let count = Instrumentation.find_new_bits seen ibuf new_hits in
        Format.printf "%a %d %d %d\n%!" pp_sample tc depth count (Queue.length q);
        if count > 0 then Queue.add (depth + 1, tc) q
      done
    done
  with
  | () ->
     TestPass !ntests
  | exception Fail(s, Std_generators.Failed_test p, _) ->
     TestFail (s, p)
  | exception Fail(s, e, bt) ->
     TestExn (s, e, bt)
  | exception e ->
     GenFail (e, Printexc.get_raw_backtrace ())
