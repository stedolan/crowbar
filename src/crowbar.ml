(* Fix for OCaml 5.0 *)
let () = Random.init 42

type src = Random of Random.State.t | Fd of Unix.file_descr
type state =
  {
    chan : src;
    buf : Bytes.t;
    mutable offset : int;
    mutable len : int
  }

type 'a printer = Format.formatter -> 'a -> unit

type 'a strat =
  | Choose of 'a gen list
  | Map : ('f, 'a) gens * 'f -> 'a strat
  | Bind : 'a gen * ('a -> 'b gen) -> 'b strat
  | Option : 'a gen -> 'a option strat
  | List : 'a gen -> 'a list strat
  | List1 : 'a gen -> 'a list strat
  | Array : 'a gen -> 'a array strat
  | Array1 : 'a gen -> 'a array strat
  | Unlazy of 'a gen Lazy.t
  | Primitive of (state -> 'a)
  | Print of 'a printer * 'a gen

and 'a gen =
  { strategy: 'a strat;
    small_examples: 'a list; }

and ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

let unlazy f = { strategy = Unlazy f; small_examples = [] }

let fix f =
  let rec lazygen = lazy (f (unlazy lazygen)) in
  Lazy.force lazygen

let map (type f) (type a) (gens : (f, a) gens) (f : f) =
  { strategy = Map (gens, f); small_examples = match gens with [] -> [f] | _ -> [] }

let dynamic_bind m f = {strategy = Bind(m, f); small_examples = [] }

let const x = map [] x
let choose gens = { strategy = Choose gens; small_examples = List.map (fun x -> x.small_examples) gens |> List.concat }
let option gen = { strategy = Option gen; small_examples = [None] }
let list gen = { strategy = List gen; small_examples = [[]] }
let list1 gen = { strategy = List1 gen; small_examples = List.map (fun x -> [x]) gen.small_examples }
let array gen = { strategy = Array gen; small_examples = [[||]] }
let array1 gen = { strategy = Array1 gen; small_examples = List.map (fun x -> [|x|]) gen.small_examples }
let primitive f ex = { strategy = Primitive f; small_examples = [ex] }

let pair gena genb =
  map (gena :: genb :: []) (fun a b -> (a, b))

let concat_gen_list sep l =
  match l with
  | h::t -> List.fold_left (fun acc e ->
      map [acc; sep; e] (fun acc sep e -> acc ^ sep ^ e)
    ) h t
  | [] -> const ""

let with_printer pp gen = {strategy = Print (pp, gen); small_examples = gen.small_examples }

let result gena genb =
  choose [
    map [gena] (fun va -> Ok va);
    map [genb] (fun vb -> Error vb);
  ]


let pp = Format.fprintf
let pp_int ppf n = pp ppf "%d" n
let pp_int32 ppf n = pp ppf "%s" (Int32.to_string n)
let pp_int64 ppf n = pp ppf "%s" (Int64.to_string n)
let pp_float ppf f = pp ppf "%f" f
let pp_bool ppf b = pp ppf "%b" b
let pp_char ppf c = pp ppf "%c" c
let pp_uchar ppf c = pp ppf "U+%04x" (Uchar.to_int c)
let pp_string ppf s = pp ppf "%S" s
(* taken from OCaml stdlib *)
let pp_print_iter ~pp_sep iter pp_v ppf v =
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_v ppf v
  in
  iter pp_v v
let pp_list pv ppf l =
  pp ppf "@[<hv 1>[%a]@]"
     (pp_print_iter ~pp_sep:(fun ppf () -> pp ppf ";@ ") List.iter pv) l
let pp_array pv ppf a =
  pp ppf "@[<hv 1>[|%a|]@]"
  (pp_print_iter ~pp_sep:(fun ppf () -> pp ppf ";@ ") Array.iter pv) a
let pp_option pv ppf = function
  | None ->
      Format.fprintf ppf "None"
  | Some x ->
      Format.fprintf ppf "(Some %a)" pv x

exception BadTest of string
exception FailedTest of unit printer
let guard = function
  | true -> ()
  | false -> raise (BadTest "guard failed")
let bad_test () = raise (BadTest "bad test")
let nonetheless = function
  | None -> bad_test ()
  | Some a -> a

let get_data chan buf off len =
  match chan with
  | Random rand ->
     for i = off to off + len - 1 do
       Bytes.set buf i (Char.chr (Random.State.bits rand land 0xff))
     done;
     len - off
  | Fd ch ->
     Unix.read ch buf off len

let refill src =
  assert (src.offset <= src.len);
  let remaining = src.len - src.offset in
  (* move remaining data to start of buffer *)
  Bytes.blit src.buf src.offset src.buf 0 remaining;
  src.len <- remaining;
  src.offset <- 0;
  let read = get_data src.chan src.buf remaining (Bytes.length src.buf - remaining) in
  if read = 0 then
    raise (BadTest "premature end of file")
  else
    src.len <- remaining + read

let rec getbytes src n =
  assert (src.offset <= src.len);
  if n > Bytes.length src.buf then failwith "request too big";
  if src.len - src.offset >= n then
    let off = src.offset in
    (src.offset <- src.offset + n; off)
  else
    (refill src; getbytes src n)

let read_char src =
  let off = getbytes src 1 in
  Bytes.get src.buf off

let read_byte src =
  Char.code (read_char src)

let read_bool src =
  let n = read_byte src in
  n land 1 = 1

let bool = with_printer pp_bool (primitive read_bool false)

let uint8 = with_printer pp_int (primitive read_byte 0)
let int8 = with_printer pp_int (map [uint8] (fun n -> n - 128))

let read_uint16 src =
  let off = getbytes src 2 in
  Bytes.get_uint16_le src.buf off

let read_int16 src =
  let off = getbytes src 2 in
  Bytes.get_int16_le src.buf off

let uint16 = with_printer pp_int (primitive read_uint16 0)
let int16 = with_printer pp_int (primitive read_int16 0)

let read_int32 src =
  let off = getbytes src 4 in
  Bytes.get_int32_le src.buf off

let read_int64 src =
  let off = getbytes src 8 in
  Bytes.get_int64_le src.buf off

let int32 = with_printer pp_int32 (primitive read_int32 0l)
let int64 = with_printer pp_int64 (primitive read_int64 0L)

let int =
  with_printer pp_int
    (if Sys.word_size <= 32 then
      map [int32] Int32.to_int
    else
      map [int64] Int64.to_int)

let float = with_printer pp_float (primitive (fun src ->
  let off = getbytes src 8 in
  let i64 = Bytes.get_int64_le src.buf off in
  Int64.float_of_bits i64) 0.)

let char = with_printer pp_char (primitive read_char 'a')

(* maybe print as a hexdump? *)
let bytes = with_printer pp_string (primitive (fun src ->
  (* null-terminated, with '\001' as an escape code *)
  let buf = Bytes.make 64 '\255' in
  let rec read_bytes p =
    if p >= Bytes.length buf then p else
    match read_char src with
    | '\000' -> p
    | '\001' ->
       Bytes.set buf p (read_char src);
       read_bytes (p + 1)
    | c ->
       Bytes.set buf p c;
       read_bytes (p + 1) in
  let count = read_bytes 0 in
  Bytes.sub_string buf 0 count) "")

let bytes_fixed n = with_printer pp_string (primitive (fun src ->
  let off = getbytes src n in
  Bytes.sub_string src.buf off n) (String.make n 'a'))

let choose_int n state =
  assert (n > 0);
  if n = 1 then
    0
  else if (n <= 0x100) then
    read_byte state mod n
  else if (n < 0x1000000) then
    Int32.(to_int (abs (rem (read_int32 state) (of_int n))))
  else
    Int64.(to_int (abs (rem (read_int64 state) (of_int n))))

let range ?(min=0) n =
  if n <= 0 then
    raise (Invalid_argument "Crowbar.range: argument n must be positive");
  if min < 0 then
    raise (Invalid_argument "Crowbar.range: argument min must be positive or null");
  with_printer pp_int (primitive (fun s -> min + choose_int n s) min)

let uchar : Uchar.t gen =
  map [range 0x110000] (fun x ->
    guard (Uchar.is_valid x); Uchar.of_int x)
let uchar = with_printer pp_uchar uchar

let rec sequence = function
  g::gs -> map [g; sequence gs] (fun x xs -> x::xs)
| [] -> const []

let shuffle_arr arr =
  let n = Array.length arr in
  let gs = List.init n (fun i -> range ~min:i (n - i)) in
  map [sequence gs] @@ fun js ->
    js |> List.iteri (fun i j ->
      let t = arr.(i) in arr.(i) <- arr.(j); arr.(j) <- t);
    arr

let shuffle l = map [shuffle_arr (Array.of_list l)] Array.to_list

exception GenFailed of exn * Printexc.raw_backtrace * unit printer

let rec generate : type a . int -> state -> a gen -> a * unit printer =
  fun size input gen ->
  if size <= 1 && gen.small_examples <> [] then List.hd gen.small_examples, fun ppf () -> pp ppf "?" else
  match gen.strategy with
  | Choose gens ->
     (* FIXME: better distribution? *)
     (* FIXME: choices of size > 255? *)
     let n = choose_int (List.length gens) input in
     let v, pv = generate size input (List.nth gens n) in
     v, fun ppf () -> pp ppf "#%d %a" n pv ()
  | Map ([], k) ->
     k, fun ppf () -> pp ppf "?"
  | Map (gens, f) ->
     let rec len : type k res . int -> (k, res) gens -> int =
       fun acc xs -> match xs with
       | [] -> acc
       | _ :: xs -> len (1 + acc) xs in
     let n = len 0 gens in
     (* the size parameter is (apparently?) meant to ensure that generation
        eventually terminates, by limiting the set of options from which the
        generator might choose once we've gotten deep into a tree.  make sure we
        always mark our passing, even when we've mapped one value into another,
        so we don't blow the stack. *)
     let size = (size - 1) / n in
     let v, pvs = gen_apply size input gens f in
     begin match v with
       | Ok v -> v, pvs
       | Error (e, bt) -> raise (GenFailed (e, bt, pvs))
     end
  | Bind (m, f) ->
     let index, pv_index = generate (size - 1) input m in
     let a, pv = generate (size - 1) input (f index) in
     a, (fun ppf () -> pp ppf "(%a) => %a" pv_index () pv ())
  | Option gen ->
     if size < 1 then
       None, fun ppf () -> pp ppf "None"
     else if read_bool input then
       let v, pv = generate size input gen in
       Some v, fun ppf () -> pp ppf "Some (%a)" pv ()
     else
       None, fun ppf () -> pp ppf "None"
  | List gen ->
     let elems = generate_list size input gen in
     List.map fst elems,
       fun ppf () -> pp_list (fun ppf (_, pv) -> pv ppf ()) ppf elems
  | List1 gen ->
     let elems = generate_list1 size input gen in
     List.map fst elems,
       fun ppf () -> pp_list (fun ppf (_, pv) -> pv ppf ()) ppf elems
  | Array gen ->
    let elems = generate_list size input gen in
    let elems = Array.of_list elems in
    Array.map fst elems, fun ppf () -> pp_array (fun ppf (_, pv) -> pv ppf ()) ppf elems
  | Array1 gen ->
    let elems = generate_list1 size input gen in
    let elems = Array.of_list elems in
    Array.map fst elems, fun ppf () -> pp_array (fun ppf (_, pv) -> pv ppf ()) ppf elems
  | Primitive gen ->
     gen input, fun ppf () -> pp ppf "?"
  | Unlazy gen ->
     generate size input (Lazy.force gen)
  | Print (ppv, gen) ->
     let v, _ = generate size input gen in
     v, fun ppf () -> ppv ppf v

and generate_list : type a . int -> state -> a gen -> (a * unit printer) list =
  fun size input gen ->
  if size <= 1 then []
  else if read_bool input then
    generate_list1 size input gen
  else
    []

and generate_list1 : type a . int -> state -> a gen -> (a * unit printer) list =
  fun size input gen ->
  let ans = generate (size/2) input gen in
  ans :: generate_list (size/2) input gen

and gen_apply :
    type k res . int -> state ->
       (k, res) gens -> k ->
       (res, exn * Printexc.raw_backtrace) result * unit printer =
  fun size state gens f ->
  let rec go :
    type k res . int -> state ->
       (k, res) gens -> k ->
       (res, exn * Printexc.raw_backtrace) result * unit printer list =
      fun size input gens -> match gens with
      | [] -> fun x -> Ok x, []
      | g :: gs -> fun f ->
        let v, pv = generate size input g in
        let res, pvs =
          match f v with
          | exception (BadTest _ as e) -> raise e
          | exception e ->
             Error (e, Printexc.get_raw_backtrace ()) , []
          | fv -> go size input gs fv in
        res, pv :: pvs in
  let v, pvs = go size state gens f in
  let pvs = fun ppf () ->
    match pvs with
    | [pv] ->
       pv ppf ()
    | pvs ->
       pp_list (fun ppf pv -> pv ppf ()) ppf pvs in
  v, pvs


let fail s = raise (FailedTest (fun ppf () -> pp ppf "%s" s))

let failf format =
  Format.kasprintf fail format

let check = function
  | true -> ()
  | false -> raise (FailedTest (fun ppf () -> pp ppf "check false"))

let check_eq ?pp:pv ?cmp ?eq a b =
  let pass = match eq, cmp with
    | Some eq, _ -> eq a b
    | None, Some cmp -> cmp a b = 0
    | None, None ->
       Stdlib.compare a b = 0 in
  if pass then
    ()
  else
    raise (FailedTest (fun ppf () ->
      match pv with
      | None -> pp ppf "different"
      | Some pv -> pp ppf "@[<hv>%a@ !=@ %a@]" pv a pv b))

let () = Printexc.record_backtrace true

type test = Test : string * ('f, unit) gens * 'f -> test

type test_status =
  | TestPass of unit printer
  | BadInput of string
  | GenFail of exn * Printexc.raw_backtrace * unit printer
  | TestExn of exn * Printexc.raw_backtrace * unit printer
  | TestFail of unit printer * unit printer

let run_once (gens : (_, unit) gens) f state =
  match gen_apply 100 state gens f with
  | Ok (), pvs -> TestPass pvs
  | Error (FailedTest p, _), pvs -> TestFail (p, pvs)
  | Error (e, bt), pvs -> TestExn (e, bt, pvs)
  | exception (BadTest s) -> BadInput s
  | exception (GenFailed (e, bt, pvs)) -> GenFail (e, bt, pvs)

let classify_status = function
  | TestPass _ -> `Pass
  | BadInput _ -> `Bad
  | GenFail _ -> `Fail (* slightly dubious... *)
  | TestExn _ | TestFail _ -> `Fail

let print_status ppf status =
  let print_ex ppf (e, bt) =
    pp ppf "%s" (Printexc.to_string e);
    bt
    |> Printexc.raw_backtrace_to_string
    |> Str.split (Str.regexp "\n")
    |> List.iter (pp ppf "@,%s") in
  match status with
  | TestPass pvs ->
     pp ppf "When given the input:@.@[<v 4>@,%a@,@]@.the test passed."
        pvs ()
  | BadInput s ->
     pp ppf "The testcase was invalid:@.%s" s
  | GenFail (e, bt, pvs) ->
     pp ppf "When given the input:@.@[<4>%a@]@.the testcase generator threw an exception:@.@[<v 4>@,%a@,@]"
        pvs ()
        print_ex (e, bt)
  | TestExn (e, bt, pvs) ->
     pp ppf "When given the input:@.@[<v 4>@,%a@,@]@.the test threw an exception:@.@[<v 4>@,%a@,@]"
        pvs ()
        print_ex (e, bt)
  | TestFail (err, pvs) ->
     pp ppf "When given the input:@.@[<v 4>@,%a@,@]@.the test failed:@.@[<v 4>@,%a@,@]"
        pvs ()
        err ()

let prng_state_of_seed seed =
  (* try to make this independent of word size *)
  let seed = Int64.( [|
       to_int (logand (of_int 0xffff) seed);
       to_int (logand (of_int 0xffff) (shift_right seed 16));
       to_int (logand (of_int 0xffff) (shift_right seed 32));
       to_int (logand (of_int 0xffff) (shift_right seed 48)) |]) in
  Random.State.make seed
let src_of_seed seed =
  Random (prng_state_of_seed seed)

let run_test ~mode ~silent ?(verbose=false) (Test (name, gens, f)) =
  let show_status_line ?(clear=false) stat =
    Printf.printf "%s: %s\n" name stat;
    if clear then print_newline ();
    flush stdout in
  let ppf = Format.std_formatter in
  if not silent && Unix.isatty Unix.stdout then
    show_status_line ~clear:false "....";
  let status = match mode with
  | `Once state ->
     run_once gens f state
  | `Repeat (iters, seedseed) ->
     let worst_status = ref (TestPass (fun _ () -> ())) in
     let npass = ref 0 in
     let nbad = ref 0 in
     let seedsrc = prng_state_of_seed seedseed in
     while !npass < iters && classify_status !worst_status = `Pass do
       let seed = Random.State.int64 seedsrc Int64.max_int in
       let state = { chan = src_of_seed seed;
                     buf = Bytes.make 256 '0';
                     offset = 0; len = 0 } in
       let status = run_once gens f state in
       begin match classify_status status with
       | `Pass -> incr npass
       | `Bad -> incr nbad
       | `Fail ->
          worst_status := status
       end;
     done;
     let status = !worst_status in
     status in
  if silent && verbose && classify_status status = `Fail then begin
         show_status_line
           ~clear:true "FAIL";
         pp ppf "%a@." print_status status;
  end;
  if not silent then begin
      match classify_status status with
      | `Pass ->
         show_status_line
           ~clear:true "PASS";
         if verbose then pp ppf "%a@." print_status status
      | `Fail ->
         show_status_line
           ~clear:true "FAIL";
         pp ppf "%a@." print_status status;
      | `Bad ->
         show_status_line
           ~clear:true "BAD";
         pp ppf "%a@." print_status status;
    end;
  status

exception TestFailure
let run_all_tests seed repeat file verbosity infinity tests =
  match file with
  | None ->
    let seed = match seed with
      | Some seed -> seed
      | None -> Random.int64 (Int64.max_int)
    in
    if infinity then
      (* infinite QuickCheck mode *)
      let rec go ntests alltests tests = match tests with
        | [] ->
           go ntests alltests alltests
        | t :: rest ->
           if ntests mod 10000 = 0 then Printf.eprintf "\r%d%!" ntests;
           let chan = src_of_seed seed in
           let state = { chan ; buf = Bytes.make 256 '0'; offset = 0; len = 0 } in
           match classify_status (run_test ~mode:(`Once state) ~silent:true ~verbose:true t) with
           | `Fail -> Printf.printf "%d tests passed before first failure\n%!" ntests
           | _ -> go (ntests + 1) alltests rest in
      let () = go 0 tests tests in
      1
    else
      (* limited-run QuickCheck mode *)
      let failures = ref 0 in
      let () = tests |> List.iter (fun t ->
          match (run_test ~mode:(`Repeat (repeat, seed)) ~silent:false t |> classify_status) with
          | `Fail -> failures := !failures + 1
          | _ -> ()
        )
      in
      !failures
  | Some file ->
    (* AFL mode *)
    let verbose = List.length verbosity > 0 in
    let () = AflPersistent.run (fun () ->
         let fd = Unix.openfile file [Unix.O_RDONLY] 0o000 in
         let state = { chan = Fd fd; buf = Bytes.make 256 '0';
                       offset = 0; len = 0 } in
         let status =
           try run_test ~mode:(`Once state) ~silent:false ~verbose @@
             List.nth tests (choose_int (List.length tests) state)
           with
           BadTest s -> BadInput s
         in
         Unix.close fd;
         match classify_status status with
         | `Pass | `Bad -> ()
         | `Fail ->
            Printexc.record_backtrace false;
            raise TestFailure)
    in
    0 (* failures come via the exception mechanism above *)

let last_generated_name = ref 0
let generate_name () =
  incr last_generated_name;
  "test" ^ string_of_int !last_generated_name

let registered_tests = ref []

let add_test ?name gens f =
  let name = match name with
    | None -> generate_name ()
    | Some name -> name in
  registered_tests := Test (name, gens, f) :: !registered_tests

(* cmdliner stuff *)

let randomness_file =
  let doc = "A file containing some bytes, consulted in constructing test cases.  \
    When `afl-fuzz` is calling the test binary, use `@@` to indicate that \
    `afl-fuzz` should put its test case here \
    (e.g. `afl-fuzz -i input -o output ./my_crowbar_test @@`).  Re-run a test by \
    supplying the test file here \
    (e.g. `./my_crowbar_test output/crashes/id:000000`).  If no file is \
    specified, the test will use OCaml's Random module as a source of \
    randomness for a predefined number of rounds." in
  Cmdliner.Arg.(value & pos 0 (some file) None & info [] ~doc ~docv:"FILE")

let seed =
  let doc = "The seed (an int64) for the PRNG. Use as an alternative to FILE
    when running in non-AFL (quickcheck) mode." in
  Cmdliner.Arg.(value & opt (some int64) None & info ["s"; "seed"] ~doc ~docv:"SEED")

let repeat =
  let doc = "The number of times to repeat the test in quick-check." in
  Cmdliner.Arg.(value & opt int 5000 & info ["r"; "repeat"] ~doc ~docv:"REPEAT")

let verbosity =
  let doc = "Print information on each test as it's conducted." in
  Cmdliner.Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docv:"VERBOSE")

let infinity =
  let doc = "In non-AFL (quickcheck) mode, continue running until a test failure is \
             discovered.  No attempt is made to track which tests have already been run, \
             so some tests may be repeated, and if there are no failures reachable, the \
             test will never terminate without outside intervention." in
  Cmdliner.Arg.(value & flag & info ["i"] ~doc ~docv:"INFINITE")

let crowbar_info = Cmdliner.Cmd.info @@ Filename.basename Sys.argv.(0)

let () =
  at_exit (fun () ->
      let t = !registered_tests in
      registered_tests := [];
      match t with
      | [] -> ()
      | t ->
        let cmd = Cmdliner.Term.(const run_all_tests $ seed $ repeat $ randomness_file $ verbosity $
                                 infinity $ const (List.rev t)) in
        exit @@ Cmdliner.Cmd.eval' ~catch:false (Cmdliner.Cmd.v crowbar_info cmd)
    )

module Syntax = struct
  let ( let* ) = dynamic_bind
  let ( let+ ) gen map_fn = map [ gen ] map_fn
  let ( and+ ) = pair
end
