type src = Random of Random.State.t | Chan of in_channel
type state =
  {
    chan : src;
    buf : Bytes.t;
    mutable offset : int;
    mutable len : int
  }

type 'a printer = Format.formatter -> 'a -> unit


type 'a gen =
  | Const of 'a
  | Choose of 'a gen list
  | Map : ('f, 'a) gens * 'f -> 'a gen
  | Option : 'a gen -> 'a option gen
  | List : 'a gen -> 'a list gen
  | List1 : 'a gen -> 'a list gen
  | Join : 'a gen gen -> 'a gen
  | Primitive of (state -> 'a)
  | Print of 'a printer * 'a gen
and ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

let pp = Format.fprintf
let pp_int ppf n = pp ppf "%d" n
let pp_int32 ppf n = pp ppf "%s" (Int32.to_string n)
let pp_int64 ppf n = pp ppf "%s" (Int64.to_string n)
let pp_float ppf f = pp ppf "%f" f
let pp_bool ppf b = pp ppf "%b" b
let pp_string ppf s = pp ppf "\"%s\"" (String.escaped s)
let pp_list pv ppf l =
  pp ppf "@[<hv 1>[%a]@]"
     (Format.pp_print_list ~pp_sep:(fun ppf () -> pp ppf ";@ ") pv) l

exception BadTest of string
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
  | Chan ch ->
     input ch buf off len

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

let uint8 = Print(pp_int, Primitive read_byte)
let bool = Print(pp_bool, Primitive read_bool)


let read_int32 src =
  let off = getbytes src 4 in
  EndianBytes.LittleEndian.get_int32 src.buf off

let read_int64 src =
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_int64 src.buf off

let int8 = Print(pp_int, Map ([uint8], fun n -> n - 128))
let int32 = Print (pp_int32, Primitive read_int32)
let int64 = Print (pp_int64, Primitive read_int64)

let int =
  Print (pp_int,
    if Sys.word_size <= 32 then
      Map([int32], Int32.to_int)
    else
      Map([int64], Int64.to_int))

let float = Print (pp_float, Primitive (fun src ->
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_double src.buf off))

(* maybe print as a hexdump? *)
let bytes = Print (pp_string, Primitive (fun src ->
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
  Bytes.sub_string buf 0 count))

let choose n state =
  assert (n > 0);
  if (n < 100) then
    read_byte state mod n
  else if (n < 0x1000000) then
    Int32.(to_int (abs (rem (read_int32 state) (of_int n))))
  else
    Int64.(to_int (abs (rem (read_int64 state) (of_int n))))

let range n = Print (pp_int, Primitive (choose n))

exception GenFailed of exn * Printexc.raw_backtrace * unit printer

let rec generate : type a . int -> state -> a gen -> a * unit printer =
  fun size input gen -> match gen with
  | Const k ->
     k, fun ppf () -> pp ppf "?"
  | Choose xs ->
     (* FIXME: better distribution? *)
     (* FIXME: choices of size > 255? *)
     let gens =
       if size <= 1 then
         match List.filter (function Const _ -> true | _ -> false) xs with
         | [] -> xs
         | small -> small
       else
         xs in
     let n = choose (List.length gens) input in
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
     let v, pvs = gen_apply (size / n) input gens f in
     begin match v with
       | Ok v -> v, pvs
       | Error (e, bt) -> raise (GenFailed (e, bt, pvs))
     end
  | Option gen ->
     if read_bool input then
       let v, pv = generate size input gen in
       Some v, fun ppf () -> pp ppf "Some (%a)" pv ()
     else
       None, fun ppf () -> pp ppf "None"
  | List gen ->
     let elems = generate_list size input gen in
     List.map fst elems,
       fun ppf () -> pp_list (fun ppf (v, pv) -> pv ppf ()) ppf elems
  | List1 gen ->
     let elems = generate_list1 size input gen in
     List.map fst elems,
       fun ppf () -> pp_list (fun ppf (v, pv) -> pv ppf ()) ppf elems
  | Join gengen ->
     let gen, pgen = generate size input gengen in
     let v, pv = generate size input gen in
     v, fun ppf () -> pp ppf "@[<hv 1>[%a; %a]@]" pgen () pv ()
  | Primitive gen ->
     gen input, fun ppf () -> pp ppf "?"
  | Print (ppv, gen) ->
     let v, pv = generate size input gen in
     v, fun ppf () -> ppv ppf v

and generate_list : type a . int -> state -> a gen -> (a * unit printer) list =
  fun size input gen ->
  if read_bool input then
    generate_list1 size input gen
  else
    []

and generate_list1 : type a . int -> state -> a gen -> (a * unit printer) list =
  fun size input gen ->
  let ans = generate size input gen in
  ans :: generate_list size input gen

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

type test_result = (unit, unit printer) result

let check = function
  | true -> Ok ()
  | false -> Error (fun ppf () -> pp ppf "check false")

let check_eq ?pp:pv ?cmp ?eq a b =
  let pass = match eq, cmp with
    | Some eq, _ -> eq a b
    | None, Some cmp -> cmp a b = 0
    | None, None ->
       Pervasives.compare a b = 0 in
  if pass then
    Ok ()
  else
    Error (fun ppf () ->
      match pv with
      | None -> pp ppf "different"
      | Some pv -> pp ppf "@[<hv>%a@ !=@ %a@]" pv a pv b)

let () = Printexc.record_backtrace true

type test = Test : string * ('f, test_result) gens * 'f -> test

type test_status =
  | TestPass of unit printer
  | BadInput of string
  | GenFail of exn * Printexc.raw_backtrace * unit printer
  | TestExn of exn * Printexc.raw_backtrace * unit printer
  | TestFail of unit printer * unit printer

let run_once (gens : (_, test_result) gens) f state =
  match gen_apply 100 state gens f with
  | Ok (Ok v), pvs -> TestPass pvs
  | Ok (Error p), pvs -> TestFail (p, pvs)
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

type mode =
  (* run whichever test this file says to run *)
  | File of string
  (* run tests once using a particular seed *)
  | Seeded of Int64.t
  (* run tests a few times with random seeds and log failures *)
  | Quick of int

let src_of_seed seed =
  (* try to make this independent of word size *)
  let seed = Int64.( [|
       to_int (logand (of_int 0xffff) seed);
       to_int (logand (of_int 0xffff) (shift_right seed 16));
       to_int (logand (of_int 0xffff) (shift_right seed 32));
       to_int (logand (of_int 0xffff) (shift_right seed 48)) |]) in
  Random (Random.State.make seed)

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
  | `Repeat iters ->
     let worst_status = ref (TestPass (fun ppf () -> ())) in
     let npass = ref 0 in
     let nbad = ref 0 in
     while !npass < iters && classify_status !worst_status = `Pass do
       let seed = Random.int64 Int64.max_int in
       let state = { chan = src_of_seed seed;
                     buf = Bytes.make 256 '0';
                     offset = 0; len = 0 } in
       let status = run_once gens f state in
       begin match classify_status status with
       | `Pass -> incr npass
       | `Bad -> incr nbad
       | `Fail ->
          (* if not silent then pp ppf "failed with seed %016LX" seed; *)
          worst_status := status
       end;
     done;
     let status = !worst_status in
     status in
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
let run_all_tests tests =
  match Sys.argv with
  | [| _ |] ->
     (* Quickcheck mode *)
     tests |> List.iter (fun t ->
       ignore (run_test ~mode:(`Repeat 5000) ~silent:false t))
  | [| _; "-i" |] ->
     (* Infinite quickcheck mode *)
     Random.self_init ();
     let rec go ntests alltests tests = match tests with
       | [] ->
          go ntests alltests alltests
       | t :: rest ->
          if ntests mod 10000 = 0 then Printf.eprintf "%d\n%!" ntests;
          match classify_status (run_test ~mode:(`Once { chan = src_of_seed (Random.int64 (Int64.max_int));
                     buf = Bytes.make 256 '0';
                     offset = 0; len = 0 })  ~silent:true t) with
          | `Fail -> Printf.printf "%d\n" ntests
          | _ -> go (ntests + 1) alltests rest in
     go 0 tests tests
  | [| _; file |]
  | [| _; "-v"; file |] ->
     (* AFL mode *)
     let verbose = (Array.length Sys.argv = 3) in
     AflPersistent.run (fun () ->
         let chan = open_in file in
         let state = { chan = Chan chan; buf = Bytes.make 256 '0';
                       offset = 0; len = 0 } in
         let test = List.nth tests (choose (List.length tests) state) in
         let status = run_test ~mode:(`Once state)
                               ~silent:false
                               ~verbose
                               test in
         close_in chan;
         match classify_status status with
         | `Pass | `Bad -> ()
         | `Fail ->
            Printexc.record_backtrace false;
            raise TestFailure)
  | _ -> failwith "command line parsing is not my strong point"

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

let () =
  at_exit (fun () ->
      let t = !registered_tests in
      registered_tests := [];
      match t with
      | [] -> ()
      | t -> run_all_tests (List.rev t))

(*

let runs = ref 0
let rec run_test t =
  if Array.length Sys.argv > 1 then begin
  AflPersistent.run (fun () ->
      let file = open_in Sys.argv.(1) in
      let input = { chan = Chan file;
                    buf = Bytes.make 4096 '0';
                    offset = 0; len = 0 } in
      begin match generate 100 input t with
      | Ok (), _ -> ()
      | exception (BadTest b) ->
         pp Format.std_formatter "bad test: %s\n" b
      | Error perr, pv ->
         pp Format.std_formatter
            "Testcase: {%a}\nFailure: {%a}\n"
            pv () perr ()
      end;
      close_in file)
  end else begin
      let file = open_in "/dev/urandom" in
        let input = { chan = Chan file;
                    buf = Bytes.make 4096 '0';
                    offset = 0; len = 0 } in
      begin match generate 100 input t with
      | Ok (), _ -> ()
      | exception (GenFailed (e, bt, pv)) ->
         pp Format.std_formatter "on testcase %a\ntest threw exn: %s\n%s\n" pv () (Printexc.to_string e) (Printexc.raw_backtrace_to_string bt);
      | exception (BadTest b) ->
         pp Format.std_formatter "bad test: %s\n" b
      | Error perr, pv ->
         pp Format.std_formatter
            "Testcase: %a\nFailure: %a\n"
            pv () perr ()
      end;
      close_in file;
      incr runs;
      if !runs mod 1000 = 0 then Printf.printf "%d\n%!" !runs;
      run_test t
    end

*)
