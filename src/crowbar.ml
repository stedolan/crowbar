(* FIXME: add join/bind?
Command-line interface:
  quickcheck or fuzz
Nice output
 *)
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
  | Primitive of (state -> 'a)
  | Print of 'a printer * 'a gen
and ('k, 'res) gens =
  | [] : ('res, 'res) gens
  | (::) : 'a gen * ('k, 'res) gens -> ('a -> 'k, 'res) gens

type nonrec +'a list = 'a list = [] | (::) of 'a * 'a list

exception BadTest of string
let guard = function
  | true -> ()
  | false -> raise (BadTest "guard failed")
let bad_test () = raise (BadTest "bad test")

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


let read_byte src =
  let off = getbytes src 1 in
  Char.code (Bytes.get src.buf off)

let read_bool src =
  let n = read_byte src in
  n land 1 = 1

let uint8 = Primitive read_byte
let bool = Primitive read_bool
                   
let int8 = Map ([uint8], fun n -> n - 128)

let int32 = Primitive (fun src ->
  let off = getbytes src 4 in
  EndianBytes.LittleEndian.get_int32 src.buf off)

let int64 = Primitive (fun src ->
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_int64 src.buf off)

let int =
  if Sys.word_size <= 32 then
    Map([int32], Int32.to_int)
  else
    Map([int64], Int64.to_int)

let float = Primitive (fun src ->
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_double src.buf off)

let bytes = Primitive (fun src ->
  let off1 = getbytes src 1 in
  let n = Char.code src.buf.[off1] mod 64 in
  let off2 = getbytes src n in
  Bytes.sub_string src.buf off2 n)

let range n =
  assert (0 <= n && n < 256);   (* FIXME *)
  Map ([uint8], fun k -> k mod n)


let pp = Format.fprintf
let pp_int ppf n = pp ppf "%d" n
let pp_float ppf f = pp ppf "%f" f
let pp_bool ppf b = pp ppf "%b" b
let pp_string ppf s = pp ppf "\"%s\"" (String.escaped s)
let pp_list pv ppf l =
  pp ppf "@[<hv 1>[%a]@]"
     (Format.pp_print_list ~pp_sep:(fun ppf () -> pp ppf ";@ ") pv) l

exception GenFailed of exn * Printexc.raw_backtrace * unit printer

let choose state n =
  assert (n < 100);             (* FIXME *)
  read_byte state mod n

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
     let n = choose input (List.length gens) in
     let v, pv = generate size input (List.nth gens n) in
     v, fun ppf () -> pp ppf "%d: %a" n pv ()
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
  | TestPass
  | BadInput of string
  | GenFail of exn * Printexc.raw_backtrace * unit printer
  | TestExn of exn * Printexc.raw_backtrace * unit printer
  | TestFail of unit printer * unit printer

let run_once (gens : (_, test_result) gens) f state =
  match gen_apply 100 state gens f with
  | Ok (Ok v), _ -> TestPass
  | Ok (Error p), pvs -> TestFail (p, pvs)
  | Error (e, bt), pvs -> TestExn (e, bt, pvs)
  | exception (BadTest s) -> BadInput s
  | exception (GenFailed (e, bt, pvs)) -> GenFail (e, bt, pvs)

let classify_status = function
  | TestPass -> `Pass
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
  | TestPass ->
     pp ppf "The test passed."
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

let run_test ~mode ~quiet (Test (name, gens, f)) =
  let show_status_line ?(clear=false) ?(c=Notty.A.empty) stat =
    let open Notty in
    Notty_unix.output_image_size ~clear (fun (w, _) ->
      let i1 = I.string c name in
      let i2 = I.strf ~attr:c "[%04s]" stat in
      I.(i1 <|> void (min w 80 - width i1 - width i2) 1 <|> i2));
    if clear then print_newline ();
    flush stdout in
  let ppf = Format.std_formatter in
  if not quiet then show_status_line ~clear:false "....";
  let status = match mode with
  | `Once state ->
     run_once gens f state
  | `Repeat iters ->
     let worst_status = ref TestPass in
     let npass = ref 0 in
     let nbad = ref 0 in
     while !npass < iters && !worst_status = TestPass do
       let seed = Random.int64 Int64.max_int in
       let state = { chan = src_of_seed seed;
                     buf = Bytes.make 256 '0';
                     offset = 0; len = 0 } in
       let status = run_once gens f state in
       begin match classify_status status with
       | `Pass -> incr npass
       | `Bad -> incr nbad
       | `Fail ->
          (* if not quiet then pp ppf "failed with seed %016LX" seed; *)
          worst_status := status
       end;
     done;
     let status = !worst_status in
     status in
  if not quiet then begin
      match classify_status status with
      | `Pass ->
         show_status_line 
           ~clear:true ~c:Notty.A.(fg green) "PASS"
      | `Fail ->
         show_status_line
           ~clear:true ~c:Notty.A.(fg lightred ++ st bold) "FAIL";
         pp ppf "%a@." print_status status;
      | `Bad ->
         show_status_line
           ~clear:true ~c:Notty.A.(fg yellow) "BAD";
         pp ppf "%a@." print_status status;
    end;
  status

exception TestFailure
let run_all_tests tests =
  match Sys.argv with
  | [| _ |] ->
     (* Quickcheck mode *)
     tests |> List.iter (fun t ->
       ignore (run_test ~mode:(`Repeat 5000) ~quiet:false t))
  | [| _; file |] ->
     (* AFL mode *)
     AflPersistent.run (fun () ->
         let chan = open_in file in
         let state = { chan = Chan chan; buf = Bytes.make 256 '0';
                       offset = 0; len = 0 } in
         let test = List.nth tests (choose state (List.length tests)) in
         let status = run_test ~mode:(`Once state) ~quiet:false test in
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
