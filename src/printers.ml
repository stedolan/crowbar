type 'a printer = Format.formatter -> 'a -> unit
let pp = Format.fprintf
let pp_int ppf n = pp ppf "%d" n
let pp_int32 ppf n = pp ppf "%s" (Int32.to_string n)
let pp_int64 ppf n = pp ppf "%s" (Int64.to_string n)
let pp_float ppf f = pp ppf "%f" f
let pp_bool ppf b = pp ppf "%b" b
let pp_string ppf s = pp ppf "\"%s\"" (String.escaped s)
let pp_printer ppf (pv : unit printer) = pv ppf ()
let pp_exn ppf e = pp ppf "%s" (Printexc.to_string e)
let pp_exn_bt ppf (e, bt) =
  pp_exn ppf e;
  bt
  |> Printexc.raw_backtrace_to_string
  |> Str.split (Str.regexp "\n")
  |> List.iter (pp ppf "@,%s")

let pp_delim ppf = function
  | "[", `Close -> pp ppf "]"
  | "{", `Close -> pp ppf "}"
  | "(", `Close -> pp ppf ")"
  | "<", `Close -> pp ppf ">"
  | s, (`Open|`Close) -> pp ppf "%s" s

let pp_list ?(delim="[") ?(sep=";") pv ppf l =
  pp ppf "@[<hv 1>%a%a%a@]"
     pp_delim (delim, `Open)
     (Format.pp_print_list ~pp_sep:(fun ppf () -> pp ppf "%s@ " sep) pv) l
     pp_delim (delim, `Close)
