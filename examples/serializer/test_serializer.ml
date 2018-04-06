open Crowbar

module S = Serializer

type any_ty = Any : 'a S.ty -> any_ty

let ty_gen =
  with_printer (fun ppf (Any t)-> S.pp_ty ppf t) @@
  fix (fun ty_gen -> choose [
    const (Any S.Int);
    const (Any S.Bool);
    map [ty_gen; ty_gen] (fun (Any ta) (Any tb) ->
      Any (S.Prod (ta, tb)));
    map [ty_gen] (fun (Any t) -> Any (List t));
  ])

let prod_gen ga gb = map [ga; gb] (fun va vb -> (va, vb))

let rec gen_of_ty : type a . a S.ty -> a gen = function
  | S.Int -> int
  | S.Bool -> bool
  | S.Prod (ta, tb) -> prod_gen (gen_of_ty ta) (gen_of_ty tb)
  | S.List t -> list (gen_of_ty t)

type pair = Pair : 'a S.ty * 'a -> pair

(* The generator for the final value, [gen_of_ty t], depends on the
   generated type representation, [t]. This dynamic dependency cannot
   be expressed with [map], it requires [dynamic_bind]. *)
let pair_gen : pair gen =
  dynamic_bind ty_gen @@ fun (Any t) ->
  map [gen_of_ty t] (fun v -> Pair (t, v))

let rec printer_of_ty : type a . a S.ty -> a printer = function
  | S.Int -> pp_int
  | S.Bool -> pp_bool
  | S.Prod (ta, tb) -> (fun ppf (a, b) ->
      pp ppf "(%a, %a)" (printer_of_ty ta) a (printer_of_ty tb) b)
  | S.List t -> pp_list (printer_of_ty t)

let check_pair (Pair (t, v)) =
  let data = S.serialize t v in
  match S.deserialize t data with
  | exception _ -> fail "incorrect deserialization"
  | v' -> check_eq ~pp:(printer_of_ty t) v v'

let () = add_test ~name:"pairs" [pair_gen] check_pair
