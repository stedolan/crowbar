type data =
  | Datum of string
  | Block of header * data list
and header = string

type _ ty =
  | Int : int ty
  | Bool : bool ty
  | Prod : 'a ty * 'b ty -> ('a * 'b) ty
  | List : 'a ty -> 'a list ty

let rec pp_ty : type a . _ -> a ty -> unit = fun ppf ->
  let printf fmt = Format.fprintf ppf fmt in
  function
  | Int -> printf "Int"
  | Bool -> printf "Bool"
  | Prod(ta, tb) -> printf "Prod(%a,%a)" pp_ty ta pp_ty tb
  | List t -> printf "List(%a)" pp_ty t

let rec serialize : type a . a ty -> a -> data = function
  | Int -> fun n -> Datum (string_of_int n)
  | Bool -> fun b -> Datum (string_of_bool b)
  | Prod (ta, tb) -> fun (va, vb) ->
    Block("pair", [serialize ta va; serialize tb vb])
  | List t -> fun vs ->
    Block("list", List.map (serialize t) vs)

let rec deserialize : type a . a ty -> data -> a = function[@warning "-8"]
  | Int -> fun (Datum s) -> int_of_string s
  | Bool -> fun (Datum s) -> bool_of_string s
  | Prod (ta, tb) -> fun (Block("pair", [sa; sb])) ->
    (deserialize ta sa, deserialize tb sb)
  | List t -> fun (Block("list", ss)) ->
    List.map (deserialize t) ss
