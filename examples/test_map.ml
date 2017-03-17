open Crowbar

module Map = Map.Make (struct
  type t = int
  let compare (i : int) (j : int) = compare i j
end)

type t = ((int * int) list * int Map.t)

let check_map ((list, map) : t) =
  let rec dedup k = function
    | [] -> []
    | (k', v') :: rest when k = k' -> dedup k rest
    | (k', v') :: rest ->
       (k', v') :: dedup k' rest in
  let list = match List.stable_sort (fun a b -> compare (fst a) (fst b)) list with
    | [] -> []
    | (k, v) :: rest -> (k, v) :: dedup k rest in
  List.for_all (fun (k, v) -> Map.find k map = v) list && 
    list = Map.bindings map

let rec map : t gen = Choose [
  Const ([], Map.empty);
  Map ([uint8; uint8; map], fun k v (l, m) ->
    (k, v) :: l, Map.add k v m);
  Map ([uint8; uint8], fun k v ->
    [k, v], Map.singleton k v);
  Map ([uint8; map], fun k (l, m) ->
    let rec rem_all k l =
      let l' = List.remove_assoc k l in
      if l = l' then l else rem_all k l' in
    rem_all k l, Map.remove k m);
  (* merge? *)
  Map ([map; map], fun (l, m) (l', m') ->
    l @ l', Map.union (fun k a b -> Some a) m m');
  Map ([uint8; map], fun k (list, map) ->
    let (l, v, r) = Map.split k map in
    let (l', vr') = List.partition (fun (kx,vx) -> kx < k) list in
    let r' = List.filter (fun (kx, vx) -> kx <> k) vr' in
    let v' = match List.assoc k vr' with n -> Some n | exception Not_found -> None in
    assert (v = v');
    (l' @ List.map (fun (k,v) -> k,v+42) r',
     Map.union (fun k a b -> assert false) l (Map.map (fun v -> v + 42) r)))]

let () =
  add_test ~name:"map" [map] @@ fun m ->
    check (check_map m)
