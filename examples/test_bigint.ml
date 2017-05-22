open Crowbar

let num =
  Map ([List int64], fun ns ->
    List.fold_left (fun n i ->
        Big_int.(add_big_int (shift_left_big_int n 64) (big_int_of_int64 i)))
        Big_int.zero_big_int ns)

let pp_num ppf n =
  Format.fprintf ppf "%s" (Big_int.string_of_big_int n)
let num = Print(pp_num, num)

let () =
  add_test ~name:"bigint" [num; num] @@ fun a b ->
    let b' = Big_int.abs_big_int b in
    guard (not Big_int.(eq_big_int b zero_big_int));
    let sqb = Big_int.sqrt_big_int b' in
    let q, r = Big_int.quomod_big_int a b in
    check (Big_int.(le_big_int (square_big_int sqb) b' &&
               gt_big_int (square_big_int (Big_int.succ_big_int sqb)) b') &&
      Big_int.(eq_big_int a (add_big_int (mult_big_int q b) r)) &&
      Big_int.(le_big_int zero_big_int r && lt_big_int r b'))
  

