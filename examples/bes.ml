(* $Id: promela.mli 2996 2012-03-14 09:58:12Z weissmam $

Copyright (c) 2011 - 2012 Technische Universitaet Muenchen
Copyright (c) 2011 Alexander Ostrovsky <ostrovsk@in.tum.de
Copyright (c) 2011 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Apple Inc. nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

(***********************************************)
(**     Optimizer for logical expressions      *)
(***********************************************)
(*open ExtLib*)

module List = struct
include List
let sort ?(cmp=compare) = List.sort cmp

end

(* Note: Monoms and minterms are used here interchangeable *)

(*================DATA TYPES=======================*)

let is_verbose_mode = ref false

let should_verify_results = ref false

(* This trick increases the code readability *)
let (|>) a b = b a

(** A boolean type with 3 values : true, false and don't care
   Some true means true, Some false means false and None means
   don't care *)
type ext_bool = [ `True | `False | `Dontcare ]

(** A minterm with true, false and dont't care values *)
type dnf_minterm = ext_bool list

(** A sum of minterms *)
type dnf_expression = dnf_minterm list

module Dnfexpressionmap = Map.Make(struct
   type t = dnf_expression
   let compare = Pervasives.compare
end)

(** The algorithm for the optimization *)
type optimization_algorithm =
   | Qmc
   (** Quine-McCluskey only *)

   | Qmc_CyclicCore
   (** Quine-McCluskey, then cyclic core *)

   | Qmc_SimpleHeuristic_SortOnce
   (** Quine-McCluskey, then cyclic core, then heuristic cover search.
      The impicants are sorted by the number of covered minterms once
      before the optimization. The length of the implications is used
      as the heuristic value.  *)

   | Qmc_SimpleHeuristic_SortEachTime
   (** Quine-McCluskey, then cyclic core, then heuristic cover search.
      The impicants are sorted by the number of covered minterms after
      each step. The length of the implications is used as the
      heuristic value. *)

   | Qmc_SimpleHeuristic_AdvancedHeuristic
   (** Quine-McCluskey, then cyclic core, then heuristic cover search.
      The impicants are sorted by the number of covered minterms
      after each step. Advanced heuristic formula (see thesis) is used
      to calculate the heuristic value *)

   | Qmc_SimpleHeuristic_Best
   (** Quine-McCluskey, then cyclic core, then heuristic cover search.
      The impicants are sorted by the number of covered minterms after
      each step. Advanced heurisitc or the length of the
      implications is used as heuristic value, depending on the
      better results. *)

   | Qmc_PetricksMethod
   (** Quine-McCluskey and then Petrick's method *)

   | In_Place_Heuristic

(** This exaption is raised whenever some operation is not possible.
   E.g. merging two minterms with different number of literals
   (including the don't care ones) is such oprtation*)
exception Not_possible

(*==================CONVERSIONS====================*)
(** Converts the given string to a minterm. 1 is true,
   0 is false and (small) x is don't matter. *)
let dnf_minterm_of_string (str : string) =
   let result = ref [] in
   String.iter
   (function
      | 'x' -> result := `Dontcare :: !result
      | '0' -> result := `False :: !result
      | '1' -> result := `True :: !result
      | _ -> raise (Invalid_argument "minterm_of_string"))
   str;
   !result

(** Converts the given string, separated  by the given separator to
   a dnf expression. 1 is true, 0 is false and (small) x
   is don't matter.*)
let rec dnf_expression_of_string (separator : char) (str : string) =
   if String.rindex str separator <> (String.length str) - 1 then
      dnf_expression_of_string separator (String.concat "" [str; (Char.escaped separator)])
   else
      let parse_next_minterm (str : string) =
         let pos = String.index str separator in
         let minterm_string = String.sub str 0 pos in
         let minterm = dnf_minterm_of_string minterm_string in
         let rest = String.sub str (pos + 1) ((String.length str) - (String.length minterm_string) - 1) in
         (List.rev minterm, rest)
      in
      let rec parse_string str =
         if String.length str = 0 then
            []
         else
            let (parsed_minterm, rest) = parse_next_minterm str in
            parsed_minterm :: (parse_string rest)
         in
      parse_string str

(** Converts the given string list to a dnf expression.
   1 is true, 0 is false and (small) x is don't matter.*)
let dnf_expression_of_string_list (lst : string list) =
   dnf_expression_of_string ';' (String.concat ";" lst)


(** Converts a minterm of a dnf expression to a string *)
let string_of_dnf_minterm (m : dnf_minterm) =
   let res_as_list = ref [] in
   List.iter
      (function
         | `Dontcare -> res_as_list := "x" :: !res_as_list
         | `True -> res_as_list := "1" :: !res_as_list
         | `False -> res_as_list := "0" :: !res_as_list
      ) m;
   String.concat "" (List.rev !res_as_list)

(** Converts a dnf expression to a string *)
let string_of_dnf_expression (expr : dnf_expression) =
   let res_as_list = ref [] in
   List.iter (fun x -> res_as_list := (string_of_dnf_minterm x) :: !res_as_list) expr;
   String.concat "\n\r" (List.rev !res_as_list)

(*==============LOADING FROM FILES=================*)

(** Loads a dnf expression from file with the given name. The file should
   contain one minterm per line. The format of the minterm
   representations is as following: 1 means true, 0 means false and
   x means don't matter. Lines starting with any other characters
   then 0, 1 or x are ignored. *)
let load_from_file file_name =
   let res = ref [] in
   let in_channel = open_in file_name in
   let rec read_lines () =
      try
         let resl = ref [] in
         let line = input_line in_channel in
         let first_char = String.get line 0 in
         if (first_char = '0' || first_char = '1' || first_char = 'x') then
            begin
            String.iter
               (function
                  | '0' -> resl := `False :: !resl
                  | '1' -> resl := `True :: !resl
                  | 'x' -> resl := `Dontcare :: !resl
                  | _ -> () (*do nothing*))
               line;
            res := !resl :: !res
            end
         else
            ()
         ;
         read_lines ()
      with End_of_file -> ()
   in
   read_lines ();
   List.map (fun x -> List.rev x) !res

(*========ADDITIONAL OPERATIONS ON LISTS===========*)

(** Flattens a list of lists *)
let fast_flatten (l : 'a list list) =
   List.fold_left (fun a x ->  List.rev_append x a) [] l
   (*List.flatten l*)

(** Maps a list *)
let fast_map f l =
   (*List.map f l*)
   Array.to_list (Array.map f (Array.of_list l))

(** Remove duplicates from a list. It is equavalien to creating a
    set of list and converting it back to the list *)
let remove_duplicates lst =
   if lst <> [] then
      let rec doit s lst = function
         | [] -> []
         | xs::xr when lst <> xs || s -> xs::(doit false xs xr)
         | xs::xr -> doit false xs xr
      in
      lst
      |> List.fast_sort compare
      |> doit true (List.hd lst)
   else
      []

(** Returns whether l1 is subset of l2 *)
let is_subset l1 l2 =
   let rec doit = function
      | xs::xr -> if List.mem xs l2 then doit xr else false
      | [] -> true
   in
   doit l1

(** Returns a list of sublists of the length <= n.
   Note: The result may contain some results twice, but
   it is ok for our purpuses. *)
let sublists_of_n n l =
   if n < 1 then raise (Invalid_argument "n should be >= 1");
   let rec doit n l =
      if n = 0 then
         l
      else
         fast_flatten
            (List.map (fun x -> List.map (fun y -> remove_duplicates(x@y)) l) (doit (n - 1) l))
   in
   List.map (fun x -> [x]) l
   |> doit (n - 1)

(** Removes the items which are in the list l1 from the other list*)
let rec remove_sublist_from_list l1 =
   List.filter (fun x -> not (List.mem x l1))

(*====================DEBUG========================*)

type debug_level = Info | Waring | Error

(** Prints a debug message to the standard output *)
let debug_print (lvl : debug_level) (str : string) =
   if !is_verbose_mode then
      match lvl with
      | Info -> print_string "Info: "; print_endline str
      | Waring -> print_string "WARNING: "; print_endline str
      | Error -> print_string "!ERROR!: "; print_endline str
   else
      ()

(** Sets whether the debug info should be
    printed to the standard output *)
let set_verbose_mode m =
   is_verbose_mode := m

(*================OUTPUT===========================*)

(** Prints a minterm of a dnf expression to the standard
   output *)
let print_dnf_minterm m =
   print_endline (string_of_dnf_minterm m)

(** Prints a dnf expression to the standard output *)
let print_dnf_expression expr =
   print_endline (string_of_dnf_expression expr)

(*===========OPTIMIZATION ROUTINES=================*)


(*==================HELPERS========================*)
(** Counts the 'True's in the given dnf_minterm.
    Used for sorting the minterms of the expression *)
let count_non_negated_variables =
   List.fold_left (fun a x -> if x = `True then a + 1 else a) 0

(** Returns the number of trues + falses in a minterm *)
let minterm_size (m : dnf_minterm) =
   m
   |> List.filter ((<>) `Dontcare)
   |> List.length

(** Sorts and groups the minterms of the expression by the number
    of non negated variables and adds and indicator what terms have
    been merged to produce the resulting term Needed. because two
    minterms can only be merged if the number of non negated variables
    differs by one *)
let group_expression (expr : dnf_expression) =
      expr
      (* Sort *)
      |> List.fast_sort
            (fun a b ->
               (count_non_negated_variables a)
               - (count_non_negated_variables b))
      (* Group *)
      |> List.fold_left
            (fun (a, cnt) x ->
               let act_cnt = count_non_negated_variables x in
               if act_cnt > cnt then
                  (* start a new group... *)
                  ([(x, ref false)]::a, act_cnt)
               else
                  (* ...or add the expression to an existing one *)
                  (((x, ref false)::(List.hd a))::(List.tl a), cnt))
            ([], -1)
      |> fst

(** Merges two minterms if it is possible and returns Some result
    or returns None if the it is not possible to merge. *)
let try_merge_two_minterms (m1 : dnf_minterm) (m2 : dnf_minterm) : dnf_minterm option =
   let rec doit = function
      | x::xs, y::ys, cnt when x = y && cnt <= 1 ->
         x::(doit (xs, ys, cnt))
      | x::xs, y::ys, cnt when (x <> y) && cnt <= 1 ->
         `Dontcare::(doit(xs, ys, (cnt + 1)))
      | [], [], cnt  when cnt <= 1 ->
         []
      | _ ->
         raise Not_possible
   in
   try
      Some (doit (m1, m2, 0))
   with Not_possible ->
      None

(** Expands the don't care literals. I.e [[x..]; ..] will
   be turned into [[1..]; [0..]; ..] *)
let rec expand_minterm = function
   | [] -> [[]]
   | h::t when h <> `Dontcare ->
      List.fold_left (fun xs t -> (h::t)::xs) [] (expand_minterm t)
   | h::t when h = `Dontcare ->
      List.fold_left
         (fun xs t -> (`True::t)::(`False::t)::xs)
         []
         (expand_minterm t)
   | _ -> raise Not_possible


(** Returns true if expr_from implies expr_to *)
let rec is_implication (expr_from : dnf_minterm) (expr_to : dnf_minterm) =
   let expanded_from = expand_minterm expr_from in
   let expanded_to = expand_minterm expr_to in
   is_subset expanded_to expanded_from

(*========SEARCH FOR PRIME IMPICANTS===============*)

(** Finds the prime implicants of a dnf-expression. An implicant
    is a sum of some minterms. A prime implicant is an implicant
    that cannot be convered by other implicants, except by itself *)
let cache = ref Dnfexpressionmap.empty

let find_prime_implicants (expr : dnf_expression) =
   try
      Dnfexpressionmap.find expr !cache
   with Not_found -> begin
      let grouped_expr = group_expression expr in
      let remove_merged grouped_expr =
         let z =
            fast_map
               (fun x -> List.filter (fun (y, b) -> not !b) x)
               grouped_expr
         in
        List.filter (function | [] -> false | _ -> true) z
      in
      (* Scans the minterms pairwise, whether they can
         be merged and merges if it is possible *)
      let rec do_merging = function
         | xs1::xs2::xr ->
         let merges = fast_map
            (fun (x, bx) ->
               (x, bx)::fast_map
               (fun (y, by) ->
                  match try_merge_two_minterms x y with
                  | Some z -> bx := true; by := true; (z, ref false)
                  | None -> (y, by)
               )
               xs1
            )
         xs2 in
         (remove_duplicates (fast_flatten merges))::do_merging(xs2::xr)
       | x -> x
      in
      (* Merges the minterms as long as it is possible *)
      let rec consecutive_merge = function
         | [] -> []
         | xs::[] -> xs::[]
         | x -> x |> do_merging |> remove_merged |> consecutive_merge
      in
      debug_print Info "Searching prime implicants started...";
      let (res, _) =
         grouped_expr
         |> consecutive_merge
         |> fast_flatten
         |> List.split
      in
      debug_print Info "Searching prime implicants finished...";
      debug_print Info ((string_of_int (List.length res)) ^ " prime implicants found");
      cache := Dnfexpressionmap.add expr res !cache;
      res
   end

(*============SEARCH FOR MINIIMAL COVER============*)

(** Finds the implications of the prime implicants *)
let find_implications (expr : dnf_expression) (implicants : dnf_expression) =
   debug_print Info "Search for Implications started...";
   let implications =
      fast_map (fun x -> x, (List.filter (fun y -> is_implication x y) expr))
      implicants
   in
   let back_implications =
      fast_map (fun x -> x, (List.filter (fun y -> is_implication y x) implicants))
      expr
   in
   debug_print Info "Search for Implications finished...";
   (implications, back_implications)

(** Finds the essential implications of the implications. They must
   occur in the minimized expression.*)
let find_essential_implications (implications, back_implications) =
   let (exprs, _) =
      back_implications
      |> List.filter (fun (x, lx) -> (List.length lx) = 1)
      |> List.split
   in
   let imps =
      List.filter
         (fun (x, lx) -> List.exists (fun y -> List.mem y lx) exprs)
         implications
   in
   let (essential_implicants, covered_minterms) = List.split imps in
   let covered_minterms = remove_duplicates (fast_flatten covered_minterms) in
   let not_covered_minterms =
      List.fold_left
         (fun a (x, _) ->
            if (List.mem x covered_minterms) then
               a
            else
               x::a)
         []
         back_implications
   in
   (essential_implicants, covered_minterms, not_covered_minterms)

(** Splits the given implications and back_implications into the essential
      and non essential ones *)
let split_essential_not_essential implications back_implications =
   let (essential_implicants, covered_minterms, not_covered_minterms) =
      find_essential_implications (implications, back_implications) in
   let (es_imp, non_es_imp) =
      List.partition (fun (x, _) -> List.mem x essential_implicants) implications in
   let (es_b_imp, non_es_b_imp) =
      List.partition (fun (x, _) -> List.mem x covered_minterms) back_implications in
   let non_es_imp =
      List.map (fun (w, x)  -> w, (List.filter (fun y -> List.mem y not_covered_minterms) x)) non_es_imp in
   let non_es_b_imp =
      List.map (fun (w, x)  -> w, (List.filter (fun y -> not (List.mem y essential_implicants)) x)) non_es_b_imp in
   (es_imp, non_es_imp, es_b_imp, non_es_b_imp)

(** Calculates the cyclic core of the given implications,
   back_implication pair *)
let cyclic_core implications back_implications =
   (** Returns whether imp1 dominates imp2 *)
   let is_row_dominance imp1 imp2 =
      match imp1, imp2 with
      | (_, x), (_, y) when x <> y && is_subset y x -> true
      | _ -> false
   in
   (** Returns whether bimp1 dominates bimp2 *)
   let is_column_dominance bimp1 bimp2 =
      match bimp1, bimp2 with
      | (_, x), (_, y) when x <> y && is_subset x y -> true
      | _ -> false
   in
   (** Does the row and column dominance checks *)
   let rec do_dominance implications back_implications =
      debug_print Info "Checking dominance...";
      let implications_new = implications in
      let back_implications_new = back_implications in
      (* row dominance *)
      let implications_new =
         implications_new
         |> List.filter
            (fun x -> not (List.exists (fun y -> is_row_dominance y x) implications))
         |> List.filter (fun (_, x) -> x <> [])
      in
      let implicants = List.map (fun (x, _) -> x) implications_new in
      (* remove deleted implicants from back_implications *)
      let back_implications_new =
         List.map
            (fun (x, xl) -> x, (List.filter (fun y -> List.mem y implicants) xl))
            back_implications_new
      in
      (* column dominance *)
      let back_implications_new =
         back_implications_new
         |> List.filter
            (fun x -> not (List.exists (fun y -> is_column_dominance y x) back_implications))
         |> List.filter (fun (_, x) -> x <> [])
      in
      let minterms = List.map (fun (x, _) -> x) back_implications_new in
      (* remove deleted minterms from implications *)
      let implications_new =
         List.map
            (fun (x, xl) -> x, (List.filter (fun y -> List.mem y minterms) xl))
            implications_new
      in
      if (List.length back_implications_new) < (List.length back_implications)
         || (List.length implications_new) < (List.length implications) then
         do_dominance implications_new back_implications_new
      else
         (implications_new, back_implications_new)
   in
   (* Does the essentiality check and calls the dominance check routine *)
   let rec do_cyclic_core essential_implications non_essential_implications
                      essential_back_implications non_essential_back_implications =
       debug_print Info "Checking essentiality...";

      (* Count the non essential imps and back-imps before the dominance test *)
      let (non_essential_implication_count,
          non_essential_back_implication_count) =
         ((List.length non_essential_implications), (List.length non_essential_back_implications)) in

      (* Do the dominance test *)
      let (non_essential_implications_new, non_essential_back_implications_new) =
         do_dominance non_essential_implications non_essential_back_implications in

      (* Split the new implication chart into the essential and non essential parts *)
      let (essential_implications_new,
          non_essential_implications_new,
          essential_back_implications_new,
          non_essential_back_implications_new) =
         split_essential_not_essential non_essential_implications_new non_essential_back_implications_new in

      (* Count the new non essential imps, and back-imps*)
      let (non_essential_implication_count_new,
          non_essential_back_implication_count_new) =
         ((List.length non_essential_implications_new), (List.length non_essential_back_implications_new)) in

      debug_print Info ((string_of_int (List.length essential_implications_new)) ^ " new essential implicants found");

      (* If the dominance check was successful then we have to try the whole procedure again
         with the new implications, back-implications pair *)
      if (non_essential_implication_count_new < non_essential_implication_count)
         || (non_essential_back_implication_count_new < non_essential_back_implication_count) then
         do_cyclic_core
            (fast_flatten [essential_implications; essential_implications_new])
            non_essential_implications_new
            (fast_flatten [essential_back_implications; essential_back_implications_new])
            non_essential_back_implications_new
      else
         ((fast_flatten [essential_implications_new; essential_implications; non_essential_implications_new]),
         (fast_flatten [essential_back_implications_new; essential_back_implications; non_essential_back_implications_new]))
   in
   let (es_imp, non_es_imp, es_b_imp, non_es_b_imp) =
      split_essential_not_essential implications back_implications in
   let _ = debug_print Info ("Originally: " ^ (string_of_int (List.length non_es_imp)) ^ " non essential implicants") in
   let (implications, back_implications) =
      do_cyclic_core es_imp non_es_imp es_b_imp non_es_b_imp in
   let (e, _, _) = find_essential_implications (implications, back_implications) in
   let _ = debug_print Info ("Ater cyclic core: " ^ (string_of_int ((List.length implications) - (List.length e))) ^ " non essential implicants") in
   (implications, back_implications)

(** Makes a simple heuristic optmization using the following algorithm:
   1) res = []
   2) sort non essential implicants descending by the number
      of not covered minterms
   3) for each non essential implicant t, if t covers some minterms
      not covered yet then res = t :: res
   3.1) if sort_each_time then the rest of non essential implicants are
        sorted again
   5) return res *)
let simple_heuristic implications
                     essential_implicants
                     covered_minterms
                     not_covered_minterms
                     use_advanced_heuristic
                     sort_each_time =
    let non_essential_implications =
      implications
      (* Remove covered miterms *)
      |> List.map
         (fun (x, t) -> (x, remove_sublist_from_list covered_minterms t))
      (* Sort by the number of still covered miterms *)
      |> List.fast_sort
         (fun (x1, t1) (x2, t2) -> (List.length t1) - (List.length t2))
      (* Filter the non essential implications *)
      |> List.fold_left
         (fun a (x, t) -> if List.mem x essential_implicants then a else (x, t)::a)
         []
   in

   let all_minterms_multiset =
      non_essential_implications
      |> List.fold_left (fun a (_, x) -> x::a) []
      |> fast_flatten
   in

   (* Minterms which are covered by the use of this algorithm and were not covered before *)
   let new_covered_minterms = ref [] in
   List.iter
      (fun x -> new_covered_minterms := x :: (!new_covered_minterms))
      covered_minterms;

   (* Returns the heuristic value for the implication l *)
   let choose l =
      let minterms =
         l
         |> snd
         |> remove_sublist_from_list !new_covered_minterms
      in
      if use_advanced_heuristic then
         minterms
         |> List.map
            (fun x ->
               let cov =
                  (List.filter (fun y -> x = y) all_minterms_multiset)
                  |> List.length
                  |> float_of_int
               in
               1. /. (cov -. 1.) )
         |> List.fold_left (+.) 0.
         |> ( *. ) 128.
         |> int_of_float
      else
         List.length minterms
   in

   debug_print Info ("Heuristic optimization with "
                  ^ (string_of_int (List.length non_essential_implications))
                  ^ " non essential implicants");
   let covered_count () = List.length !new_covered_minterms in
   let rec mark_covered = function
      | xs::xr ->
         if not (List.exists (fun x -> x = xs) !new_covered_minterms) then
            new_covered_minterms := xs :: (!new_covered_minterms)
         else ()
         ;
         mark_covered xr
      | [] -> ()
   in
   let is_all_covered () =
      (List.length !new_covered_minterms) =
         (List.length covered_minterms) + (List.length not_covered_minterms)
   in
   (* TODO: Nicht jedes mal !new_covered_minterms löschen sondern Ergebnisse zwischenspreichern *)
   let rec do_optimization l =
      let ls =
         if sort_each_time then
            List.sort ~cmp:(fun x y -> choose y - choose x) l
         else
            l
      in
      match ls with
      | (xs, ts)::xr when not (is_all_covered ()) ->
         let c_before = covered_count () in
         mark_covered ts;
         let c_after = covered_count () in
         if c_after > c_before then
            xs::(do_optimization xr)
         else
            do_optimization xr
      | _ -> []
   in
   fast_flatten [(do_optimization non_essential_implications); essential_implicants]

(** Uses the petrick's method for exact minimization *)
let petricks_method implications
                    back_implications
                    essential_implicants
                    covered_minterms
                    not_covered_minterms =
   (* Remove the essential implications and the covered minterms of them *)
   let expression =
      back_implications
      |> fast_map
         (
            fun (x, y) ->
               if (List.mem x covered_minterms) then
                  []
               else
                  y
                  |> fast_map
                     (fun z ->
                        if (List.mem z essential_implicants) then
                           []
                        else
                           [z]
                     )
                  |> List.filter (fun x -> x <> [])
         )
      |> List.filter (fun x -> x <> [])
   in
   let number_of_implications = List.length expression in
   debug_print Info
      ("Petrick's optimization with " ^ (string_of_int number_of_implications) ^ " implications");
   if number_of_implications > 10 then
      debug_print Waring  "Too many implications. The optimization may take some time..."
   else
      ();
   (* utilizes the X + XY <=> X law *)
   let remove_subsets lst =
      let rec doit = function
         | xs::xr ->
            if List.exists (fun x -> (is_subset x xs) && x<>xs) lst then
               doit xr
            else
               xs::(doit xr)
         | [] -> []
      in
      doit lst
   in
   let rec multiply = function
      | xs1::xs2::xr ->
         ((fast_flatten (fast_map (fun x -> fast_map (fun y -> remove_duplicates (x@y)) xs2) xs1)) :: xr)
         |> remove_subsets
         |> multiply
      | x -> x
   in
   let _ = debug_print Info "Multiplying..." in
   let multiplication_results =
      expression
      |> multiply
      |> fast_flatten
      |> remove_duplicates
      |> remove_subsets
   in

   (* Finds the minimal expression that covers all minterms
      (which were uncovered by the essential implicanta) by trying out
      every subset of the multiplication result. The length of the subset
      is increased every run. It is necessary the multiplication result
      may be very long *)
   let rec find_minimal_result len_of_subset =
      if number_of_implications = 0 then
         []
      else
         let res =
            multiplication_results
            (* Get all subsets of the given size *)
            |> sublists_of_n len_of_subset
            (* Get the implications of the results (resp. subsets) *)
            |> List.map (fun x -> ((fast_flatten x), x))
            |> List.map
               (fun (x, y) -> (fast_flatten (List.map (fun z -> List.assoc z implications) x), y))
            (* Filter away all results (subsets) that do not cover all miterms *)
            |> List.filter
               (fun (x, xl) -> (fun y -> List.for_all (fun z -> List.mem z x) not_covered_minterms) x)
            |> List.map (fun (_, x) -> x)
            (* Sort by the number of literals *)
            |> List.fast_sort
               (fun x y -> minterm_size (fast_flatten (fast_flatten x)) - minterm_size (fast_flatten (fast_flatten y)))
            (* Sort by the number of monoms *)
            |> List.stable_sort
               (fun x y -> List.length (fast_flatten x) - List.length (fast_flatten y))
         in
         if res = [] then (* No compbination found. *)
            find_minimal_result (len_of_subset + 1)
         else
            res
   in
   let _ = debug_print Info "Choosing Results..." in
   match find_minimal_result 1 with
      | [] -> essential_implicants
      | xs::xr -> fast_flatten (xs @ [essential_implicants])



(*===========IN PLACE HEURISTIC PRIME IMPLICANT SEARCH==================*)

(** EXPREREMNTAL, NOT TESTED: A heuristic optimization method that
   uses the Expand step instead of the QMC algorithm *)
let rec mini (expr : dnf_expression) =
   let rec merge_possible_miterms (ps : dnf_minterm) (pr : dnf_minterm list) =
      match pr with
      | xs::xr ->
         if List.exists (fun y -> List.hd y = `Dontcare) pr then
            `Dontcare :: (merge_possible_miterms (List.tl ps) xr)
         else
            (List.hd ps) :: (merge_possible_miterms (List.tl ps) xr)
      | [] -> ps
   in
   let remove_covered m =
      List.filter (fun x -> not (is_implication m x))
   in
   let rec do_merging = function
      | xs::xr ->
         let possible_minterms =
            List.fold_left (fun a x -> match try_merge_two_minterms xs x with None -> a | Some y -> y :: a) [xs] xr in
         let new_minterm = (merge_possible_miterms (List.hd possible_minterms) (List.tl possible_minterms)) in
         let not_covered = remove_covered new_minterm xr in
         new_minterm :: (do_merging not_covered)
      | _ -> []
   in
   let new_expr = do_merging expr in
   if new_expr = expr then new_expr else mini new_expr

(*===========TEST FOR EQUIVALENCE==================*)

(** Tests whether two expressions are equivalent *)
let test_minimization_equivalence (expr1: dnf_expression) (expr2 : dnf_expression) =
   let rec expand_minterm = function
      | [] -> [[]]
      | h::t when h <> `Dontcare -> List.fold_left (fun xs t -> (h::t)::xs) [] (expand_minterm t)
      | h::t when h = `Dontcare -> List.fold_left (fun xs t -> (`True::t)::(`False::t)::xs) [] (expand_minterm t)
      | _ -> raise Not_possible
   in
   let expand_expression (expr : dnf_expression) =
      expr
      |> List.map expand_minterm
      |> fast_flatten
      |> remove_duplicates
   in
   let e1 =
      expr1
      |> expand_expression
      |> remove_duplicates
      |> List.sort
   in
   let e2 =
      expr2
      |> expand_expression
      |> remove_duplicates
      |> List.sort
   in
   assert (e1 = e2)

(** Sets whether the optimization results should be verfied *)
let set_result_verification m =
   should_verify_results := m



(*=====OPTIMIZATION METHODS FOR THE INTERFACE======*)
(** Optimizes the given expression with the suitable algoirthm.
   When there is more then 12 non essential prime implicants
   Qmc_SimpleHeuristic_Best is used, otherwise Qmc_PetricksMethod.
   The result is a tuple of the minimized expression and a boolean
   which indicates whether the optimization was exact (true) or not*)
let auto_optimize expr =
   let implicants = find_prime_implicants expr in
   let (implications, back_implications) =
      find_implications expr implicants
   in
   let (implications, back_implications) =
      cyclic_core implications back_implications in
   let (essential_implicants, covered_minterms, not_covered_minterms) =
      find_essential_implications (implications, back_implications)
   in
   let (_, non_es_imps, _, _)  =
      split_essential_not_essential implications back_implications
   in
   let exact = List.length non_es_imps <= 12 in
   if exact then
      let optimized =
         petricks_method implications back_implications essential_implicants covered_minterms not_covered_minterms
      in
      if !should_verify_results then test_minimization_equivalence expr optimized;
      (optimized, true)
   else
      let optimized =
         let m1 = simple_heuristic implications essential_implicants covered_minterms not_covered_minterms true true in
         let m2 = simple_heuristic implications essential_implicants covered_minterms not_covered_minterms false true in
         if (List.length m1) < (List.length m2) then m1 else m2
      in
      if !should_verify_results then test_minimization_equivalence expr optimized;
      (optimized, false)

(** Optimizes the given expression with the specified algorithm *)
let optimize (algorithm : optimization_algorithm) (expr : dnf_expression) =
   match algorithm with
   | Qmc ->
      let optimized = find_prime_implicants expr in
      if !should_verify_results then test_minimization_equivalence expr optimized;
      optimized
   | Qmc_CyclicCore ->
      let implicants = find_prime_implicants expr in
      let (implications, back_implications) =
         find_implications expr implicants
      in
      let (implications, _) =
         cyclic_core implications back_implications
      in
      let optimized = List.map (fun (x, _) -> x) implications in
      if !should_verify_results then test_minimization_equivalence expr optimized;
      optimized
   | In_Place_Heuristic ->
      let implicants = mini expr in
      let (implications, back_implications) =
         find_implications expr implicants
      in
      let (implications, back_implications) =
         cyclic_core implications back_implications in
      let (essential_implicants, covered_minterms, not_covered_minterms) =
         find_essential_implications (implications, back_implications)
      in
         let optimized =
            simple_heuristic implications essential_implicants covered_minterms not_covered_minterms true true in
         if !should_verify_results then test_minimization_equivalence expr optimized;
         optimized
   | Qmc_SimpleHeuristic_SortOnce
   | Qmc_SimpleHeuristic_SortEachTime
   | Qmc_SimpleHeuristic_AdvancedHeuristic
   | Qmc_SimpleHeuristic_Best
   | Qmc_PetricksMethod ->
      let implicants = find_prime_implicants expr in
      let (implications, back_implications) =
         find_implications expr implicants
      in
      let (implications, back_implications) =
         cyclic_core implications back_implications in
      let (essential_implicants, covered_minterms, not_covered_minterms) =
         find_essential_implications (implications, back_implications)
      in
      let optimized =
         if algorithm = Qmc_SimpleHeuristic_SortOnce then
            simple_heuristic implications essential_implicants covered_minterms not_covered_minterms false false
         else if algorithm = Qmc_SimpleHeuristic_SortEachTime then
            simple_heuristic implications essential_implicants covered_minterms not_covered_minterms false true
         else if algorithm = Qmc_PetricksMethod then
            petricks_method implications back_implications essential_implicants covered_minterms not_covered_minterms
         else if algorithm = Qmc_SimpleHeuristic_AdvancedHeuristic then
            simple_heuristic implications essential_implicants covered_minterms not_covered_minterms true true
         else if algorithm = Qmc_SimpleHeuristic_Best then
            let m1 = simple_heuristic implications essential_implicants covered_minterms not_covered_minterms true true in
            let m2 = simple_heuristic implications essential_implicants covered_minterms not_covered_minterms false true in
            if (List.length m1) < (List.length m2) then m1 else m2
         else
            raise Not_possible
      in
      if !should_verify_results then test_minimization_equivalence expr optimized;
      optimized
