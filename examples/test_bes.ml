open Crowbar
open Bes
let ext_bool = Choose [Const `True; Const `False; Const `Dontcare] 
let rec minterm = function 
  | 0 -> Const [] 
  | n -> Map ([ext_bool; minterm (n-1)], fun x xs -> x :: xs)
let pp_dnf ppf d = Format.fprintf ppf "\n%s" (Bes.string_of_dnf_expression d)
let dnf = Print(pp_dnf, List (minterm 3))
let algo = Choose [
               Map ([], Qmc);
               Const Qmc_CyclicCore;
               Const Qmc_SimpleHeuristic_SortOnce;
               Const Qmc_SimpleHeuristic_SortEachTime;
               Const Qmc_SimpleHeuristic_AdvancedHeuristic;
               Const Qmc_SimpleHeuristic_Best;
               Const Qmc_PetricksMethod;
               (*Const In_Place_Heuristic*)]
let () =
  Bes.set_result_verification true;
  add_test ~name:"bes" [algo; dnf] @@ fun a d ->
    ignore (Bes.optimize a d); Bes.cache := Bes.Dnfexpressionmap.empty; check true
