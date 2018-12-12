open Core;;

type pattern = (bool * bool * bool * bool * bool)
type rule = { pattern: pattern; value: bool }

let iterate_pot chunk =
  let applicable_rule = List.find rules ~f:(fun rules -> chunk = rules.pattern) in
  match applicable_rule with
    | Some rule -> rule.value
    | None -> false

let rec trim_pots (start_num, pots) = match pots with
  | false :: pots -> trim_pots (start_num + 1, pots)
  | pots -> (start_num, pots)

let iterate_pots (start_num, pots) =
  let false_chunk = [ false; false; false; false; false ] in
  let rec iterate_pots' pots = match pots with
    | [ false; false; false; false; false ] -> [ false; false; false; false; false ]
    | [ a; b; c; d; e ] -> (iterate_pot (a, b, c, d, e)) :: (iterate_pots' ([ b; c; d; e; false ]))
    | a::b::c::d::e::ns -> (iterate_pot (a, b, c, d, e)) :: (iterate_pots' (b::c::d::e::ns)) in
  trim_pots (start_num - 3, (iterate_pots' (false_chunk @ pots)))

let rec iterate_pots_n pots n = match n with
  | 0 -> pots
  | n -> iterate_pots_n (iterate_pots pots) (n - 1)

let rec count_plant_nos (start, pots) = match pots with
  | true  :: pots -> start + (count_plant_nos (start + 1, pots))
  | false :: pots -> count_plant_nos (start + 1, pots)
  | []            -> 0

  let rules = [
    {pattern = (false, false, false, true, true); value = false; };
    {pattern = (false, false, false, true, false); value = true; };
    {pattern = (false, false, false, false, true); value = false; };
    {pattern = (true, true, true, false, true); value = true; };
    {pattern = (false, false, false, false, false); value = false; };
    {pattern = (false, false, true, false, false); value = false; };
    {pattern = (true, false, true, false, true); value = false; };
    {pattern = (true, false, false, true, false); value = false; };
    {pattern = (true, false, false, false, true); value = false; };
    {pattern = (true, true, false, false, false); value = false; };
    {pattern = (false, true, false, true, false); value = true; };
    {pattern = (false, true, false, false, true); value = false; };
    {pattern = (false, true, true, true, false); value = false; };
    {pattern = (true, false, false, true, true); value = true; };
    {pattern = (false, false, true, false, true); value = true; };
    {pattern = (false, true, true, true, true); value = true; };
    {pattern = (true, true, false, false, true); value = true; };
    {pattern = (true, true, false, true, false); value = true; };
    {pattern = (false, true, false, false, false); value = true; };
    {pattern = (true, false, true, false, false); value = false; };
    {pattern = (true, true, true, true, true); value = false; };
    {pattern = (true, true, true, false, false); value = true; };
    {pattern = (false, true, true, false, true); value = false; };
    {pattern = (true, false, true, true, false); value = false; };
    {pattern = (false, false, true, true, true); value = false; };
    {pattern = (false, true, false, true, true); value = true; };
    {pattern = (false, false, true, true, false); value = true; };
    {pattern = (true, false, true, true, true); value = false; };
    {pattern = (false, true, true, false, false); value = true; };
    {pattern = (true, true, false, true, true); value = false; };
    {pattern = (true, false, false, false, false); value = false; };
    {pattern = (true, true, true, true, false); value = true; };
  ]

let initial = [ true; false ; false ; false ; true; true; true; true; false ; true; true; false ; false ; true; true; true; true; false ; false ; true; false ; true; true; false ; false ; false ; false ; true; true; false ; false ; false ; true; true; true; false ; true; true; false ; true; false ; false ; true; true; true; true; true; true; false ; false ; true; false ; false ; true; false ; false ; true; true; true; false ; false ; true; true; false ; true; false ; true; true; true; false ; true; true; true; true; true; false ; true; true; false ; true; false ; true; false ; true; false ; true; true; false ; false ; false ; false ; true; false ; false ; true; false ; false ; true; false ; false ];;
