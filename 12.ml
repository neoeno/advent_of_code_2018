open Core;;

type pattern = (bool * bool * bool * bool * bool)
type rule = { pattern: pattern; value: bool }

type terminal_pattern = {
  n: int;
  start_num: int;
  interval: int;
  pots: bool list;
}

let iterate_pot rules chunk =
  let rev_trim_pots pots = List.rev (List.drop_while ~f:((=) false) (List.rev pots)) in
  let applicable_rule = List.find rules ~f:(fun rules -> chunk = rules.pattern) in
  match applicable_rule with
    | Some rule -> rule.value
    | None -> false

let rec trim_pots (start_num, pots) = match (rev_trim_pots pots) with
  | false :: pots -> trim_pots (start_num + 1, pots)
  | pots -> (start_num, pots)

let iterate_pots rules (start_num, pots) =
  let false_chunk = [ false; false; false; false; false ] in
  let rec iterate_pots' pots = match pots with
    | [ false; false; false; false; false ] -> [ false; false; false; false; false ]
    | [ a; b; c; d; e ] -> (iterate_pot rules (a, b, c, d, e)) :: (iterate_pots' ([ b; c; d; e; false ]))
    | a::b::c::d::e::ns -> (iterate_pot rules (a, b, c, d, e)) :: (iterate_pots' (b::c::d::e::ns)) in
  trim_pots (start_num - 3, (iterate_pots' (false_chunk @ pots)))

let rec find_terminal_pattern rules (start_num, pots) n =
  let (next_start_num, next_pots) = iterate_pots rules (start_num, pots) in
  if next_pots = pots then
    { n = n;
      start_num = start_num;
      interval = next_start_num - start_num;
      pots = pots; }
  else
    find_terminal_pattern rules (next_start_num, next_pots) (n + 1)

let iterate_pots_n rules pots n =
  let rec iterate_pots_n' pots n = match n with
    | 0 -> pots
    | n -> iterate_pots_n' (iterate_pots rules pots) (n - 1) in
  let terminal = find_terminal_pattern rules pots 0 in
  if n < terminal.n then
    iterate_pots_n' pots n
  else
    (
      terminal.start_num + terminal.interval * (n - terminal.n),
      terminal.pots
    )

let rec count_plant_nos (start, pots) = match pots with
  | true  :: pots -> start + (count_plant_nos (start + 1, pots))
  | false :: pots -> count_plant_nos (start + 1, pots)
  | []            -> 0

let rec format pots = match pots with
  | true :: pots  -> "#" ^ (format pots)
  | false :: pots -> "." ^ (format pots)
  | []            -> ""

let rec visualize rules pots from until =
  if from < until then
    let (start_num, calculated_pots) = iterate_pots_n rules pots from in
    Printf.printf "%d %d %s\n" from start_num (format calculated_pots);
    visualize rules pots (from + 1) until
  else
    ignore ()

let parse_state initial =
  String.to_list initial
  |> List.map ~f:((=) '#')

let parse_config filename =
  let (line_1 :: _ :: lines) = In_channel.read_lines filename in
  let initial = Scanf.sscanf line_1 "initial state: %s" parse_state in
  let rules = List.map lines ~f:(fun line ->
    Scanf.sscanf line "%s => %s" (fun pattern value ->
      let [a; b; c; d; e] = parse_state pattern in
      {pattern = (a, b, c, d, e); value = (value = "#"); };
    )
  ) in
  (initial, rules)

let main =
  let (initial, rules) = parse_config "12.txt" in
  let step_1 = count_plant_nos (iterate_pots_n rules (0, initial) 20) in
  let step_2 = count_plant_nos (iterate_pots_n rules (0, initial) 50000000000) in
  Format.printf "Step 1: %d\n" step_1;
  Format.printf "Step 2: %d\n" step_2;
