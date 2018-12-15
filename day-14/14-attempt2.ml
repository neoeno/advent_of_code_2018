#require "core.top";;
#require "core.syntax";;
open Core;;

let digits n =
  let rec digits' n = match n with
    | n when n < 10 -> [ n ]
    | n ->
        let remaining = (n / 10) in
        n - (remaining * 10) :: digits' remaining in
  List.rev (digits' n)

let lookup_scores scores indexes lookup =
  let index_scores = List.map indexes ~f:(fun item -> (List.nth_exn lookup (snd item))) in
  let trim_to = List.fold ~init:(9999999999) ~f:(min) (List.map index_scores ~f:(List.length)) in
  let index_scores_trimmed = List.map index_scores ~f:(fun list -> List.slice list 0 trim_to) in
  let next_sums = List.map (List.transpose_exn index_scores_trimmed) ~f:(List.fold ~init:(0) ~f:(+)) in
  let new_digits = List.fold ~init:([]) ~f:(fun acc n -> acc @ (digits n)) next_sums in
  let new_scores = scores @ new_digits in
  let scores_count = List.length new_scores in
  let advanced_indexes = List.map (List.zip_exn indexes index_scores_trimmed) ~f:(fun (index, scores) ->
    let new_index = (fst index) + (List.length scores) + (List.fold scores ~init:(0) ~f:(+)) in
    if new_index >= scores_count then
      (new_index % scores_count, new_index % scores_count)
    else
      (new_index, (snd index))
  ) in
  (new_scores, advanced_indexes, lookup)

let iterate_index scores index lookup =
  let score = Option.value_exn (List.nth scores (fst index)) in
  let scores_count = (List.length scores) in
  let new_index = ((fst index) + score + 1) % scores_count in
  if new_index < 10 && ((List.length (List.nth_exn lookup new_index)) != 0) then
    ((new_index, new_index), lookup)
  else
    let strand_id = snd index in
    let new_strand = (List.nth_exn lookup (strand_id)) @ [score] in
    let new_lookup = (List.slice lookup 0 (strand_id)) @ [new_strand] @ (List.slice lookup (strand_id + 1) 0) in
    ((new_index, strand_id), new_lookup)

let rec iterate_indexes scores indexes lookup = match indexes with
  | (index::indexes) ->
    let (new_index, new_lookup) = iterate_index scores index lookup in
    let (new_indexes, new_lookup) = iterate_indexes scores indexes new_lookup in
    (new_index :: new_indexes, new_lookup)
  | [] -> ([], lookup)

let calculate_scores scores indexes lookup =
  let score_sum = List.fold indexes ~init:(0) ~f:(fun sum idx -> sum + (List.nth_exn scores (fst idx))) in
  let new_digits = digits score_sum in
  let new_scores = scores @ new_digits in
  let (new_indexes, new_lookup) = iterate_indexes new_scores indexes lookup in
  (new_scores, new_indexes, new_lookup)

let iterate scores indexes lookup =
  (* Printf.printf "1: %d,%d\n" (fst (List.nth_exn indexes 0)) (snd (List.nth_exn indexes 0));
  Printf.printf "2: %d,%d\n" (fst (List.nth_exn indexes 1)) (snd (List.nth_exn indexes 1));flush stdout; *)
  if List.count indexes ~f:(fun (idx, strand) -> idx = strand ) = (List.length indexes) then
    (Printf.printf "Looking up\n"; flush stdout;
    lookup_scores scores indexes lookup)
  else
    calculate_scores scores indexes lookup

let format (scores, indexes) =
  let active_char i = (if is_some @@ List.find indexes ~f:(fun (a, _) -> a = i) then "#" else "") in
  List.foldi scores ~init:("") ~f:(fun i acc score ->
    acc ^ " " ^ (active_char i) ^ (Int.to_string score))

let rec iterate_n scores indexes lookup from until =
  if from % 1000 = 0 then (Printf.printf "%d\n" from; flush stdout;);
  if from < until then (
    let (scores, indexes, lookup) = iterate scores indexes lookup in
    (* Printf.printf "%s\n" (format (scores, indexes)); flush stdout; *)
    iterate_n scores indexes lookup (from + 1) until
  ) else (
    (* Printf.printf "%s\n" (format (scores, indexes)); flush stdout; *)
    (scores, indexes)
  )

let main n m =
  let scores = [3; 7; 1; 0; 1; 0; 1; 2; 4; 5] in
  let lookup = [[3; 1; 1; 4]; [7; 5]; [1; 1; 1; 4]; [0; 1; 1; 4]; [1; 1; 4]; [0; 1; 4]; [1; 4]; [2]; [4]; [5]; ] in
  let indexes = [(6, 6); (3, 3)] in
  let (scores, indexes) = iterate_n scores indexes lookup 0 (n + m) in
  let slice = List.slice scores n (n + m) in
  List.fold_right (List.map slice ~f:(Int.to_string)) ~init:("") ~f:(^)
