#require "core.top";;
#require "core.syntax";;
open Core;;

type lookup_field = List of int list | GoTo of (int * int) | Uncalculated;;

let digits n =
  let rec digits' n = match n with
    | n when n < 10 -> [ n ]
    | n ->
        let remaining = (n / 10) in
        n - (remaining * 10) :: digits' remaining in
  List.rev (digits' n)

let iterate_scores scores indexes =
  let score_sum = List.fold indexes ~init:(0) ~f:(fun sum idx -> sum + (Option.value_exn (List.nth scores idx))) in
  let score_sum_digits = digits score_sum in
  scores @ score_sum_digits

let iterate_index scores index =
  let score = Option.value_exn (List.nth scores index) in
  (index + score + 1) % (List.length scores)

let iterate_indexes scores = List.map ~f:(iterate_index scores)

let rec iterate_n scores indexes from until =
  if from < until then (
    let scores = iterate_scores scores indexes in
    let indexes = iterate_indexes scores indexes in
    iterate_n scores indexes (from + 1) until
  ) else (
    (scores, indexes)
  )

let lookup_scores scores indexes lookup =
  let index_scores = List.map indexes ~f:(fun index -> match lookup.(index) with
    | GoTo (a, b) -> (match lookup.(a) with
      | List ls -> Some (List.slice ls b 0)
      | GoTo _ -> raise (Failure "Deep goto")
      | Uncalculated -> raise (Failure "Uncalculated goto"))
    | List a -> Some a
    | Uncalculated -> None
  ) in
  if List.count index_scores ~f:(Option.is_none) > 0 then
    (scores, indexes)
  else
    let index_scores = List.map index_scores ~f:(fun a -> Option.value_exn a) in
    let trim_to = List.fold ~init:(9999999999) ~f:(min) (List.map index_scores ~f:(fun a -> List.length a)) in
    let index_scores_trimmed = List.map index_scores ~f:(fun list -> List.slice list 0 trim_to) in
    let next_sums = List.map (List.transpose_exn index_scores_trimmed) ~f:(List.fold ~init:(0) ~f:(+)) in
    let new_digits = List.fold ~init:([]) ~f:(fun acc n -> acc @ (digits n)) next_sums in
    let new_scores = scores @ new_digits in
    let scores_count = List.length new_scores in
    let advanced_indexes = List.map (List.zip_exn indexes index_scores_trimmed) ~f:(fun (index, scores) ->
      let new_index = index + (List.length scores) + (List.fold scores ~init:(0) ~f:(+)) in
      if new_index >= scores_count then
        new_index % scores_count
      else
        new_index
    ) in (
      Printf.printf "Saved %d scores\n" ((List.length new_scores) - (List.length scores)); flush stdout;
      (new_scores, advanced_indexes)
    )

let format (scores, indexes) =
  let active_char i = (if is_some @@ List.find indexes ~f:((=) i) then "#" else "") in
  List.foldi scores ~init:("") ~f:(fun i acc score ->
    acc ^ " " ^ (active_char i) ^ (Int.to_string score))

let consolidate scores lookup =
  let rec collect_scores_from scores = match scores with
    | [] -> []
    | (score::scores) -> score :: (collect_scores_from (List.drop scores (score))) in
  List.iter ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9]) ~f:(fun i ->
    match lookup.(i) with
      | List existing when i = 7 ->
        let sequence = (collect_scores_from (List.drop scores i)) in (
          lookup.(i) <- List sequence;
          ignore (List.foldi sequence ~init:(i) ~f:(fun i2 sum score ->
            let place = sum + score + 1 in (
              lookup.(place) <- GoTo (i, i2 + 1);
              place
            )
          )))
      | Uncalculated ->
        let sequence = (collect_scores_from (List.drop scores i)) in (
          lookup.(i) <- List sequence;
          ignore (List.foldi sequence ~init:(i) ~f:(fun i2 sum score ->
            let place = sum + score + 1 in (
              lookup.(place) <- GoTo (i, i2 + 1);
              place
            )
          )))
      | _ -> ignore ()
  ); lookup


let rec go_hard_until scores indexes lookup n =
  let (scores, indexes) = iterate_n scores indexes 0 100 in
  let lookup = consolidate scores lookup in
  let (scores, indexes) = lookup_scores scores indexes lookup in
  if (List.length scores) > n then
    scores
  else (
    Printf.printf "Generating scores (%d)\n" (List.length scores);
    flush stdout;
    go_hard_until scores indexes lookup n
  )

let rec search_for (a1::b1::c1::d1::e1::f1::rest) [a2;b2;c2;d2;e2;f2] n =
  if a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2 && e1 = e2 && f1 = f2 then
    n
  else
    search_for (b1::c1::d1::e1::f1::rest) [a2;b2;c2;d2;e2;f2] (n + 1)

let step1 n m =
  let scores = [3; 7] in
  let indexes = [0; 1] in
  let lookup = Array.create (n + m + 1000) Uncalculated in
  let scores = go_hard_until scores indexes lookup (n + 100) in
  let slice = List.slice scores n (n + m) in
  List.fold_right (List.map slice ~f:(Int.to_string)) ~init:("") ~f:(^)

let step2 n m pattern =
  let scores = [3; 7] in
  let indexes = [0; 1] in
  let lookup = Array.create (n + m + 1000) Uncalculated in
  let scores = go_hard_until scores indexes lookup (n + 100) in
  search_for scores pattern 0
