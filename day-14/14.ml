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

let iterate_scores scores indexes =
  let score_sum = List.fold indexes ~init:(0) ~f:(fun sum idx -> sum + (Option.value_exn (List.nth scores idx))) in
  let score_sum_digits = digits score_sum in
  scores @ score_sum_digits

let iterate_index scores index =
  let score = Option.value_exn (List.nth scores index) in
  (index + score + 1) % (List.length scores)

let iterate_indexes scores = List.map ~f:(iterate_index scores)

let rec iterate_n scores indexes from until =
  if from % 1000 = 0 then (Printf.printf "%d\n" from; flush stdout;);
  if from < until then (
    let scores = iterate_scores scores indexes in
    let indexes = iterate_indexes scores indexes in
    Printf.printf "%s\n" (List.fold indexes ~init:("") ~f:(fun acc a -> acc ^ ", " ^ (Int.to_string (List.nth_exn scores a))));
    iterate_n scores indexes (from + 1) until
  ) else (
    (scores, indexes)
  )

let format (scores, indexes) =
  let active_char i = (if is_some @@ List.find indexes ~f:((=) i) then "#" else "") in
  List.foldi scores ~init:("") ~f:(fun i acc score ->
    acc ^ " " ^ (active_char i) ^ (Int.to_string score))

let main n m =
  let scores = [3; 7] in
  let indexes = [0; 1] in
  (* Printf.printf "%s\n" (format (iterate_n scores indexes 0 n)) *)
  let (scores, _) = iterate_n scores indexes 0 (n + m) in
  Printf.printf "%s\n" (format (scores, []));
  let slice = List.slice scores n (n + m) in
  List.fold_right (List.map slice ~f:(Int.to_string)) ~init:("") ~f:(^)
