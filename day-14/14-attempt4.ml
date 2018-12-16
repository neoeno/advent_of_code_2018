#require "core.top";;
#require "core.syntax";;
open Core;;
open List.Monad_infix;;

module OptimizedSequence = struct
  type record = { idx: int; value: int; }
  type lookup_field = GoTo of (int * int) | Unreached
  type seq = { mutable head: int; lists: record list array; list_lengths: int array; lookup: lookup_field array }

  let make n m = { head = -1; lists = Array.create n []; list_lengths = Array.create n 0; lookup = Array.create m Unreached }

  let add_m seq strand idx value =
    seq.lists.(strand) <- { idx = idx; value = value } :: seq.lists.(strand);
    seq.list_lengths.(strand) <- seq.list_lengths.(strand) + 1;
    seq.lookup.(idx) <- GoTo (strand, seq.list_lengths.(strand))

  let get seq idx =
    if idx < 0 then None else
    match seq.lookup.(idx) with
      | GoTo (strand, offset) ->
        let sequence = seq.lists.(strand) in
        let index = (seq.list_lengths.(strand) - offset) in
        Some (List.nth_exn sequence index).value
      | _ -> None

  let get_onwards seq idx =
    match seq.lookup.(idx) with
      | GoTo (strand, offset) ->
        let sequence = seq.lists.(strand) in
        let index = (seq.list_lengths.(strand) - offset) in
        List.slice sequence 0 (index + 1)
        |> List.map ~f:(fun r -> r.value)
        |> List.rev
      | _ -> []

  let find_strands_attaching_to_index seq idx =
    List.range 0 10
    |> List.filter ~f:(fun steps ->
      (get seq (idx - steps - 1))
      |> Option.map ~f:((=) steps)
      |> Option.value ~default:(false))
    |> List.map ~f:(fun steps ->
      let to_idx = (idx - steps - 1) in
      match seq.lookup.(to_idx) with
        | GoTo (strand, _) -> strand
        | _ -> raise (Failure "Nope")
    )

  let push seq value =
    seq.head <- seq.head + 1;
    match seq.head with
      | n when n <= 9 ->
        find_strands_attaching_to_index seq seq.head
        |> List.iter ~f:(fun strand -> add_m seq strand seq.head value);
        add_m seq seq.head seq.head value;
      | n ->
        (find_strands_attaching_to_index seq seq.head)
        |> List.iter ~f:(fun strand -> add_m seq strand seq.head value)

  let next_index seq = 1 + seq.head

  let rec with_values seq indexes n ~init:init ~index:indexfn ~f:fn =
    let rec run_sequence sequences indexes ~init:init =
      if seq.head % 1000 = 0 then Printf.printf "Running sequence %d\n" seq.head; flush stdout;
      let values = List.map sequences ~f:(List.hd) |> Option.all in
      match values with
        | Some list ->
          let acc = fn init list in
          let next_sequences = List.map sequences ~f:(List.tl_exn) in
          let new_indexes = List.map (List.zip_exn indexes list) ~f:(fun (a, b) -> (a + b + 1) % (seq.head + 1)) in
          (* Printf.printf "Indexes: %s\n" (indexes |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b));
          Printf.printf "Indexes: %s\n" (new_indexes |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
          run_sequence next_sequences new_indexes ~init:acc
        | None -> (init, indexes) in
    let sequences = List.map indexes ~f:(fun idx -> get_onwards seq idx) in
    (* Printf.printf "Getting seqence from indexes: %s\n" (indexes |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
    (* Printf.printf "Sequence 0: %s\n" ((List.nth_exn sequences 0) |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
    (* Printf.printf "Sequence 1: %s\n" ((List.nth_exn sequences 1) |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
    let (acc, new_indexes) = run_sequence sequences indexes ~init:init in
    match n with
      | n when n < 0 -> (acc, new_indexes)
      | n -> with_values seq new_indexes (n - 1) ~init:acc ~index:indexfn ~f:fn
    ;;

  let get_tail seq =
    List.range (seq.head - 9) (seq.head + 1)
    |> List.map ~f:(get seq)
    |> List.map ~f:(Option.value ~default:0)
end

let digits n =
  let rec digits' n = match n with
    | n when n < 10 -> [ n ]
    | n ->
        let remaining = (n / 10) in
        n - (remaining * 10) :: digits' remaining in
  List.rev (digits' n)

let iterate_sequence seq indexes =
  let values = List.map indexes ~f:(fun idx -> Option.value_exn (OptimizedSequence.get seq idx)) in
  let summed = List.fold values ~init:(0) ~f:(+) in
  let digits = digits summed in (
    List.map digits ~f:(OptimizedSequence.push seq);
    digits;
  )

let calculate_next_idx seq idx =
  Option.value_exn (OptimizedSequence.get seq idx)
  |> (+) (1 + idx)

let iterate_indexes seq indexes =
  (* Printf.printf "indexes: %s\n" (indexes |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
  List.map indexes ~f:(calculate_next_idx seq)
  |> List.map ~f:(fun next -> next % (OptimizedSequence.next_index seq))

let iterate_n seq indexes n =
  OptimizedSequence.with_values seq indexes n ~index:(iterate_indexes) ~init:([]) ~f:(fun scores values ->
    let summed = List.reduce_exn values ~f:(+) in
    let digits = digits summed in (
      List.map digits ~f:(OptimizedSequence.push seq);
      (* Printf.printf "scores:  %s\n" (scores |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      (* Printf.printf "values:  %s\n" (values |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      (* Printf.printf "digits:  %s\n" (digits |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      (List.rev digits) @ scores
    )
  )

let pattern_matches_head list [a2;b2;c2;d2;e2;f2] = match list with
  | (a1::b1::c1::d1::e1::f1::_) -> a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2 && e1 = e2 && f1 = f2
  | _ -> false

let iterate_n_searching seq indexes n pattern =
  OptimizedSequence.with_values seq indexes n ~index:(iterate_indexes) ~init:(([], 0)) ~f:(fun scores values ->
    let summed = List.reduce_exn values ~f:(+) in
    let digits = digits summed in (
      List.map digits ~f:(OptimizedSequence.push seq);
      (* Printf.printf "scores:  %s\n" (scores |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      (* Printf.printf "values:  %s\n" (values |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      (* Printf.printf "digits:  %s\n" (digits |> List.map ~f:(Int.to_string) |> List.fold ~init:("") ~f:(fun a b -> a ^ ", " ^ b)); *)
      List.fold digits ~init:(scores) ~f:(fun scores digit ->
        let new_scores = digit :: (fst scores) in
        if pattern_matches_head new_scores pattern then (
          Printf.printf "Found: %d\n" ((snd scores) - 3); flush stdout;
          raise (Failure "ENDED");
          ([], 0)
        ) else
          (List.slice new_scores 0 (min (List.length new_scores) 10), (snd scores) + 1)
      )
    )
  )


let rec iterate_n_ seq indexes from until =
  if from < until then (
    let new_digits = iterate_sequence seq indexes in
    let indexes = iterate_indexes seq indexes in
    let (new_scores, indexes) = iterate_n_ seq indexes (from + 1) until in
    (new_digits @ new_scores, indexes)
  ) else (
    ([], indexes)
  )

let rec search_for (a1::b1::c1::d1::e1::f1::rest) [a2;b2;c2;d2;e2;f2] n =
  if a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2 && e1 = e2 && f1 = f2 then
    (n, [a1; b1; c1; d1; e1; f1])
  else
    search_for (b1::c1::d1::e1::f1::rest) [a2;b2;c2;d2;e2;f2] (n + 1)

let step2 n m =
  let indexes = [0; 1] in
  let seq = OptimizedSequence.make 10 (n + 100000000) in (
    OptimizedSequence.push seq 3;
    OptimizedSequence.push seq 7;
    let (scores, indexes) = iterate_n_searching seq indexes m (List.rev (digits n)) in
    (* (seq, List.length scores, List.slice (List.rev scores) (n - 2) (n + 8)) *)
    let (offset, found_line) = search_for (List.rev (fst scores)) (digits n) 2 in
    (offset, found_line, List.slice (List.rev (fst scores)) (offset - 2) (offset + 8))
  )


(* 20696327 is too high *)
