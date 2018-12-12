#use "topfind";;
#thread;;
#require "core.top";;
#require "core.syntax";;

open Core;;

type vector = { x: int; y: int };;

let get_hundreds_digit n = (n / 100) - (n / 1000) * 10;;

let calculate_power_level serial { x = x; y = y } =
  let rack_id = x + 10 in
  let power_level_1 = (y * rack_id + serial) * rack_id in
  let hundreds_digit = get_hundreds_digit (power_level_1) in
  hundreds_digit - 5;;

let vec_add a b = { x = a.x + b.x; y = a.y + b.y }

let all_points_within start until =
  List.fold_left ~init:[] ~f:(fun points x ->
    List.fold_left ~init:points ~f:(fun points y ->
      { x = x ; y = y } :: points
    ) (List.range start.y until.y)
  ) (List.range start.x until.x)

let max_by list fn = match list with
  | [] -> invalid_arg "Can't max an empty list"
  | x::xs -> fst (List.fold_left ~f:(
    fun a b -> if snd a > snd b then a else b) ~init:(x, fn x) (List.map xs (fun x -> (x, fn x))));;

let square_power point serial size =
  let points = all_points_within point (vec_add point { x = size ; y = size }) in
  List.fold_left ~init:0 ~f:(fun sum point ->
    sum + calculate_power_level serial point
  ) points

let square_power_for_points points serial =
  List.fold_left ~init:0 ~f:(fun sum point ->
    sum + calculate_power_level serial point
  ) points

let square_power_for_all_sizes point serial =
  let max_size = min (300 - point.x) (300 - point.y) in
  let sizes = List.range 1 max_size in
  List.fold_left ~init:[(0, 0)] ~f:(fun ((last_sum, last_size)::sums) size ->
    if last_sum < -100 then (last_sum, last_size) :: sums else
    let new_row = all_points_within { x = point.x; y = point.y + size - 1 } { x = point.x + size; y = point.y + size } in
    let new_col = all_points_within { x = point.x + size - 1; y = point.y } { x = point.x + size; y = point.y + size - 1 } in
    let sum = last_sum + (square_power_for_points new_row serial) + (square_power_for_points new_col serial) in
    (sum, size) :: (last_sum, last_size) :: sums
  ) sizes

let max_square_power_for_all_sizes point serial =
  max_by (square_power_for_all_sizes point serial) fst

let find_max_square_power serial =
  let best_point = max_by
    (all_points_within { x = 0; y = 0 } { x = 300; y = 300 })
    (fun point -> fst (max_square_power_for_all_sizes point serial)) in
  let best_size = max_square_power_for_all_sizes best_point serial in
  (best_point, snd best_size)
