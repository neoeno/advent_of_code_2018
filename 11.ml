open Core;;

type vector = { x: int; y: int };;

let get_hundreds_digit n = (n / 100) - (n / 1000) * 10;;

let calculate_power_level serial { x = x; y = y } =
  let rack_id = x + 10 in
  let power_level_1 = (y * rack_id + serial) * rack_id in
  let hundreds_digit = get_hundreds_digit (power_level_1) in
  hundreds_digit - 5;;

let vec_add a b = { x = a.x + b.x; y = a.y + b.y }

let rec all_points_within start until =
  let rec all_points_within' current start until =
    current :: if current = until then
      []
    else if current.x >= until.x then
      all_points_within' { x = start.x; y = current.y + 1 } start until
    else
      all_points_within' { x = current.x + 1; y = current.y } start until in
  all_points_within' start start until;;

let make_n_square n point = all_points_within point (vec_add point { x = n - 1; y = n - 1 });;

let all_n_squares n start until = List.map (all_points_within start until) (make_n_square n);;

let sum = List.reduce ~f:(+);;

let sum_power_level serial points = sum (List.map points (calculate_power_level serial));;

let max_by list fn = match list with
  | [] -> invalid_arg "Can't max an empty list"
  | x::xs -> fst (List.fold_left ~f:(
    fun a b -> if snd a > snd b then a else b) ~init:(x, fn x) (List.map xs (fun x -> (x, fn x))));;

let square_power point serial size = sum_power_level serial (make_n_square size point);;

let find_max_power serial size =
  max_by
    (all_points_within { x = 0; y = 0 } (vec_add { x = 300; y = 300 } { x = -1 * size; y = -1 * size }))
    (fun point -> square_power point serial size);;
