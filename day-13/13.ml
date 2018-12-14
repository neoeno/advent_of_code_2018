#require "core.top";;
#require "core.syntax";;

open Core;;

type turn = Forward | Left | Right

type cart = {
  position: (int * int);
  direction: (int * int);
  next_turn: turn;
}

module Vector = struct
  let add (ax, ay) (bx, by) = (ax + bx, ay + by)
end

module Grid = struct
  let map grid ~f:fn =
    List.map grid ~f:(fun line ->
      List.map line ~f:(fn)
    )

  let find grid ~f:fn =
    List.foldi grid ~init:([]) ~f:(fun row acc line ->
      List.foldi line ~init:(acc) ~f:(fun col acc cell ->
        if fn cell then (col, row) :: acc else acc
      )
    )

  let get grid (x, y) =
    let row = Option.value ~default:([]) (List.nth grid y) in
    Option.value ~default:(' ') (List.nth row x)

  let set grid (x, y) new_cell =
    List.mapi grid ~f:(fun row line ->
      List.mapi line ~f:(fun col cell ->
        if (col, row) = (x, y)
          then new_cell
          else cell
      )
    )
end

let parse filename =
  let grid = In_channel.read_lines filename in
  List.map grid ~f:(String.to_list)

let strip_carts =
  Grid.map ~f:(fun cell -> match cell with
    | '^' -> '|'
    | '>' -> '-'
    | 'v' -> '|'
    | '<' -> '-'
    | c -> c
  )

let get_cart_coords = Grid.find ~f:(fun cell ->
  cell = '^' || cell = '>' || cell = 'v' || cell = '<')

let cart_direction_vector cell = match cell with
  | '^' -> ( 0, -1 )
  | '>' -> ( 1,  0 )
  | 'v' -> ( 0,  1 )
  | '<' -> (-1,  0 )
  | _ -> raise (Failure "Invalid cart direction")

let extract_carts grid =
  get_cart_coords grid
  |> List.map ~f:(fun coord ->
    { position = coord;
      direction = cart_direction_vector (Grid.get grid coord);
      next_turn = Left }
  )

let change_direction cart position = match cart.next_turn with
  | Left -> (match cart.direction with
    | ( 1, 0 ) -> { position = position; direction = ( 0,-1 ); next_turn = Forward }
    | (-1, 0 ) -> { position = position; direction = ( 0, 1 ); next_turn = Forward }
    | ( 0,-1 ) -> { position = position; direction = (-1, 0 ); next_turn = Forward }
    | ( 0, 1 ) -> { position = position; direction = ( 1, 0 ); next_turn = Forward }
    | _ -> raise (Failure "Invalid direction"))
  | Right -> (match cart.direction with
    | ( 1, 0 ) -> { position = position; direction = ( 0, 1 ); next_turn = Left }
    | (-1, 0 ) -> { position = position; direction = ( 0,-1 ); next_turn = Left }
    | ( 0,-1 ) -> { position = position; direction = ( 1, 0 ); next_turn = Left }
    | ( 0, 1 ) -> { position = position; direction = (-1, 0 ); next_turn = Left }
    | _ -> raise (Failure "Invalid direction"))
  | Forward -> { position = position; direction = cart.direction; next_turn = Right }

let iterate_cart grid cart =
  let next_position = (Vector.add cart.position cart.direction) in
  let next_cell = Grid.get grid next_position in
  match next_cell with
  | '|' -> { position = next_position; direction = cart.direction; next_turn = cart.next_turn }
  | '-' -> { position = next_position; direction = cart.direction; next_turn = cart.next_turn }
  | '/' -> (match cart.direction with
    | ( 1, 0 ) -> { position = next_position; direction = ( 0,-1 ); next_turn = cart.next_turn }
    | (-1, 0 ) -> { position = next_position; direction = ( 0, 1 ); next_turn = cart.next_turn }
    | ( 0,-1 ) -> { position = next_position; direction = ( 1, 0 ); next_turn = cart.next_turn }
    | ( 0, 1 ) -> { position = next_position; direction = (-1, 0 ); next_turn = cart.next_turn }
    | _ -> raise (Failure (Format.sprintf "Invalid cart turn cell '%c' for (%d, %d) -> (%d, %d)" next_cell (fst cart.position) (snd cart.position) (fst cart.direction) (snd cart.direction) )))
  | '\\' -> (match cart.direction with
    | (-1, 0 ) -> { position = next_position; direction = ( 0,-1 ); next_turn = cart.next_turn }
    | ( 1, 0 ) -> { position = next_position; direction = ( 0, 1 ); next_turn = cart.next_turn }
    | ( 0,-1 ) -> { position = next_position; direction = (-1, 0 ); next_turn = cart.next_turn }
    | ( 0, 1 ) -> { position = next_position; direction = ( 1, 0 ); next_turn = cart.next_turn }
    | _ -> raise (Failure (Format.sprintf "Invalid cart turn cell '%c' for (%d, %d) -> (%d, %d)" next_cell (fst cart.position) (snd cart.position) (fst cart.direction) (snd cart.direction) )))
  | '+' -> change_direction cart next_position
  | _ -> raise (Failure (Format.sprintf "Invalid cart cell '%c' for (%d, %d) -> (%d, %d)" next_cell (fst cart.position) (snd cart.position) (fst cart.direction) (snd cart.direction) ))

let iterate_carts_until_collision grid carts =
  let cell_number (x, y) = y * 10000 + x in
  let sorted_carts = List.sort carts ~compare:(fun a b -> (cell_number a.position) - (cell_number b.position)) in
  let rec iterate_nth_cart (cart::carts) n = match n with
    | 0 -> iterate_cart grid cart :: carts
    | n -> cart :: (iterate_nth_cart carts (n - 1)) in
  List.fold_until (List.range 0 (List.length sorted_carts)) ~init:(sorted_carts) ~f:(fun carts n ->
    let new_carts = iterate_nth_cart carts n in
    let positions = List.map new_carts ~f:(fun cart -> cart.position) in
    if (List.contains_dup positions ~compare:(fun a b -> Bool.to_int(not (a = b)))) then
      Stop (new_carts, List.find_a_dup positions ~compare:(fun a b -> Bool.to_int(not (a = b))))
    else
      Continue new_carts
  ) ~finish:(fun carts -> (carts, None))

let iterate_carts_until_one_cart_remains grid carts =
  let cell_number (x, y) = y * 10000 + x in
  let sorted_carts = List.sort carts ~compare:(fun a b -> (cell_number a.position) - (cell_number b.position)) in
  let rec iterate_nth_cart (cart::carts) n = match n with
    | 0 -> iterate_cart grid cart :: carts
    | n -> cart :: (iterate_nth_cart carts (n - 1)) in
  List.fold sorted_carts ~init:(sorted_carts) ~f:(fun carts cart ->
    match List.findi carts ~f:(fun _ contender -> cart = contender) with
      | Some (idx, _) -> (
        let new_carts = iterate_nth_cart carts idx in
        let positions = List.map new_carts ~f:(fun cart -> cart.position) in
        let without_dupes = List.filter new_carts ~f:(fun cart -> (List.count positions ~f:((=) cart.position)) = 1) in
        if (List.length without_dupes) = 1 then
          without_dupes
        else
          without_dupes
        )
      | None -> carts
  )

let iterate_carts grid carts =
  List.sort carts ~compare:(fun { direction = (_, ay) } { direction = (_, by) } -> ay - by)
  |> List.map ~f:(iterate_cart grid)

let format_grid grid carts =
  let concat chars = String.concat ~sep:("") (List.map ~f:(Char.to_string) chars) in
  let grid_with_carts = List.fold carts ~init:(grid) ~f:(fun grid cart ->
    Grid.set grid cart.position '$') in
  List.map grid_with_carts ~f:(concat)
  |> String.concat ~sep:("\n")

let rec visualize grid carts from until =
  if from < until then (
    Printf.printf "%d\n%s" from (format_grid grid carts);
    visualize grid (iterate_carts grid carts) (from + 1) until
  )

let rec find_collision grid carts from until =
  if from < until then (
    let (carts, collision) = iterate_carts_until_collision grid carts in
    if is_some collision then (
      (carts, collision, from)
    ) else (
      (* Printf.printf "%s\n" (format_grid grid carts); *)
      find_collision grid carts (from + 1) until)
  ) else (carts, None, from)

let rec find_last_cart grid carts from until =
  if from < until then (
    let carts = iterate_carts_until_one_cart_remains grid carts in
    if (List.length carts) = 1 then (
      (List.hd carts, from)
    ) else (
      (* Printf.printf "%d\n" (List.length carts); *)
      (* Printf.printf "%s\n" (format_grid grid carts); *)
      find_last_cart grid carts (from + 1) until)
  ) else (None, from)

let step_1 filename =
  let grid = parse filename in
  let carts = extract_carts grid in
  let stripped_grid = strip_carts grid in
  match find_collision stripped_grid carts 0 20000 with
    | (_, Some collision, from) -> (
        Printf.printf "%s\n" (format_grid grid carts);
        Printf.printf "COLLISION FOUND: %d, %d (gen: %d)\n" (fst collision) (snd collision) (from);
      )
    | _ -> raise (Failure "No collision found")

let step_2 filename =
  let grid = parse filename in
  let carts = extract_carts grid in
  let stripped_grid = strip_carts grid in
  match find_last_cart stripped_grid carts 0 50000 with
    | (Some last_cart, from) -> (
        Printf.printf "%s\n" (format_grid stripped_grid [last_cart]);
        Printf.printf "LAST CART FOUND: %d, %d (gen: %d)\n" (fst last_cart.position) (snd last_cart.position) (from);
      )
    | _ -> raise (Failure "No collision found")

let main =
  step_1 "13-real1.txt";
  step_2 "13-real1.txt";
