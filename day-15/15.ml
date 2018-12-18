#require "core.top";;
#require "core.syntax";;
#require "ppx_jane";;

open Core;;
open Core.List.Monad_infix;;

module Vector = struct
  type t = { x: int; y: int } [@@deriving sexp, compare]

  let add {x = ax; y = ay} {x = bx; y = by} = { x = ax + bx; y = ay + by }

  let distance {x = ax; y = ay} {x = bx; y = by} = (abs (ax - bx)) + (abs (ay - by))

  let make x y = { x = x; y = y }

  let cell_num vec size = vec.y * size.x + vec.x

  let neighbours vec = [
    add vec { x =  0; y = -1 };
    add vec { x = -1; y =  0 };
    add vec { x =  1; y =  0 };
    add vec { x =  0; y =  1 };
  ]

  let x vec = vec.x

  let y vec = vec.y
end

module Entity = struct
  type category = Goblin | Elf | Wall
  type entity = { id: int; position: Vector.t; category: category; health: int; attack_power: int; }

  let make id position category health attack_power =
    { id = id; position = position; category = category; health = health; attack_power = attack_power }

  let move entity position = make entity.id position entity.category entity.health entity.attack_power

  let move_by entity change = move entity (Vector.add entity.position change)

  let damage entity points = make entity.id entity.position entity.category (entity.health - points) entity.attack_power

  let category entity = entity.category

  let position entity = entity.position

  let attack_power entity = entity.attack_power

  let health entity = entity.health

  let same a b = a.id = b.id
end

module VectorSet = Set.Make(Vector)
module VectorMap = Map.Make(Vector)

(* Thanks wikipedia *)
let route (start: Vector.t) (goal: Vector.t) ~available:available_fn =
  let rec reconstruct_path came_from current =
    match (VectorMap.find came_from current) with
      | Some next -> current :: reconstruct_path came_from next
      | None -> [current]
    in
  let rec iterate open_set closed_set f_score g_score came_from =
    let current = VectorSet.fold open_set ~init:(None) ~f:(fun lowest item ->
      if is_none lowest then Some item else
      let f_score_for_item = VectorMap.find_exn f_score item in
      let f_score_for_lowest = VectorMap.find_exn f_score (Option.value_exn lowest) in
      if f_score_for_lowest < f_score_for_item then
        lowest
      else
        Some item) in
    if is_none current then None else
    let current = Option.value_exn current in
    if current = goal then Some (List.tl_exn @@ List.rev @@ reconstruct_path came_from current) else
    let open_set = VectorSet.remove open_set current in
    let closed_set = VectorSet.add closed_set current in
    let (open_set, came_from, g_score, f_score) = Vector.neighbours current
    |> List.filter ~f:(fun item -> available_fn item)
    |> List.fold ~init:((open_set, came_from, g_score, f_score)) ~f:(fun (open_set, came_from, g_score, f_score) neighbour ->
      if VectorSet.exists closed_set ~f:((=) neighbour) then
        (open_set, came_from, g_score, f_score)
      else
        let tentative_g_score = (VectorMap.find_exn g_score current) + 1 in
        if not (VectorSet.exists open_set ~f:((=) neighbour)) then
          let open_set = VectorSet.add open_set neighbour in
          let came_from = VectorMap.update came_from neighbour ~f:(fun _ -> current) in
          let g_score = VectorMap.update g_score neighbour ~f:(fun _ -> tentative_g_score) in
          let f_score = VectorMap.update f_score neighbour ~f:(fun _ ->
            (VectorMap.find_exn g_score neighbour) +
            Vector.distance neighbour goal
          ) in
          (open_set, came_from, g_score, f_score)
        else if tentative_g_score >= (VectorMap.find g_score neighbour |> Option.value ~default:(Int.max_value)) then
          (open_set, came_from, g_score, f_score)
        else
          let came_from = VectorMap.update came_from neighbour ~f:(fun _ -> current) in
          let g_score = VectorMap.update g_score neighbour ~f:(fun _ -> tentative_g_score) in
          let f_score = VectorMap.update f_score neighbour ~f:(fun _ ->
            (VectorMap.find_exn g_score neighbour) +
            Vector.distance neighbour goal
          ) in
          (open_set, came_from, g_score, f_score)
    ) in
    iterate open_set closed_set f_score g_score came_from

  in
  let closed_set = VectorSet.empty in
  let open_set = VectorSet.add (VectorSet.empty) start in
  let came_from = VectorMap.empty in
  let g_score = VectorMap.empty in
  let f_score = VectorMap.empty in
  let g_score = VectorMap.add_exn g_score ~key:start ~data:(0) in
  let f_score = VectorMap.add_exn f_score ~key:start ~data:(Vector.distance start goal) in
  iterate open_set closed_set f_score g_score came_from

module World = struct
  type world = { size : Vector.t; entities : Entity.entity list; closed: int option }

  let make size entities = { size = size; entities = entities; closed = None }

  let get world coord =
    world.entities
    |> List.find ~f:(fun entity ->
      (Entity.position entity) = coord
    )

  let size world = world.size

  let refresh_unit world unit = world.entities |> List.find ~f:(Entity.same unit)

  let sorted_entities world =
    let appraise a = (Vector.cell_num (Entity.position a) world.size) in
    world.entities |> List.sort ~compare:(fun a b ->
      (appraise a) - (appraise b)
    )

  let units world =
    sorted_entities world
    |> List.filter ~f:(fun entity -> match Entity.category entity with
      | Entity.Goblin -> true
      | Entity.Elf -> true
      | _ -> false
    )

  let empty world coord = is_none (get world coord)

  let enemies world unit =
    units world
    |> List.filter ~f:(fun entity -> (Entity.category unit) <> (Entity.category entity))

  let fold_units world ~f:f : world =
    List.fold (units world) ~init:(world) ~f:(fun interstitial_world unit ->
      f interstitial_world (refresh_unit interstitial_world unit)
    )

  let attack world unit target =
    if Entity.health target < Entity.attack_power unit then
      List.filter world.entities ~f:((<>) target)
      |> make world.size
    else
      Entity.damage target (Entity.attack_power unit)
      :: List.filter world.entities ~f:((<>) target)
      |> make world.size

  let move_towards world unit coord =
    let neighbour_routes =
      Vector.neighbours (Entity.position unit)
      |> List.filter ~f:(fun vec -> empty world vec)
      |> List.map ~f:(fun neighbour -> (neighbour, route neighbour coord ~available:(empty world)))
      |> List.filter ~f:(fun (_, route) -> is_some route)
      |> List.map ~f:(fun (neighbour, route) -> (neighbour, Option.value_exn route)) in
    let minimum_length = neighbour_routes |> List.map ~f:(fun (_, route) -> List.length route) |> List.min_elt ~compare |> Option.value ~default:0 in
    let candidates = List.filter neighbour_routes ~f:(fun (_, route) -> (List.length route) = minimum_length) in
    List.hd candidates
    |> Option.map ~f:(fun (step, _) -> ((Entity.move unit step) :: (List.filter world.entities ~f:((<>) unit))))
    |> Option.map ~f:(make world.size)
    |> Option.value ~default:(world)

  let close world rounds =
     { size = world.size; entities = world.entities; closed = Some rounds }

  let closed world = world.closed
end

module Engine = struct
  let best_attack_target world unit =
    let neighbours = Vector.neighbours (Entity.position unit) in
    let enemies = World.enemies world unit in
    let available_enemies = List.filter enemies ~f:(fun enemy ->
      List.exists neighbours ~f:(fun neighbour ->
        Entity.position enemy = neighbour)) in
    List.min_elt available_enemies ~compare:(fun a b -> (Entity.health a) - (Entity.health b))

  let best_move_target world unit =
    World.enemies world unit
    |> List.concat_map ~f:(fun enemy -> Vector.neighbours (Entity.position enemy))
    |> List.filter ~f:(fun vec -> World.empty world vec)
    >>| (fun vec -> (vec, route ~available:(World.empty world) (Entity.position unit) vec))
    |> List.filter ~f:(fun (vec, route) -> is_some route)
    |> List.map ~f:(fun (vec, route) -> (vec, Option.value_exn route))
    |> List.min_elt ~compare:(fun (_, route_a) (_, route_b) -> (List.length route_a) - (List.length route_b))
    |> Option.map ~f:(fst)
    (* Doesn't do tie breaking yet *)

  let is_game_over world =
    World.units world
    |> List.map ~f:(Entity.category)
    |> List.remove_consecutive_duplicates ~equal:(=)
    |> List.length
    |> (=) 1
end

let cell_to_category = function
  | '#' -> Some Entity.Wall
  | 'E' -> Some Entity.Elf
  | 'G' -> Some Entity.Goblin
  | _   -> None

let category_to_cell = function
  | Entity.Wall -> '#'
  | Entity.Elf -> 'E'
  | Entity.Goblin -> 'G'

let format world =
  List.fold (List.range 0 (World.size world).y) ~init:("") ~f:(fun acc y ->
    List.fold (List.range 0 (World.size world).x) ~init:(acc) ~f:(fun acc x ->
      acc ^ Char.to_string (
        World.get world (Vector.make x y)
        |> Option.map ~f:(fun entity -> category_to_cell (Entity.category entity))
        |> Option.value ~default:('.')
      )
    )  ^
    List.fold (World.units world) ~init:("   ") ~f:(fun acc entity ->
      if Vector.y (Entity.position entity) = y then
        acc ^ " " ^ (Char.to_string (category_to_cell (Entity.category entity))) ^ "(" ^ (Int.to_string (Entity.health entity)) ^ ")"
      else acc
    ) ^ "\n"
  )

let print world =
  ignore (Printf.printf "%s" (format world); Out_channel.flush stdout)

let load_map filename : World.world =
  let find_bounds grid =
    let width = (List.length @@ List.hd_exn grid) in
    let height = (List.length grid) in
    Vector.make width height in

  let grid = In_channel.read_lines filename >>| String.to_list in
  List.foldi grid ~init:([]) ~f:(fun row items line ->
    List.foldi line ~init:(items) ~f:(fun col items cell ->
      cell_to_category cell
      |> Option.map ~f:(fun category ->
        let id = (Vector.cell_num (Vector.make col row) (find_bounds grid)) in
        Entity.make id (Vector.make col row) category 200 3)
      |> Option.map ~f:(fun e -> e :: items)
      |> Option.value ~default:(items);
    )
  ) |> World.make (find_bounds grid)

let calculate_checksum world =
  ((Option.value_exn (World.closed world)) - 1) * (
      World.units world
      |> List.map ~f:(Entity.health)
      |> List.reduce_exn ~f:(+))

(* val run_round : World.world -> World.world *)
let run_round world n =
  World.fold_units world ~f:(fun world maybe_unit ->
    if Engine.is_game_over world then World.close world n else
    if is_none maybe_unit then world else
    let unit = Option.value_exn maybe_unit in
    let target = Engine.best_attack_target world unit in
    if is_some target then
      World.attack world unit (Option.value_exn target)
    else
      let world = Engine.best_move_target world unit
      |> Option.map ~f:(World.move_towards world unit)
      |> Option.value ~default:(world) in
      let unit = World.refresh_unit world unit |> Option.value_exn in
      let world = Engine.best_attack_target world unit
      |> Option.map ~f:(World.attack world unit)
      |> Option.value ~default:(world) in
      world
  )

let run_rounds world n =
  print world; Out_channel.flush stdout;
  ignore (List.fold_until (List.range 1 (n + 1)) ~init:(world) ~f:(fun world n ->
    Printf.printf "\n== ROUND %d ==\n" n; Out_channel.flush stdout;
    let new_world = run_round world n in
    print new_world; Out_channel.flush stdout;
    if is_some (World.closed world) then (
      Printf.printf "Game over! Checksum: %d\n" (calculate_checksum world);
      Stop world
    ) else
      Continue new_world
  ) ~finish:(ident))
