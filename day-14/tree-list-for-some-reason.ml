module TreeList = struct
  type ('a, 'b) early_returner = Stop of 'a | Continue of 'b
  type 'a tree = Node of ('a tree * 'a tree) | Leaf of 'a;;

  let nth list n =
    let rec nth_until list n = match list with
      | Node (a, b) -> (match nth_until a n with
        | Stop item -> Stop item
        | Continue n2 -> (match nth_until b n2 with
          | Stop item -> Stop item
          | Continue n3 -> Continue n3))
      | Leaf item when n = 0 -> Stop item
      | Leaf _ -> Continue (n - 1) in
    match nth_until list n with
      | Continue _ -> None
      | Stop item -> Some item
end
