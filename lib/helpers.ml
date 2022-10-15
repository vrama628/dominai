open Base

(** find_and_remove card cards =
  Some cards' where cards' = cards with one instance of card removed, if one exists
  OR None if there is no instance of card in cards
*)
let rec find_and_remove (card : Card.t) : Card.t list -> Card.t list option =
  function
  | [] -> None
  | x :: xs ->
    if Card.equal x card then
      Some xs
    else
      Option.map ~f:(fun cards' -> x :: cards') (find_and_remove card xs)

(** is_submultiset a b =
    Some b' if a is a submultiset of b, where b' is b with a removed
    None if a is not a submultiset of b
*)
let rec is_submultiset (a : Card.t list) (b : Card.t list) : Card.t list option
    =
  match a with
  | [] -> Some b
  | x :: xs -> (
    match find_and_remove x b with
    | None -> None
    | Some b' -> is_submultiset xs b'
  )
