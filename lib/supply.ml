open Base

type t = (Card.t, int, CardComparator.comparator_witness) Map.t

let initial_supply_of_card : Card.t -> int = function
  | Card.Gardens -> 12
  | _ -> 10

let create ~kingdom ~n_players : t =
  let copper =
    let initial_copper =
      if n_players > 4 then
        60
      else
        120
    in
    initial_copper - (7 * n_players)
  in
  let silver =
    if n_players > 4 then
      40
    else
      80
  in
  let gold =
    if n_players > 4 then
      30
    else
      60
  in
  let (estate as duchy) = match n_players with 2 -> 8 | _ -> 12 in
  let province =
    match n_players with
    | 2 -> 8
    | 3 | 4 -> 12
    | 5 -> 15
    | 6 -> 18
    | _ -> failwith "invalid number of players"
  in
  let curse =
    match n_players with
    | 2 -> 10
    | 3 -> 20
    | 4 -> 30
    | 5 -> 40
    | 6 -> 50
    | _ -> failwith "unreachable"
  in
  let kingdom_supply =
    List.map kingdom ~f:(fun card -> card, initial_supply_of_card card)
  in
  Map.of_alist_exn
    (module CardComparator)
    Card.(
      (Copper, copper)
      :: (Silver, silver)
      :: (Gold, gold)
      :: (Estate, estate)
      :: (Duchy, duchy)
      :: (Province, province)
      :: (Curse, curse)
      :: kingdom_supply
    )

let yojson_of_t (supply : t) : Yojson.Safe.t =
  `Assoc
    (Map.fold supply ~init:[] ~f:(fun ~key ~data acc ->
         (Card.yojson_of_t key |> Yojson.Safe.Util.to_string, `Int data) :: acc
     )
    )

let t_of_yojson (json : Yojson.Safe.t) : t =
  json
  |> Yojson.Safe.Util.to_assoc
  |> List.map ~f:(fun (card, amount) ->
         Card.t_of_yojson (`String card), Yojson.Safe.Util.to_int amount
     )
  |> Map.of_alist_exn (module CardComparator)

let take (card : Card.t) (supply : t) : t Errorable.t =
  let open Errorable in
  match Map.find supply card with
  | None -> error "Card %s not in kingdom." (Card.to_string card)
  | Some n when n > 0 -> return (Map.set supply ~key:card ~data:(n - 1))
  | _ -> error "No supply of %s remaining." (Card.to_string card)

let empty_piles : t -> int = Map.count ~f:(( = ) 0)

(** assumes 2-4 players *)
let game_is_over (supply : t) : bool =
  empty_piles supply >= 3 || Map.find_exn supply Card.Province <= 0
