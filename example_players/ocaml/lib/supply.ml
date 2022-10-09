open Core

type t = int Card.Map.t [@@deriving sexp]

let yojson_of_t supply : Yojson.Safe.t =
  `Assoc
    (Map.fold supply ~init:[] ~f:(fun ~key ~data acc ->
         (Card.yojson_of_t key |> Yojson.Safe.Util.to_string, `Int data) :: acc))

let t_of_yojson (y : Yojson.Safe.t) : t =
  match y with
  | `Assoc alist ->
      List.map alist ~f:(fun (card, data) ->
          ( Card.of_string card,
            match data with
            | `Int data -> data
            | _ -> failwith "Malformed supply: card count not a number" ))
      |> Card.Map.of_alist_exn
  | _ -> failwith "Malformed supply: not an alist"
