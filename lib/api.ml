open Base

type data = Yojson.Safe.t
let data_of_yojson = Fn.id
let yojson_of_data = Fn.id

type player_to_game_request =
  | EndTurn of unit
  | Play of {
      card : Card.t;
      (* TODO: move data entirely to separate requests *)
      data : data;
    }
  | Buy of { card : Card.t }
[@@deriving of_yojson]

type turn_phase =
  | Action
  | Buy
[@@deriving yojson_of]

type turn_info = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
  trash : Trash.t;
  buys : int;
  actions : int;
  treasure : int;
  in_play : Card.t list;
  phase : turn_phase;
}
[@@deriving yojson_of]

type game_over_result =
  | Win
  | Lose
[@@deriving yojson_of]

let yojson_of_game_over_result (game_over_result : game_over_result) :
    Yojson.Safe.t =
  match yojson_of_game_over_result game_over_result with
  | `List [name] -> name
  | _ -> failwith "unreachable"

type game_to_player_notification =
  | StartTurn of turn_info
  | FatalError of { message : string }
  | GameOver of {
      result : game_over_result;
      scores : Scores.t;
    }
[@@deriving yojson_of]

type game_to_player_request =
  | StartGame of {
      kingdom : Card.t list;
      order : string list;
    }
  | Attack of {
      card : Card.t;
      data : data;
    }
  | Harbinger of { discard : Card.t list }
  | Vassal of { card : Card.t }
  | Poacher of {
      hand : Card.t list;
      empty_supply_piles : int;
    }
  | ThroneRoom of { card : Card.t }
  | Library of {
      card : Card.t;
      hand : Card.t list;
    }
  | Sentry of {
      hand : Card.t list;
      cards : Card.t list;
    }
[@@deriving yojson_of]

let method_and_params_of_json = function
  | `List [`String method_; `Assoc params] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let json_of_method_and_params ~method_ ~params =
  `List (`String method_ :: Option.to_list params)
