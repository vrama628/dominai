open Base

type data = Yojson.Safe.t
let data_of_yojson = Fn.id
let yojson_of_data = Fn.id

type game_over_result =
  | Win
  | Lose
[@@deriving yojson, eq]

type player_to_game_request =
  | EndTurn of unit
  | Play of {
      card : Card.t;
      (* TODO: move data entirely to separate requests *)
      data : data;
    }
  | Buy of { card : Card.t }
[@@deriving yojson]

type turn_phase =
  | Action
  | Buy
[@@deriving yojson, eq]

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
[@@deriving yojson, eq]

let yojson_of_game_over_result (game_over_result : game_over_result) :
    Yojson.Safe.t =
  match yojson_of_game_over_result game_over_result with
  | `List [name] -> name
  | _ -> failwith "unreachable"

let game_over_result_of_yojson (json : Yojson.Safe.t) : game_over_result =
  game_over_result_of_yojson (`List [json])

type game_to_player_notification =
  | StartTurn of turn_info
  | FatalError of { message : string }
[@@deriving yojson]

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
  | GameOver of {
      result : game_over_result;
      scores : Scores.t;
    }
[@@deriving yojson]

let method_and_params_of_json = function
  | `List [`String method_; `Assoc params] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let json_of_method_and_params ~method_ ~(params : Yojson.Safe.t option) :
    Yojson.Safe.t =
  `List (`String method_ :: Option.to_list params)

module GameToPlayerRequest = struct
  module Attack = struct
    module Bandit = struct
      type t = Card.t list [@@deriving yojson_of]
    end
  end
end

module GameToPlayerResponse = struct
  module EndTurn = struct
    type t = {
      hand : Card.t list;
      discard : int;
      deck : int;
      supply : Supply.t;
    }
    [@@deriving yojson_of]
  end
  module Play = struct
    type t = turn_info [@@deriving yojson_of]
  end
  module Buy = struct
    type t = turn_info [@@deriving yojson_of]
  end
end

module PlayerToGameResponse = struct
  module Attack = struct
    type t = {
      reaction : Card.t option; [@yojson.option]
      data : data option; [@yojson.option]
    }
    [@@deriving of_yojson]

    module Bureaucrat = struct
      type t =
        | Reveal
        | VictoryCard of Card.t

      let t_of_yojson : data -> t = function
        | `String "reveal" -> Reveal
        | card -> VictoryCard (Card.t_of_yojson card)
    end
    module Militia = struct
      type t = Card.t list [@@deriving of_yojson]
    end
    module Bandit = struct
      type t = Card.t option [@@deriving of_yojson]
    end
  end
  module Harbinger = struct
    type t = { card : Card.t } [@@deriving of_yojson]
  end
  module Vassal = struct
    type t = {
      play : bool;
      data : data;
    }
    [@@deriving of_yojson]
  end
  module Poacher = struct
    type t = { discard : Card.t list } [@@deriving of_yojson]
  end
  module ThroneRoom = struct
    type t = { data : data } [@@deriving of_yojson]
  end
  module Library = struct
    type t = { skip : bool } [@@deriving of_yojson]
  end
  module Sentry = struct
    type placement =
      | Trash
      | Discard
      | Topdeck
    let placement_of_yojson = function
      | `String "trash" -> Trash
      | `String "discard" -> Discard
      | `String "topdeck" -> Topdeck
      | json ->
        let e =
          Failure "`do` must be one of \"trash\", \"discard\", or \"topdeck\"."
        in
        raise (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, json))

    type card_placement = {
      card : Card.t;
      placement : placement;
    }
    [@@deriving of_yojson]

    type t = card_placement list [@@deriving of_yojson]
  end
  module GameOver = struct
    type t = { rematch : bool } [@@deriving of_yojson]
  end
end

module Play = struct
  module Cellar = struct
    type t = Card.t list [@@deriving of_yojson]
  end
  module Chapel = struct
    type t = Card.t list [@@deriving of_yojson]
  end
  module Workshop = struct
    type t = Card.t [@@deriving of_yojson]
  end
  module Moneylender = struct
    type t = bool [@@deriving of_yojson]
  end
  module Remodel = struct
    type t = {
      trash : Card.t;
      gain : Card.t;
    }
    [@@deriving of_yojson]
  end
  module ThroneRoom = struct
    type t = {
      card : Card.t;
      data : data;
    }
    [@@deriving of_yojson]
  end
  module Mine = struct
    type t = {
      trash : Card.t;
      gain : Card.t;
    }
    [@@deriving of_yojson]
  end
  module Artisan = struct
    type t = {
      gain : Card.t;
      topdeck : Card.t;
    }
    [@@deriving of_yojson]
  end
end
