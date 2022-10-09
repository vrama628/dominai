open! Core

module StartTurn = struct
  module Notification = struct
    type t = Game_state.t [@@deriving yojson, sexp]
  end
end

module StartGame = struct
  module Request = struct
    type t = { kingdom : Card.t list; order : string list }
    [@@deriving yojson, sexp] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = unit [@@deriving yojson]
  end
end

module Attack = struct
  module Request = struct
    type t = Attack.Request.t [@@deriving yojson]
  end

  module Response = struct
    type t = Attack.Response.t [@@deriving yojson]
  end
end

module Harbinger = struct
  module Request = struct
    type t = { discard : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = { card : Card.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

module Vassal = struct
  module Request = struct
    type t = { card : Card.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = { play : bool; data : Play.t option }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

module Poacher = struct
  module Request = struct
    type t = { hand : Card.t list; empty_supply_piles : int }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = { discard : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

module ThroneRoom = struct
  module Request = struct
    type t = { card : Card.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = Play.t [@@deriving yojson]

    let yojson_of_t t = Play.yojson_of_just_data t
  end
end

module Library = struct
  module Request = struct
    type t = { hand : Card.t list; card : Card.t }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = { skip : bool } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

module Sentry = struct
  module Request = struct
    type t = { hand : Card.t list; cards : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    module Placement = struct
      type t = { card : Card.t; placement : string }
      [@@deriving yojson] [@@yojson.allow_extra_fields]
    end

    type t = Placement.t list [@@deriving yojson]
  end
end

module GameOver = struct
  module Scores = struct
    type t = int String.Map.t

    let yojson_of_t t : Yojson.Safe.t =
      `Assoc
        (Map.fold t ~init:[] ~f:(fun ~key ~data acc -> (key, `Int data) :: acc))

    let t_of_yojson (y : Yojson.Safe.t) : t =
      match y with
      | `Assoc alist ->
          List.map alist ~f:(fun (player, data) ->
              ( player,
                match data with
                | `Int data -> data
                | _ -> failwith "Malformed score: player score not a number" ))
          |> String.Map.of_alist_exn
      | _ -> failwith "Malformed score: not an alist"
  end

  module Request = struct
    type t = { result : string; scores : Scores.t } [@@deriving yojson]
  end

  module Response = struct
    type t = { rematch : bool }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

module Play = struct
  module Request = struct
    type t = Play.t [@@deriving yojson]
  end

  module Response = struct
    type t = Game_state.t [@@deriving yojson]
  end
end

module Buy = struct
  module Request = struct
    type t = { card : Card.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Response = struct
    type t = Game_state.t [@@deriving yojson]
  end
end

module EndTurn = struct
  module Request = struct
    type t = unit [@@deriving yojson]
  end

  module Response = struct
    type t = {
      hand : Card.t list;
      discard : int;
      deck : int;
      supply : Supply.t;
    }
    [@@deriving yojson, sexp] [@@yojson.allow_extra_fields]
  end
end
