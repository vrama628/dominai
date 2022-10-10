open! Core

module StartTurn : sig
  module Notification : sig
    type t = Game_state.t [@@deriving yojson, sexp]
  end
end

module StartGame : sig
  module Request : sig
    type t = { kingdom : Card.t list; order : string list }
    [@@deriving yojson, sexp]
  end

  module Response : sig
    type t = unit [@@deriving yojson]
  end
end

module Attack : sig
  module Request : sig
    type t = Attack.Request.t [@@deriving yojson]
  end

  module Response : sig
    type t = Attack.Response.t [@@deriving yojson]
  end
end

module Harbinger : sig
  module Request : sig
    type t = { discard : Card.t list } [@@deriving yojson]
  end

  module Response : sig
    type t = { card : Card.t } [@@deriving yojson]
  end
end

module Vassal : sig
  module Request : sig
    type t = { card : Card.t } [@@deriving yojson]
  end

  module Response : sig
    type t = { play : bool; data : Play.t option } [@@deriving yojson]
  end
end

module Poacher : sig
  module Request : sig
    type t = { hand : Card.t list; empty_supply_piles : int }
    [@@deriving yojson]
  end

  module Response : sig
    type t = { discard : Card.t list } [@@deriving yojson]
  end
end

module ThroneRoom : sig
  module Request : sig
    type t = { card : Card.t } [@@deriving yojson]
  end

  module Response : sig
    type t = Play.t [@@deriving yojson]
  end
end

module Library : sig
  module Request : sig
    type t = { hand : Card.t list; card : Card.t } [@@deriving yojson]
  end

  module Response : sig
    type t = { skip : bool } [@@deriving yojson]
  end
end

module Sentry : sig
  module Request : sig
    type t = { hand : Card.t list; cards : Card.t list } [@@deriving yojson]
  end

  module Response : sig
    module Placement : sig
      (* placement = "trash" | "topdeck" | "discard" *)
      type t = { card : Card.t; placement : string } [@@deriving yojson]
    end

    type t = Placement.t list [@@deriving yojson]
  end
end

module GameOver : sig
  module Request : sig
    type t = {
      (* result = "Win" | "Lose" *)
      result : string;
      scores : int String.Map.t;
    }
    [@@deriving yojson]
  end

  module Response : sig
    type t = { rematch : bool } [@@deriving yojson]
  end
end

module Play : sig
  module Request : sig
    type t = Play.t [@@deriving yojson]
  end

  module Response : sig
    type t = Game_state.t [@@deriving yojson]
  end
end

module Buy : sig
  module Request : sig
    type t = { card : Card.t } [@@deriving yojson]
  end

  module Response : sig
    type t = Game_state.t [@@deriving yojson]
  end
end

module EndTurn : sig
  module Request : sig
    type t = unit [@@deriving yojson]
  end

  module Response : sig
    type t = {
      hand : Card.t list;
      discard : int;
      deck : int;
      supply : Supply.t;
    }
    [@@deriving yojson, sexp]
  end
end
