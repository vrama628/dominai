open! Core

module Cellar : sig
  type t = Card.t list [@@deriving yojson, sexp]
end

module Chapel : sig
  type t = Card.t list [@@deriving yojson, sexp]
end

module Workshop : sig
  type t = Card.t [@@deriving yojson, sexp]
end

module Moneylender : sig
  type t = bool [@@deriving yojson, sexp]
end

module Remodel : sig
  type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
end

module Mine : sig
  type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
end

module Artisan : sig
  type t = { trash : Card.t; topdeck : Card.t } [@@deriving yojson, sexp]
end

module Harbinger : sig
  type t = { card_to_topdeck : discard:Card.t list -> Card.t } [@@deriving sexp]
end

module Vassal : sig
  type 'play t = {
    play_card_from_topdeck : Game_state.t -> Card.t -> 'play option;
  }
  [@@deriving sexp]
end

module Poacher : sig
  type t = {
    cards_to_discard : number_to_discard:int -> hand:Card.t list -> Card.t list;
  }
  [@@deriving sexp]
end

module Library : sig
  type t = { skip_action_card : hand:Card.t list -> Card.t -> bool }
  [@@deriving sexp]
end

module Sentry : sig
  type t = {
    what_to_do :
      hand:Card.t list ->
      top_two_cards:Card.t list ->
      (Card.t * [ `Topdeck | `Trash | `Discard ]) list;
        [@sexp.opaque]
  }
  [@@deriving sexp]
end

type t =
  (* TREASURE CARDS *)
  | Copper
  | Silver
  | Gold
  (* ACTION CARDS *)
  | Cellar of Cellar.t
  | Chapel of Chapel.t
  | Moat
  | Merchant
  | Village
  | Workshop of Workshop.t
  | Bureaucrat
  | Militia
  | Moneylender of Moneylender.t
  | Remodel of Remodel.t
  | Smithy
  | Bandit
  | CouncilRoom
  | Festival
  | Laboratory
  | Market
  | Mine of Mine.t
  | Witch
  | Artisan of Artisan.t
  (* Require extra rpc calls *)
  | Harbinger of Harbinger.t
  | Vassal of t Vassal.t
  | Poacher of Poacher.t
  | ThroneRoom of t
  | Library of Library.t
  | Sentry of Sentry.t
[@@deriving sexp, yojson]

val to_card : t -> Card.t
val yojson_of_just_data : t -> Yojson.Safe.t
