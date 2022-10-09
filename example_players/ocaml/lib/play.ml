open Core

module Cellar = struct
  type t = Card.t list [@@deriving yojson, sexp]
end

module Chapel = struct
  type t = Card.t list [@@deriving yojson, sexp]
end

module Workshop = struct
  type t = Card.t [@@deriving yojson, sexp]
end

module Moneylender = struct
  type t = bool [@@deriving yojson, sexp]
end

module Remodel = struct
  type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
end

module Mine = struct
  type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
end

module Artisan = struct
  type t = { trash : Card.t; topdeck : Card.t } [@@deriving yojson, sexp]
end

module Harbinger = struct
  type t = { card_to_topdeck : discard:Card.t list -> Card.t [@sexp.opaque] }
  [@@deriving sexp]
end

module Vassal = struct
  type 'play t = {
    play_card_from_topdeck : Game_state.t -> Card.t -> 'play option;
        [@sexp.opaque]
  }
  [@@deriving sexp]
end

module Poacher = struct
  type t = {
    cards_to_discard : number_to_discard:int -> hand:Card.t list -> Card.t list;
        [@sexp.opaque]
  }
  [@@deriving sexp]
end

module Library = struct
  type t = {
    skip_action_card : hand:Card.t list -> Card.t -> bool; [@sexp.opaque]
  }
  [@@deriving sexp]
end

module Sentry = struct
  type t = {
    what_to_do :
      hand:Card.t list ->
      top_two_cards:Card.t list ->
      (Card.t * [ `Topdeck | `Trash | `Discard ]) list;
        [@sexp.opaque]
  }
  [@@deriving sexp]
end

module No_data = struct
  type t = { card : Card.t } [@@deriving yojson]
end

module With_data = struct
  type 'data t = { card : Card.t; data : 'data } [@@deriving yojson]
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
[@@deriving sexp]

let to_card = function
  | Copper -> Card.Copper
  | Silver -> Silver
  | Gold -> Gold
  | Cellar _ -> Cellar
  | Chapel _ -> Chapel
  | Moat -> Moat
  | Harbinger _ -> Harbinger
  | Merchant -> Merchant
  | Vassal _ -> Vassal
  | Village -> Village
  | Workshop _ -> Workshop
  | Bureaucrat -> Bureaucrat
  | Militia -> Militia
  | Moneylender _ -> Moneylender
  | Poacher _ -> Poacher
  | Remodel _ -> Remodel
  | Smithy -> Smithy
  | ThroneRoom _ -> ThroneRoom
  | Bandit -> Bandit
  | CouncilRoom -> CouncilRoom
  | Festival -> Festival
  | Laboratory -> Laboratory
  | Library _ -> Library
  | Market -> Market
  | Mine _ -> Mine
  | Sentry _ -> Sentry
  | Witch -> Witch
  | Artisan _ -> Artisan

let rec yojson_of_with_data = function
  | ( Copper | Silver | Gold | Moat | Harbinger _ | Merchant | Vassal _
    | Village | Bureaucrat | Militia | Poacher _ | Smithy | Bandit | CouncilRoom
    | Festival | Laboratory | Library _ | Market | Sentry _ | Witch ) as card ->
      With_data.yojson_of_t yojson_of_unit { card = to_card card; data = () }
  | card -> yojson_of_t card

and yojson_of_t card =
  match card with
  | Copper | Silver | Gold | Moat | Harbinger _ | Merchant | Vassal _ | Village
  | Bureaucrat | Militia | Poacher _ | Smithy | Bandit | CouncilRoom | Festival
  | Laboratory | Library _ | Market | Sentry _ | Witch ->
      No_data.yojson_of_t { card = to_card card }
  | Cellar request ->
      With_data.yojson_of_t Cellar.yojson_of_t
        { With_data.card = to_card card; data = request }
  | Chapel request ->
      With_data.yojson_of_t Chapel.yojson_of_t
        { card = to_card card; data = request }
  | Workshop request ->
      With_data.yojson_of_t Workshop.yojson_of_t
        { card = to_card card; data = request }
  | Moneylender request ->
      With_data.yojson_of_t Moneylender.yojson_of_t
        { card = to_card card; data = request }
  | Remodel request ->
      With_data.yojson_of_t Remodel.yojson_of_t
        { card = to_card card; data = request }
  | ThroneRoom request ->
      With_data.yojson_of_t yojson_of_with_data
        { card = to_card card; data = request }
  | Mine request ->
      With_data.yojson_of_t Mine.yojson_of_t
        { card = to_card card; data = request }
  | Artisan request ->
      With_data.yojson_of_t Artisan.yojson_of_t
        { card = to_card card; data = request }

let t_of_yojson _ = failwith "Cannot parse Play.t"

type just_data = t [@@deriving yojson]

let yojson_of_just_data t =
  match yojson_of_with_data t with
  | `Assoc data ->
      `Assoc
        (List.filter data ~f:(fun (key, _) -> not (String.equal key "card")))
  | data -> data
