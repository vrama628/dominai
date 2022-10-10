open! Core

type t =
  (* TREASURE CARDS *)
  | Copper
  | Silver
  | Gold
  (* VICTORY CARDS *)
  | Estate
  | Duchy
  | Province
  | Gardens
  (* CURSE CARD *)
  | Curse
  (* ACTION CARDS *)
  | Cellar
  | Chapel
  | Moat
  | Harbinger
  | Merchant
  | Vassal
  | Village
  | Workshop
  | Bureaucrat
  | Militia
  | Moneylender
  | Poacher
  | Remodel
  | Smithy
  | ThroneRoom
  | Bandit
  | CouncilRoom
  | Festival
  | Laboratory
  | Library
  | Market
  | Mine
  | Sentry
  | Witch
  | Artisan
[@@deriving yojson, sexp]

include Comparable.S with type t := t
include Stringable.S with type t := t

val is_action : t -> bool
val is_reaction : t -> bool
val is_victory : t -> bool
val is_treasure : t -> bool
val victory_points : t -> number_of_cards:int -> int
val treasure_value : t -> int
val cost : t -> int
