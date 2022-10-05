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
[@@deriving yojson, ord, sexp, eq]

(* hack around ppx_yojson_conv's weird json translation of enums *)

let yojson_of_t card =
  match yojson_of_t card with
  | `List [name] -> name
  | _ -> failwith "unreachable"

let t_of_yojson json = t_of_yojson (`List [json])

let to_string (card : t) : string =
  card |> yojson_of_t |> Yojson.Safe.Util.to_string

let is_action = function
  | Copper | Silver | Gold | Estate | Duchy | Province | Gardens | Curse ->
    false
  | Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village | Workshop
  | Bureaucrat | Militia | Moneylender | Poacher | Remodel | Smithy | ThroneRoom
  | Bandit | CouncilRoom | Festival | Laboratory | Library | Market | Mine
  | Sentry | Witch | Artisan ->
    true

let is_reaction = function Moat -> true | _ -> false

let is_victory = function
  | Estate | Duchy | Province | Gardens -> true
  | _ -> false

let is_treasure = function Copper | Silver | Gold -> true | _ -> false

let cost = function
  | Copper -> 0
  | Silver -> 3
  | Gold -> 6
  | Estate -> 2
  | Duchy -> 5
  | Province -> 8
  | Gardens -> 4
  | Curse -> 0
  | Cellar -> 2
  | Chapel -> 2
  | Moat -> 2
  | Harbinger -> 3
  | Merchant -> 3
  | Vassal -> 3
  | Village -> 3
  | Workshop -> 3
  | Bureaucrat -> 4
  | Militia -> 4
  | Moneylender -> 4
  | Poacher -> 4
  | Remodel -> 4
  | Smithy -> 4
  | ThroneRoom -> 4
  | Bandit -> 5
  | CouncilRoom -> 5
  | Festival -> 5
  | Laboratory -> 5
  | Library -> 5
  | Market -> 5
  | Mine -> 5
  | Sentry -> 5
  | Witch -> 5
  | Artisan -> 6
