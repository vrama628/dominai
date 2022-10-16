open Core
open Dominai

module Errorable = struct
  type 'a errorable = ('a, Jsonrpc.Response.Error.t) result Lwt.t

  let return (x : 'a) : 'a errorable = Lwt.return (Ok x)

  let error (fmt : ('r, unit, string, 'a errorable) format4) : 'r =
    Printf.ksprintf
      (fun message ->
        Lwt.return
          (Error
             Jsonrpc.Response.Error.(make ~code:Code.InvalidRequest ~message ())
          )
      )
      fmt

  let ensure
      (condition : bool)
      (fmt : ('r, unit, string, unit errorable) format4) : 'r =
    Printf.ksprintf
      (fun message ->
        Lwt.return
          ( if condition then
            Ok ()
          else
            Error
              Jsonrpc.Response.Error.(make ~code:Code.InvalidRequest ~message ())
          )
      )
      fmt

  module Let_syntax = struct
    let bind (x : 'a errorable) ~(f : 'a -> 'b errorable) : 'b errorable =
      match%lwt x with Ok y -> f y | Error e -> Lwt.return (Error e)
  end
end
open Errorable

type data = Yojson.Safe.t
let data_of_yojson = Fn.id
let yojson_of_data = Fn.id

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

let parse (t_of_yojson : Yojson.Safe.t -> 'a) (data : Yojson.Safe.t) :
    'a errorable =
  try return (t_of_yojson data)
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, json) ->
    error
      "Invalid format of data field:\ndata: %s\nerror: %s"
      (Yojson.Safe.to_string json)
      (Exn.to_string e)

module CardComparator = struct
  type t = Card.t

  (* what broken nonsense is this jane street *)
  include Comparator.Make (Card)
end

module Supply = struct
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
           (Card.yojson_of_t key |> Yojson.Safe.Util.to_string, `Int data)
           :: acc
       )
      )

  let take (card : Card.t) (supply : t) : t errorable =
    match Map.find supply card with
    | None -> error "Card %s not in kingdom." (Card.to_string card)
    | Some n when n > 0 -> return (Map.set supply ~key:card ~data:(n - 1))
    | _ -> error "No supply of %s remaining." (Card.to_string card)

  let empty_piles : t -> int = Map.count ~f:(( = ) 0)

  (** assumes 2-4 players *)
  let game_is_over (supply : t) : bool =
    empty_piles supply >= 3 || Map.find_exn supply Card.Province <= 0
end

type turn_phase =
  | Action
  | Buy
[@@deriving yojson_of]

type trash = Card.t list [@@deriving yojson_of]

type turn_info = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
  trash : trash;
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

type scores = (string * int) list

let yojson_of_scores (scores : scores) : Yojson.Safe.t =
  `Assoc (List.Assoc.map ~f:(fun score -> `Int score) scores)

type game_to_player_notification =
  | StartTurn of turn_info
  | FatalError of { message : string }
[@@deriving yojson_of]

(* TODO: move into module *)
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
      scores : scores;
    }
[@@deriving yojson_of]

module GameToPlayerRequest = struct
  module Attack = struct
    module Bandit = struct
      type t = Card.t list [@@deriving yojson_of]
    end
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

type player_to_game_request =
  | EndTurn of unit
  | Play of {
      card : Card.t;
      (* TODO: move data entirely to separate requests *)
      data : data; [@default `Null]
    }
  | Buy of { card : Card.t }
[@@deriving of_yojson]

let method_and_params_of_json = function
  | `List [`String method_; `Assoc params] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let json_of_method_and_params ~method_ ~params =
  `List [`String method_; Option.value params ~default:`Null]

let shuffle (list : 'a list) : 'a list =
  let tagged = List.map ~f:(fun x -> Random.bits (), x) list in
  let sorted =
    List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) tagged
  in
  List.map ~f:snd sorted

(** find_and_remove card cards =
  Some cards' where cards' = cards with one instance of card removed, if one exists
  OR None if there is no instance of card in cards
*)
let rec find_and_remove (card : Card.t) : Card.t list -> Card.t list option =
  function
  | [] -> None
  | x :: xs ->
    if Card.equal x card then
      Some xs
    else
      Option.map ~f:(fun cards' -> x :: cards') (find_and_remove card xs)

(** is_submultiset a b =
    Some b' if a is a submultiset of b, where b' is b with a removed
    None if a is not a submultiset of b
*)
let rec is_submultiset (a : Card.t list) (b : Card.t list) : Card.t list option
    =
  match a with
  | [] -> Some b
  | x :: xs -> (
    match find_and_remove x b with
    | None -> None
    | Some b' -> is_submultiset xs b'
  )

module Player = struct
  module Cards = struct
    type t = {
      deck : Card.t list;
      hand : Card.t list;
      discard : Card.t list;
    }

    let debug_str { deck; hand; discard } : string =
      Printf.sprintf
        "{ deck=%s; hand=%s; discard=%s }"
        (List.to_string ~f:Card.to_string deck)
        (List.to_string ~f:Card.to_string hand)
        (List.to_string ~f:Card.to_string discard)

    let take_from_deck (cards : t) : (Card.t * t) option =
      let cards =
        match cards with
        | { deck = []; hand; discard } ->
          let deck = shuffle discard in
          let discard = [] in
          { deck; hand; discard }
        | cards -> cards
      in
      match cards with
      | { deck = card :: deck; hand; discard } ->
        Some (card, { deck; hand; discard })
      | _ -> None

    let add_to_hand (card : Card.t) (cards : t) : t =
      { cards with hand = card :: cards.hand }

    let draw (cards : t) : t =
      match take_from_deck cards with
      | None -> cards
      | Some (card, cards) -> add_to_hand card cards

    let draw_n (n : int) : t -> t = Fn.apply_n_times ~n draw

    let clean_up ~(in_play : Card.t list) { deck; hand; discard } =
      let discard = in_play @ hand @ discard in
      let hand = [] in
      draw_n 5 { deck; hand; discard }

    let create () =
      let deck = [] in
      let hand = [] in
      let discard =
        List.init 3 ~f:(Fn.const Card.Estate)
        @ List.init 7 ~f:(Fn.const Card.Copper)
      in
      draw_n 5 { deck; hand; discard }

    let get_deck { deck; _ } : int = List.length deck

    let get_hand { hand; _ } : Card.t list = hand

    let get_discard { discard; _ } : int = List.length discard

    let remove_from_hand (to_remove : Card.t list) (cards : t) : t errorable =
      match is_submultiset to_remove cards.hand with
      | None ->
        error
          "Your hand %s does not contain all of the cards %s"
          ([%yojson_of: Card.t list] cards.hand |> Yojson.Safe.to_string)
          ([%yojson_of: Card.t list] to_remove |> Yojson.Safe.to_string)
      | Some hand -> return { cards with hand }

    let add_to_discard (to_discard : Card.t list) (cards : t) : t =
      { cards with discard = to_discard @ cards.discard }

    let topdeck (card : Card.t) (cards : t) =
      { cards with deck = card :: cards.deck }

    let all_cards { deck; hand; discard } : Card.t list = deck @ hand @ discard
  end

  type t = {
    name : string;
    websocket : Dream.websocket;
    resolver : unit Lwt.u;
    pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
    cards : Cards.t;
  }

  let reset (player : t) = { player with cards = Cards.create () }

  let log { name; cards; pending; _ } : unit =
    Dream.log
      "Player %s: Cards=%s, pending=%s"
      name
      (Cards.debug_str cards)
      (Hashtbl.keys pending |> List.to_string ~f:Int.to_string)

  module PublicState = struct
    type t = { name : string } [@@deriving yojson_of]
  end

  let yojson_of_t { name; _ } : Yojson.Safe.t =
    PublicState.({ name } |> yojson_of_t)

  let notify_websocket
      ~(websocket : Dream.websocket)
      (notification : game_to_player_notification) =
    let method_, params =
      notification
      |> yojson_of_game_to_player_notification
      |> method_and_params_of_json
    in
    Lwt.async (fun () ->
        Jsonrpc.Notification.create ~method_ ~params ()
        |> Jsonrpc.Notification.yojson_of_t
        |> Yojson.Safe.to_string
        |> Dream.send websocket
    )

  let notify { websocket; name; _ } (notification : game_to_player_notification)
      : unit =
    Dream.log
      "Player %s: Notifying %s"
      name
      (notification
      |> yojson_of_game_to_player_notification
      |> Yojson.Safe.to_string
      );
    notify_websocket ~websocket notification

  let disconnect_resolver ~(resolver : unit Lwt.u) = Lwt.wakeup resolver ()

  let disconnect ({ resolver; _ } : t) : unit = disconnect_resolver ~resolver

  (** notifies and disconnects the player *)
  let fatal_error_message
      ~(message : string)
      ~(websocket : Dream.websocket)
      ~(resolver : unit Lwt.u) : unit =
    notify_websocket ~websocket (FatalError { message });
    disconnect_resolver ~resolver

  let fatal_error
      (error : Jsonrpc.Response.Error.t)
      ({ websocket; resolver; name; _ } : t) : unit =
    Dream.log
      "Player %s Fatal Error: %s"
      name
      (error |> Jsonrpc.Response.Error.yojson_of_t |> Yojson.Safe.to_string);
    let Jsonrpc.Response.Error.{ message; _ } = error in
    fatal_error_message ~message ~websocket ~resolver

  let create
      ~(name : string)
      ~(websocket : Dream.websocket)
      ~(handler :
         player_to_game_request ->
         (Jsonrpc.Json.t, Jsonrpc.Response.Error.t) result Lwt.t
         )
      ~(on_disconnect : unit -> unit) : unit Lwt.t * t =
    let promise, resolver = Lwt.wait () in
    let pending = Hashtbl.create (module Int) in
    let rec listen () =
      match%lwt Dream.receive websocket with
      | None ->
        on_disconnect ();
        Lwt.return_unit
      | Some message ->
        Dream.log "Player %s sent %s" name message;
        begin
          match
            message |> Yojson.Safe.from_string |> Jsonrpc.Packet.t_of_yojson
          with
          | Jsonrpc.Packet.Response
              Jsonrpc.Response.{ id = `Int id; result = Ok json } ->
            let resolver =
              Option.value_exn (Hashtbl.find_and_remove pending id)
            in
            Lwt.wakeup_later resolver json
          | Jsonrpc.Packet.Request Jsonrpc.Request.{ id; method_; params } ->
            let request =
              json_of_method_and_params
                ~method_
                ~params:(Option.map params ~f:Jsonrpc.Structured.yojson_of_t)
              |> player_to_game_request_of_yojson
            in
            Lwt.async (fun () ->
                let%lwt result = handler request in
                let response =
                  match result with
                  | Ok json -> Jsonrpc.Response.ok id json
                  | Error error -> Jsonrpc.Response.error id error
                in
                Dream.log
                  "Player %s Response %s: %s"
                  name
                  (Jsonrpc.Id.yojson_of_t id |> Yojson.Safe.to_string)
                  (Jsonrpc.Response.yojson_of_t response
                  |> Yojson.Safe.to_string
                  );
                response
                |> Jsonrpc.Response.yojson_of_t
                |> Yojson.Safe.to_string
                |> Dream.send websocket
            )
          | unimplemented ->
            let unimplemented_msg =
              unimplemented
              |> Jsonrpc.Packet.yojson_of_t
              |> Yojson.Safe.pretty_to_string
            in
            Dream.log
              "Player %s sent unsupported JSONRPC packet: %s"
              name
              unimplemented_msg;
            let message =
              Printf.sprintf "Unsupported JSON-RPC message %s" unimplemented_msg
            in
            fatal_error_message ~message ~resolver ~websocket
          | exception Yojson.Json_error msg ->
            Dream.log "Player %s: error while parsing JSON: %s" name msg;
            let message = Printf.sprintf "Invalid JSON %s" msg in
            fatal_error_message ~message ~resolver ~websocket
          | exception exn ->
            Dream.log
              "Unrecognized error while parsing message: %s"
              (Exn.to_string_mach exn);
            let message =
              Printf.sprintf
                "Encountered error while parsing message: %s"
                (Exn.to_string_mach exn)
            in
            fatal_error_message ~message ~resolver ~websocket
        end;
        listen ()
    in
    Lwt.async listen;
    let cards = Cards.create () in
    promise, { name; websocket; pending; cards; resolver }

  let name { name; _ } : string = name

  let get_deck { cards; _ } : int = Cards.get_deck cards

  let get_hand { cards; _ } : Card.t list = Cards.get_hand cards

  let get_discard { cards; _ } : int = Cards.get_discard cards

  let add_to_hand (card : Card.t) (player : t) : t =
    { player with cards = Cards.add_to_hand card player.cards }

  let take_from_deck (player : t) : (Card.t * t) option =
    Option.map (Cards.take_from_deck player.cards) ~f:(fun (card, cards) ->
        card, { player with cards }
    )

  let draw_n (n : int) (player : t) : t =
    { player with cards = Cards.draw_n n player.cards }

  let clean_up ~(in_play : Card.t list) (player : t) : t =
    { player with cards = Cards.clean_up ~in_play player.cards }

  let remove_from_hand (to_remove : Card.t list) (player : t) : t errorable =
    let%bind cards = Cards.remove_from_hand to_remove player.cards in
    return { player with cards }

  let add_to_discard (to_discard : Card.t list) (player : t) : t =
    let cards = Cards.add_to_discard to_discard player.cards in
    { player with cards }

  let topdeck (card : Card.t) (player : t) : t =
    let cards = Cards.topdeck card player.cards in
    { player with cards }

  let fresh_id =
    let counter = ref 0 in
    fun () ->
      Ref.replace counter (( + ) 1);
      !counter

  let request { websocket; pending; name; _ } (request : game_to_player_request)
      : Yojson.Safe.t Lwt.t =
    Dream.log
      "Player %s: Notifying %s"
      name
      (request |> yojson_of_game_to_player_request |> Yojson.Safe.to_string);
    let method_, params =
      request |> yojson_of_game_to_player_request |> method_and_params_of_json
    in
    let promise, resolver = Lwt.wait () in
    let id =
      let key = fresh_id () in
      Hashtbl.add_exn pending ~key ~data:resolver;
      `Int key
    in
    Lwt.async (fun () ->
        Jsonrpc.Request.create ~id ~method_ ~params ()
        |> Jsonrpc.Request.yojson_of_t
        |> Yojson.Safe.to_string
        |> Dream.send websocket
    );
    promise

  let victory_points { cards; _ } : int =
    let all_cards = Cards.all_cards cards in
    let gardens_worth = List.length all_cards / 10 in
    let n_province = List.count all_cards ~f:Card.(equal Province) in
    let n_duchy = List.count all_cards ~f:Card.(equal Duchy) in
    let n_estate = List.count all_cards ~f:Card.(equal Estate) in
    let n_gardens = List.count all_cards ~f:Card.(equal Gardens) in
    let n_curse = List.count all_cards ~f:Card.(equal Curse) in
    (n_province * 6)
    + (n_duchy * 3)
    + n_estate
    + (n_gardens * gardens_worth)
    - n_curse
end

module TurnStatus = struct
  type t = {
    buys : int;
    actions : int;
    treasure : int;
    in_play : Card.t list;
    pending_merchants : int;
    phase : turn_phase;
  }

  let initial : t =
    let buys = 1 in
    let actions = 1 in
    let treasure = 0 in
    let in_play = [] in
    let pending_merchants = 0 in
    let phase = Action in
    { buys; actions; treasure; in_play; pending_merchants; phase }

  let add_buys (n : int) turn_status : t =
    { turn_status with buys = turn_status.buys + n }

  let add_actions (n : int) turn_status : t =
    { turn_status with actions = turn_status.actions + n }

  let add_treasure (n : int) turn_status : t =
    { turn_status with treasure = turn_status.treasure + n }

  let ensure_buy_phase (turn_status : t) : t = { turn_status with phase = Buy }

  (* TODO: roll into expend_treasure, and assert buy phase *)
  let expend_buy ~(cost : int) turn_status : t errorable =
    let turn_status = ensure_buy_phase turn_status in
    match turn_status.buys > 0, turn_status.treasure >= cost with
    | true, true ->
      return
        {
          turn_status with
          buys = turn_status.buys - 1;
          treasure = turn_status.treasure - cost;
        }
    | false, _ -> error "No buys left."
    | _, false -> error "Not enough treasure."

  let expend_action turn_status : t errorable =
    match turn_status.phase with
    | Action ->
      if turn_status.actions > 0 then
        return { turn_status with actions = turn_status.actions - 1 }
      else
        error "No actions left."
    | Buy -> error "Can't perform action in buy phase."

  let into_play (card : Card.t) (turn_status : t) : t =
    { turn_status with in_play = card :: turn_status.in_play }

  let pend_merchant (turn_status : t) : t =
    { turn_status with pending_merchants = turn_status.pending_merchants + 1 }

  let resolve_merchants (turn_status : t) : t =
    {
      turn_status with
      treasure = turn_status.treasure + turn_status.pending_merchants;
      pending_merchants = 0;
    }
end

(* TODO: explode back into the `turn` type *)
module CurrentPlayer = struct
  type t = {
    player : Player.t;
    turn_status : TurnStatus.t;
  }

  let yojson_of_t { player; turn_status = _ } : Yojson.Safe.t =
    Player.yojson_of_t player

  let name { player; _ } = Player.name player

  let turn_info
      {
        player;
        turn_status =
          TurnStatus.
            { buys; actions; treasure; in_play; phase; pending_merchants = _ };
      }
      ~(supply : Supply.t)
      ~(trash : trash) : turn_info =
    {
      hand = Player.get_hand player;
      discard = Player.get_discard player;
      deck = Player.get_deck player;
      supply;
      trash;
      buys;
      actions;
      treasure;
      in_play;
      phase;
    }

  (** moves card from hand into play
    * does NOT handle expending actions, adding treasure, etc *)
  let play_card (card : Card.t) { player; turn_status } : t errorable =
    let%bind player = Player.remove_from_hand [card] player in
    let turn_status = TurnStatus.into_play card turn_status in
    return { player; turn_status }

  (** Only used by Throne Room.
      Precondition: specified card is in play *)
  let unplay_card (card : Card.t) { player; turn_status } : t =
    let in_play = Option.value_exn (find_and_remove card turn_status.in_play) in
    let player = Player.add_to_hand card player in
    { player; turn_status = { turn_status with in_play } }

  (* destructor *)
  let clean_up { player; turn_status } : Player.t =
    let TurnStatus.{ in_play; _ } = turn_status in
    Player.clean_up ~in_play player
end
open CurrentPlayer

type kingdom_selection =
  | FirstGame
  | DeckTop
  | SleightOfHand
  | Engines
  | Random
[@@deriving yojson_of]

let kingdom_selection_of_string = function
  | "first_game" -> FirstGame
  | "deck_top" -> DeckTop
  | "sleight_of_hand" -> SleightOfHand
  | "engines" -> Engines
  | "random" -> Random
  | other ->
    Dream.log "Invalid kingdom selection %s" other;
    failwith "Invalid kingdom selection"

type turn = {
  kingdom : Card.t list;
  supply : Supply.t;
  trash : Card.t list;
  current_player : CurrentPlayer.t;
  next_players : Player.t list;
  kingdom_selection : kingdom_selection;
}
[@@deriving yojson_of]

type state =
  | PreStart of {
      num_players : int;
      kingdom_selection : kingdom_selection;
      players : Player.t list;
    }
  | Turn of turn
[@@deriving yojson_of]

type t = {
  state : state React.signal;
  set : state -> unit;
}

let yojson_of_t { state; set = _ } = yojson_of_state @@ React.S.value @@ state

let randomizer_cards =
  let open Card in
  [
    Cellar;
    Chapel;
    Moat;
    Harbinger;
    Merchant;
    Vassal;
    Village;
    Workshop;
    Bureaucrat;
    Gardens;
    Militia;
    Moneylender;
    Poacher;
    Remodel;
    Smithy;
    ThroneRoom;
    Bandit;
    CouncilRoom;
    Festival;
    Laboratory;
    Library;
    Market;
    Mine;
    Sentry;
    Witch;
    Artisan;
  ]

let start_turn
    ~(game : t)
    ~(kingdom : Card.t list)
    ~(supply : Supply.t)
    ~(trash : Card.t list)
    ~(player : Player.t)
    ~(next_players : Player.t list)
    ~(kingdom_selection : kingdom_selection) : unit Lwt.t =
  Dream.log "Player %s starting turn" player.name;
  Player.log player;
  let current_player =
    let turn_status = TurnStatus.initial in
    CurrentPlayer.{ player; turn_status }
  in
  game.set
    (Turn
       {
         kingdom;
         supply;
         trash;
         current_player;
         next_players;
         kingdom_selection;
       }
    );
  Player.notify
    player
    (StartTurn (CurrentPlayer.turn_info current_player ~supply ~trash));
  Lwt.return_unit

(* PRECONDITION: between 2 and 4 players *)
let start_game
    ~(game : t)
    ~(kingdom_selection : kingdom_selection)
    ~(players : Player.t list) : unit Lwt.t =
  let player, next_players =
    match shuffle players with p :: ps -> p, ps | _ -> failwith "unreachable"
  in
  let kingdom =
    match kingdom_selection with
    | FirstGame ->
      Card.
        [
          Cellar;
          Market;
          Merchant;
          Militia;
          Mine;
          Moat;
          Remodel;
          Smithy;
          Village;
          Workshop;
        ]
    | DeckTop ->
      Card.
        [
          Artisan;
          Bureaucrat;
          CouncilRoom;
          Festival;
          Harbinger;
          Laboratory;
          Moneylender;
          Sentry;
          Vassal;
          Village;
        ]
    | SleightOfHand ->
      Card.
        [
          Cellar;
          CouncilRoom;
          Festival;
          Gardens;
          Library;
          Harbinger;
          Militia;
          Poacher;
          Smithy;
          ThroneRoom;
        ]
    | Engines ->
      Card.
        [
          Chapel;
          Moat;
          Vassal;
          Village;
          Militia;
          Smithy;
          Festival;
          Laboratory;
          Library;
          Witch;
        ]
    | Random -> List.take (shuffle randomizer_cards) 10
  in
  let supply = Supply.create ~kingdom ~n_players:(List.length players) in
  let trash = [] in
  let order = List.map (player :: next_players) ~f:Player.name in
  let%lwt _ =
    Lwt.all
      (List.map players ~f:(fun player ->
           Player.request player (StartGame { kingdom; order })
       )
      )
  in
  start_turn
    ~game
    ~kingdom
    ~supply
    ~trash
    ~player
    ~next_players
    ~kingdom_selection

let game_over ~game ~kingdom_selection ~(players : Player.t list) : unit Lwt.t =
  let scores =
    List.map players ~f:(fun player ->
        Player.name player, Player.victory_points player
    )
  in
  let highest_score =
    scores
    |> List.map ~f:snd
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let%lwt responses =
    Lwt.all
      (List.map players ~f:(fun player ->
           let result =
             if Player.victory_points player = highest_score then
               Win
             else
               Lose
           in
           let%lwt response =
             Player.request player (GameOver { scores; result })
           in
           let%bind PlayerToGameResponse.GameOver.{ rematch } =
             parse PlayerToGameResponse.GameOver.t_of_yojson response
           in
           return rematch
       )
      )
  in
  if
    List.for_all responses ~f:(fun response ->
        match response with Ok true -> true | _ -> false
    )
  then
    let players = List.map players ~f:Player.reset in
    start_game ~game ~kingdom_selection ~players
  else (
    List.iter players ~f:(fun player -> Player.disconnect player);
    Lwt.return_unit
  )

type end_turn_response = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
}
[@@deriving yojson_of]

let end_turn ~(game : t) ~(name : string) : end_turn_response errorable =
  match React.S.value game.state with
  | Turn
      {
        current_player;
        next_players = next_player :: next_players;
        kingdom;
        supply;
        trash;
        kingdom_selection;
        _;
      }
    when String.equal (CurrentPlayer.name current_player) name ->
    let prev_player = CurrentPlayer.clean_up current_player in
    Dream.log "Player %s just ended their turn" name;
    Player.log prev_player;
    let end_turn_response =
      let hand = Player.get_hand prev_player in
      let discard = Player.get_discard prev_player in
      let deck = Player.get_deck prev_player in
      { hand; discard; deck; supply }
    in
    let next_players = next_players @ [prev_player] in
    if Supply.game_is_over supply then
      Lwt.async (fun () ->
          game_over
            ~kingdom_selection
            ~game
            ~players:(next_player :: next_players)
      )
    else
      Lwt.async (fun () ->
          start_turn
            ~game
            ~kingdom
            ~supply
            ~trash
            ~player:next_player
            ~next_players
            ~kingdom_selection
      );
    Lwt.return (Ok end_turn_response)
  | _ -> error "It is not your turn."

let react ~(player : Player.t) ~(card : Card.t) : unit errorable =
  if Card.is_reaction card then
    let%bind _ = Player.remove_from_hand [card] player in
    return ()
  else
    error
      "Cannot react with %s; it is not a reaction card."
      (Card.to_string card)

type play_response = turn_info [@@deriving yojson_of]

let rec play_card ~(turn : turn) ~(card : Card.t) ~(data : data) :
    turn errorable =
  let%bind current_player = CurrentPlayer.play_card card turn.current_player in
  match card with
  (* we could error when people try to play victory cards but we'll just noop *)
  | Card.Estate | Card.Duchy | Card.Province | Card.Gardens | Card.Curse ->
    error "You cannot play victory cards."
  | Card.Copper ->
    let turn_status =
      current_player.turn_status
      |> TurnStatus.ensure_buy_phase
      |> TurnStatus.add_treasure 1
    in
    return { turn with current_player = { current_player with turn_status } }
  | Card.Silver ->
    let turn_status =
      current_player.turn_status
      |> TurnStatus.ensure_buy_phase
      |> TurnStatus.add_treasure 2
      |> TurnStatus.resolve_merchants
    in
    return { turn with current_player = { current_player with turn_status } }
  | Card.Gold ->
    let turn_status =
      current_player.turn_status
      |> TurnStatus.ensure_buy_phase
      |> TurnStatus.add_treasure 3
    in
    return { turn with current_player = { current_player with turn_status } }
  | Card.Cellar ->
    let%bind cards = parse Play.Cellar.t_of_yojson data in
    let%bind turn_status =
      TurnStatus.expend_action current_player.turn_status
    in
    let%bind player = Player.remove_from_hand cards current_player.player in
    let player = Player.add_to_discard cards player in
    let player = Player.draw_n (List.length cards) player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    return { turn with current_player = { player; turn_status } }
  | Card.Chapel ->
    let%bind cards = parse Play.Chapel.t_of_yojson data in
    let%bind turn_status =
      TurnStatus.expend_action current_player.turn_status
    in
    let%bind player = Player.remove_from_hand cards current_player.player in
    let trash = cards @ turn.trash in
    let current_player = { player; turn_status } in
    return { turn with current_player; trash }
  | Card.Moat ->
    let%bind turn_status =
      TurnStatus.expend_action current_player.turn_status
    in
    let player = Player.draw_n 2 current_player.player in
    let current_player = { player; turn_status } in
    return { turn with current_player }
  | Card.Harbinger ->
    let%bind turn_status =
      TurnStatus.expend_action current_player.turn_status
    in
    let player = Player.draw_n 1 current_player.player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let Player.Cards.{ discard; _ } = player.cards in
    let%bind player =
      if List.is_empty discard then
        return player
      else
        let%lwt response = Player.request player (Harbinger { discard }) in
        let%bind PlayerToGameResponse.Harbinger.{ card } =
          parse PlayerToGameResponse.Harbinger.t_of_yojson response
        in
        match find_and_remove card discard with
        | None -> error "Card %s not in discard pile." (Card.to_string card)
        | Some discard ->
          let player = { player with cards = { player.cards with discard } } in
          return (Player.topdeck card player)
    in
    let current_player = { player; turn_status } in
    return { turn with current_player }
  | Card.Merchant ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 1 player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let turn_status = TurnStatus.pend_merchant turn_status in
    let current_player = { turn_status; player } in
    return { turn with current_player }
  | Card.Vassal -> (
    let { turn_status; player } = current_player in
    let turn_status = TurnStatus.add_treasure 2 turn_status in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    match Player.take_from_deck player with
    | None -> return { turn with current_player = { turn_status; player } }
    | Some (card, player) ->
      if Card.is_action card then
        let%lwt response = Player.request player (Vassal { card }) in
        let%bind PlayerToGameResponse.Vassal.{ play; data } =
          parse PlayerToGameResponse.Vassal.t_of_yojson response
        in
        if play then
          (* temporarily place card into hand and add action
             to make recursive call work *)
          let player = Player.add_to_hand card player in
          let turn_status = TurnStatus.add_actions 1 turn_status in
          let turn = { turn with current_player = { player; turn_status } } in
          play_card ~turn ~card ~data
        else
          let player = Player.add_to_discard [card] player in
          return { turn with current_player = { turn_status; player } }
      else
        let player = Player.add_to_discard [card] player in
        return { turn with current_player = { turn_status; player } }
  )
  | Card.Village ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 1 player in
    let turn_status = TurnStatus.add_actions 2 turn_status in
    return { turn with current_player = { player; turn_status } }
  | Card.Workshop ->
    let { player; turn_status } = current_player in
    let%bind card = parse Play.Workshop.t_of_yojson data in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind () =
      ensure
        (Card.cost card <= 4)
        "Card %s costs more than 4."
        (Card.to_string card)
    in
    let%bind supply = Supply.take card turn.supply in
    let player = Player.add_to_discard [card] player in
    return { turn with supply; current_player = { turn_status; player } }
  | Card.Bureaucrat ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind supply = Supply.take Card.Silver turn.supply in
    let player = Player.topdeck Card.Silver player in
    let%lwt next_players =
      Lwt_list.filter_map_p
        (fun player ->
          match%lwt
            let%lwt response =
              Player.request
                player
                (Attack { card = Card.Bureaucrat; data = `Null })
            in
            match%bind
              parse PlayerToGameResponse.Attack.t_of_yojson response
            with
            | PlayerToGameResponse.Attack.{ reaction = Some card; _ } ->
              let%bind () = react ~player ~card in
              return player
            | PlayerToGameResponse.Attack.{ data = Some data; _ } -> (
              match%bind
                parse PlayerToGameResponse.Attack.Bureaucrat.t_of_yojson data
              with
              | PlayerToGameResponse.Attack.Bureaucrat.Reveal ->
                if List.exists (Player.get_hand player) ~f:Card.is_victory then
                  error "Cannot reveal hand; hand contains a victory card."
                else
                  return player (* TODO: reveal *)
              | PlayerToGameResponse.Attack.Bureaucrat.VictoryCard card ->
                if Card.is_victory card then
                  let%bind player = Player.remove_from_hand [card] player in
                  let player = Player.topdeck card player in
                  return player
                else
                  error
                    "Cannot topdeck %s; is not a victory card."
                    (Card.to_string card)
            )
            | PlayerToGameResponse.Attack.{ reaction = None; data = None } ->
              error
                "Bureaucrat response must have nonempty reaction or data field."
          with
          | Ok player -> Lwt.return (Some player)
          | Error err ->
            Player.fatal_error err player;
            Lwt.return None
        )
        turn.next_players
    in
    return
      {
        turn with
        current_player = { player; turn_status };
        supply;
        next_players;
      }
  | Card.Militia ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let turn_status = TurnStatus.add_treasure 2 turn_status in
    let%lwt next_players =
      Lwt_list.filter_map_p
        (fun player ->
          match%lwt
            let%lwt response =
              Player.request
                player
                (Attack { card = Card.Militia; data = `Null })
            in
            match%bind
              parse PlayerToGameResponse.Attack.t_of_yojson response
            with
            | PlayerToGameResponse.Attack.{ reaction = Some card; _ } ->
              let%bind () = react ~player ~card in
              return player
            | PlayerToGameResponse.Attack.{ data = Some data; _ } ->
              let%bind cards =
                parse PlayerToGameResponse.Attack.Militia.t_of_yojson data
              in
              let%bind player = Player.remove_from_hand cards player in
              let%bind () =
                ensure
                  (List.length (Player.get_hand player) <= 3)
                  "%d cards remaining in hand after discarding %s"
                  (List.length (Player.get_hand player))
                  ([%yojson_of: Card.t list] cards |> Yojson.Safe.to_string)
              in
              let player = Player.add_to_discard cards player in
              return player
            | PlayerToGameResponse.Attack.{ reaction = None; data = None } ->
              error
                "Militia response must have nonempty reaction or data field."
          with
          | Ok player -> Lwt.return (Some player)
          | Error err ->
            Player.fatal_error err player;
            Lwt.return None
        )
        turn.next_players
    in
    return { turn with current_player = { player; turn_status }; next_players }
  | Card.Moneylender ->
    let { player; turn_status } = current_player in
    let%bind should_trash = parse Play.Moneylender.t_of_yojson data in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    if should_trash then
      let%bind player = Player.remove_from_hand [Card.Copper] player in
      let trash = Card.Copper :: turn.trash in
      return { turn with current_player = { player; turn_status }; trash }
    else
      return { turn with current_player = { player; turn_status } }
  | Card.Poacher ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 1 player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let turn_status = TurnStatus.add_treasure 1 turn_status in
    let empty_supply_piles = Supply.empty_piles turn.supply in
    let%bind player =
      if empty_supply_piles > 0 then
        let%lwt response =
          Player.request
            player
            (Poacher { hand = Player.get_hand player; empty_supply_piles })
        in
        let%bind PlayerToGameResponse.Poacher.{ discard } =
          parse PlayerToGameResponse.Poacher.t_of_yojson response
        in
        let%bind () =
          ensure
            (List.length discard = empty_supply_piles)
            "Requested to discard %d cards but there are %d empty supply piles"
            (List.length discard)
            (Supply.empty_piles turn.supply)
        in
        let%bind player = Player.remove_from_hand discard player in
        let player = Player.add_to_discard discard player in
        return player
      else
        return player
    in
    return { turn with current_player = { player; turn_status } }
  | Card.Remodel ->
    let { player; turn_status } = current_player in
    let%bind Play.Remodel.{ trash = to_trash; gain = to_gain } =
      parse Play.Remodel.t_of_yojson data
    in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind () =
      ensure
        (Card.cost to_gain - Card.cost to_trash <= 2)
        "%s costs %d more than %s."
        (Card.to_string to_gain)
        (Card.cost to_gain - Card.cost to_trash)
        (Card.to_string to_trash)
    in
    let%bind player = Player.remove_from_hand [to_trash] player in
    let trash = to_trash :: turn.trash in
    let%bind supply = Supply.take to_gain turn.supply in
    let player = Player.add_to_discard [to_gain] player in
    return { turn with current_player = { player; turn_status }; trash; supply }
  | Card.Smithy ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 3 player in
    return { turn with current_player = { player; turn_status } }
  | Card.ThroneRoom ->
    (* TODO allow data to differ between two card invocations *)
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind Play.ThroneRoom.{ card; data } =
      parse Play.ThroneRoom.t_of_yojson data
    in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let turn = { turn with current_player = { player; turn_status } } in
    let%bind turn = play_card ~turn ~card ~data in
    let turn =
      let current_player = CurrentPlayer.unplay_card card turn.current_player in
      let turn_status = TurnStatus.add_actions 1 current_player.turn_status in
      { turn with current_player = { current_player with turn_status } }
    in
    let%bind data =
      let%lwt response = Player.request player (ThroneRoom { card }) in
      let%bind PlayerToGameResponse.ThroneRoom.{ data } =
        parse PlayerToGameResponse.ThroneRoom.t_of_yojson response
      in
      return data
    in
    let%bind turn = play_card ~turn ~card ~data in
    return turn
  | Card.Bandit ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    (* NOTE: if there are no golds left, the attack won't go through. *)
    let%bind supply = Supply.take Card.Gold turn.supply in
    let player = Player.add_to_discard [Card.Gold] player in
    let (add_to_trashed, trash_trashed) : (Card.t -> unit) * (trash -> trash) =
      let trashed = ref [] in
      let add_to_trashed card = Ref.replace trashed (List.cons card) in
      let trash_trashed trash = !trashed @ trash in
      add_to_trashed, trash_trashed
    in
    let%lwt next_players =
      Lwt_list.filter_map_p
        (fun player ->
          match%lwt
            let revealed1, player =
              let take_result = Player.take_from_deck player in
              ( Option.map take_result ~f:fst,
                Option.value_map take_result ~default:player ~f:snd )
            in
            let revealed2, player =
              let take_result = Player.take_from_deck player in
              ( Option.map take_result ~f:fst,
                Option.value_map take_result ~default:player ~f:snd )
            in
            let revealed = List.filter_opt [revealed1; revealed2] in
            let%lwt response =
              Player.request
                player
                (Attack
                   {
                     card = Card.Bandit;
                     data =
                       GameToPlayerRequest.Attack.Bandit.yojson_of_t revealed;
                   }
                )
            in
            match%bind
              parse PlayerToGameResponse.Attack.t_of_yojson response
            with
            | PlayerToGameResponse.Attack.{ reaction = Some card; _ } ->
              let%bind () = react ~player ~card in
              return player
            | PlayerToGameResponse.Attack.{ data = Some data; _ } -> (
              let%bind to_trash =
                parse PlayerToGameResponse.Attack.Bandit.t_of_yojson data
              in
              let is_non_copper_treasure card =
                Card.is_treasure card && not (Card.equal card Card.Copper)
              in
              let non_copper_treasure_was_revealed =
                List.exists revealed ~f:is_non_copper_treasure
              in
              match non_copper_treasure_was_revealed, to_trash with
              | true, None ->
                error
                  "You must select one of the revealed non-copper treasures to \
                   trash."
              | false, Some selection ->
                error
                  "You may not trash %s, as the bandit did not reveal a \
                   non-copper treasure."
                  (Card.to_string selection)
              | true, Some selection ->
                let%bind to_discard =
                  match find_and_remove selection revealed with
                  | Some to_discard -> return to_discard
                  | None ->
                    error
                      "%s is not a card that was revealed by the bandit."
                      (Card.to_string selection)
                in
                let player = Player.add_to_discard to_discard player in
                add_to_trashed selection;
                return player
              | false, None ->
                let player = Player.add_to_discard revealed player in
                return player
            )
            | PlayerToGameResponse.Attack.{ reaction = None; data = None } ->
              error "Bandit response must have nonempty reaction or data field."
          with
          | Ok player -> Lwt.return (Some player)
          | Error err ->
            Player.fatal_error err player;
            Lwt.return None
        )
        turn.next_players
    in
    let trash = trash_trashed turn.trash in
    return
      {
        turn with
        current_player = { player; turn_status };
        next_players;
        trash;
        supply;
      }
  | Card.CouncilRoom ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 4 player in
    let turn_status = TurnStatus.add_buys 1 turn_status in
    let next_players = List.map turn.next_players ~f:(Player.draw_n 1) in
    return { turn with current_player = { player; turn_status }; next_players }
  | Card.Festival ->
    let { turn_status; _ } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let turn_status = TurnStatus.add_actions 2 turn_status in
    let turn_status = TurnStatus.add_buys 1 turn_status in
    let turn_status = TurnStatus.add_treasure 2 turn_status in
    return { turn with current_player = { current_player with turn_status } }
  | Card.Laboratory ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 2 player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    return { turn with current_player = { turn_status; player } }
  | Card.Library ->
    let rec draw_until_7 (player : Player.t) : Player.t errorable =
      if List.length (Player.get_hand player) >= 7 then
        return player
      else
        match Player.take_from_deck player with
        | None -> return player
        | Some (card, player) ->
          if Card.is_action card then
            let%lwt response =
              Player.request
                player
                (Library { card; hand = Player.get_hand player })
            in
            let%bind PlayerToGameResponse.Library.{ skip } =
              parse PlayerToGameResponse.Library.t_of_yojson response
            in
            if skip then
              let%bind player = draw_until_7 player in
              let player = Player.add_to_discard [card] player in
              return player
            else
              let player = Player.add_to_hand card player in
              draw_until_7 player
          else
            let player = Player.add_to_hand card player in
            draw_until_7 player
    in
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind player = draw_until_7 player in
    return { turn with current_player = { turn_status; player } }
  | Card.Market ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 1 player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let turn_status = TurnStatus.add_buys 1 turn_status in
    let turn_status = TurnStatus.add_treasure 1 turn_status in
    return { turn with current_player = { turn_status; player } }
  | Card.Mine ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind Play.Mine.{ trash = to_trash; gain = to_gain } =
      parse Play.Mine.t_of_yojson data
    in
    let%bind () =
      ensure
        (Card.is_treasure to_trash)
        "%s is not a treasure."
        (Card.to_string to_trash)
    in
    let%bind () =
      ensure
        (Card.is_treasure to_gain)
        "%s is not a treasure."
        (Card.to_string to_gain)
    in
    let%bind () =
      ensure
        (Card.cost to_gain - Card.cost to_trash <= 3)
        "%s costs %d more than %s."
        (Card.to_string to_gain)
        (Card.cost to_gain - Card.cost to_trash)
        (Card.to_string to_trash)
    in
    let%bind player = Player.remove_from_hand [to_trash] player in
    let trash = to_trash :: turn.trash in
    let%bind supply = Supply.take to_gain turn.supply in
    let player = Player.add_to_hand to_gain player in
    return { turn with current_player = { player; turn_status }; trash; supply }
  | Card.Sentry ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 1 player in
    let turn_status = TurnStatus.add_actions 1 turn_status in
    let revealed1, player =
      let take_result = Player.take_from_deck player in
      ( Option.map take_result ~f:fst,
        Option.value_map take_result ~default:player ~f:snd )
    in
    let revealed2, player =
      let take_result = Player.take_from_deck player in
      ( Option.map take_result ~f:fst,
        Option.value_map take_result ~default:player ~f:snd )
    in
    let revealed = List.filter_opt [revealed1; revealed2] in
    let%lwt response =
      Player.request
        player
        (Sentry { hand = Player.get_hand player; cards = revealed })
    in
    let%bind card_placements =
      parse PlayerToGameResponse.Sentry.t_of_yojson response
    in
    let cards_placed =
      List.map
        card_placements
        ~f:(fun PlayerToGameResponse.Sentry.{ card; placement = _ } -> card
      )
    in
    let%bind () =
      ensure
        (is_submultiset cards_placed revealed |> Option.is_some
        && is_submultiset revealed cards_placed |> Option.is_some
        )
        "Cards placed %s do not match cards revealed %s"
        ([%yojson_of: Card.t list] cards_placed |> Yojson.Safe.to_string)
        ([%yojson_of: Card.t list] revealed |> Yojson.Safe.to_string)
    in
    let player, trash =
      List.fold
        card_placements
        ~init:(player, turn.trash)
        ~f:(fun (player, trash) PlayerToGameResponse.Sentry.{ card; placement }
           ->
          match placement with
          | PlayerToGameResponse.Sentry.Trash -> player, card :: trash
          | PlayerToGameResponse.Sentry.Discard ->
            Player.add_to_discard [card] player, trash
          | PlayerToGameResponse.Sentry.Topdeck ->
            Player.topdeck card player, trash
      )
    in
    return { turn with current_player = { turn_status; player }; trash }
  | Card.Witch ->
    let { player; turn_status } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let player = Player.draw_n 2 player in
    (* NOTE: sequential attacks instead of parallel
     * because of curse depletion *)
    let%lwt supply, next_players =
      Lwt_list.fold_left_s
        (fun (supply, next_players) player ->
          match%lwt
            let%lwt response =
              Player.request player (Attack { card = Card.Witch; data = `Null })
            in
            match%bind
              parse PlayerToGameResponse.Attack.t_of_yojson response
            with
            | PlayerToGameResponse.Attack.{ reaction = Some card; data = None }
              ->
              let%bind () = react ~player ~card in
              return (supply, player)
            | PlayerToGameResponse.Attack.{ reaction = None; data = None } -> (
              match%lwt Supply.take Card.Curse supply with
              | Ok supply ->
                let player = Player.add_to_discard [Card.Curse] player in
                return (supply, player)
              | Error _ -> return (supply, player)
            )
            | PlayerToGameResponse.Attack.{ data = Some _; _ } ->
              error "No data expected in Witch response"
          with
          | Ok (supply, player) -> Lwt.return (supply, next_players @ [player])
          | Error err ->
            Player.fatal_error err player;
            Lwt.return (supply, next_players)
        )
        (turn.supply, [])
        turn.next_players
    in
    return
      {
        turn with
        current_player = { player; turn_status };
        next_players;
        supply;
      }
  | Card.Artisan ->
    let { turn_status; player } = current_player in
    let%bind turn_status = TurnStatus.expend_action turn_status in
    let%bind Play.Artisan.{ gain; topdeck } =
      parse Play.Artisan.t_of_yojson data
    in
    let%bind () =
      ensure
        (Card.cost gain <= 5)
        "%s costs %d."
        (Card.to_string gain)
        (Card.cost gain)
    in
    let%bind supply = Supply.take gain turn.supply in
    let player = Player.add_to_hand gain player in
    let%bind player = Player.remove_from_hand [topdeck] player in
    let player = Player.topdeck topdeck player in
    return { turn with current_player = { turn_status; player }; supply }

let play ~(game : t) ~(card : Card.t) ~(data : data) ~(name : string) :
    play_response errorable =
  match React.S.value game.state with
  | Turn turn when String.equal (CurrentPlayer.name turn.current_player) name ->
    let%bind turn = play_card ~turn ~card ~data in
    game.set (Turn turn);
    return
      (CurrentPlayer.turn_info
         ~supply:turn.supply
         ~trash:turn.trash
         turn.current_player
      )
  | _ -> error "It is not your turn."

(* TODO: don't allow playing treasures after buying *)
let buy ~(game : t) ~(card : Card.t) ~(name : string) : turn_info errorable =
  match React.S.value game.state with
  | Turn turn when String.equal (CurrentPlayer.name turn.current_player) name ->
    let%bind supply = Supply.take card turn.supply in
    let%bind turn_status =
      TurnStatus.expend_buy
        ~cost:(Card.cost card)
        turn.current_player.turn_status
    in
    let player = Player.add_to_discard [card] turn.current_player.player in
    let turn = { turn with current_player = { turn_status; player }; supply } in
    game.set (Turn turn);
    return
      (CurrentPlayer.turn_info
         turn.current_player
         ~supply:turn.supply
         ~trash:turn.trash
      )
  | _ -> error "It is not your turn."

let create ~(num_players : int) ~(kingdom_selection : kingdom_selection) : t =
  assert (2 <= num_players && num_players <= 6);
  let state, set =
    React.S.create
      ~eq:phys_equal
      (PreStart { num_players; kingdom_selection; players = [] })
  in
  { state; set }

(** removes player from game when they disconnect *)
let on_disconnect ~(game : t) ~(name : string) : unit =
  let new_state =
    match React.S.value game.state with
    | PreStart { num_players; kingdom_selection; players } ->
      let players =
        List.filter players ~f:(fun player ->
            String.( <> ) player.Player.name name
        )
      in
      PreStart { num_players; kingdom_selection; players }
    | Turn turn ->
      if String.equal turn.current_player.CurrentPlayer.player.Player.name name
      then (
        match turn.next_players with
        | [] ->
          PreStart { num_players = 2; kingdom_selection = Random; players = [] }
        | player :: next_players ->
          Lwt.async (fun () ->
              start_turn
                ~game
                ~kingdom:turn.kingdom
                ~supply:turn.supply
                ~trash:turn.trash
                ~player
                ~next_players
                ~kingdom_selection:Random
          );
          Turn turn
      ) else
        Turn
          {
            turn with
            next_players =
              List.filter turn.next_players ~f:(fun player ->
                  String.( <> ) player.Player.name name
              );
          }
  in
  game.set new_state

let add_player (game : t) (name : string) (websocket : Dream.websocket) :
    unit Lwt.t =
  match React.S.value game.state with
  | PreStart { num_players; kingdom_selection; players } ->
    if
      List.exists players ~f:(fun player -> String.equal name player.Player.name)
    then
      failwith (Printf.sprintf "Player with name %s already exists." name)
    else
      (* TODO: don't allow multiple concurrent requests
         e.g. issuing EndTurn while a Play is in progress can let a player
         revert to a prior point in time later on in the game *)
      let handler = function
        | EndTurn () ->
          end_turn ~game ~name
          |> Lwt.map (Result.map ~f:yojson_of_end_turn_response)
        | Play { card; data } ->
          play ~game ~card ~data ~name
          |> Lwt.map (Result.map ~f:yojson_of_play_response)
        | Buy { card } ->
          buy ~game ~card ~name |> Lwt.map (Result.map ~f:yojson_of_turn_info)
      in
      let on_disconnect () = on_disconnect ~game ~name in
      let promise, player =
        Player.create ~name ~websocket ~handler ~on_disconnect
      in
      let players = player :: players in
      game.set (PreStart { num_players; kingdom_selection; players });
      if List.length players >= num_players then
        Lwt.async (fun () -> start_game ~game ~kingdom_selection ~players);
      promise
  | _ -> failwith "Game has already started."
