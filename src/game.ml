open Core

module Card = struct
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
    card |> yojson_of_t |> Yojson.Safe.to_string

  let is_action = function
    | Copper | Silver | Gold | Estate | Duchy | Province | Gardens | Curse ->
      false
    | Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village
    | Workshop | Bureaucrat | Militia | Moneylender | Poacher | Remodel | Smithy
    | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory | Library
    | Market | Mine | Sentry | Witch | Artisan ->
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
end

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
end

type player_to_game_request =
  | CleanUp of unit
  | Play of {
      card : Card.t;
      (* TODO: move data entirely to separate requests *)
      data : data;
    }
[@@deriving of_yojson]

let method_and_params_of_json = function
  | `List [`String method_; `Assoc params] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let json_of_method_and_params ~method_ ~params =
  `List (`String method_ :: Option.to_list params)

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
  end

  type t = {
    name : string;
    websocket : Dream.websocket;
    resolver : unit Lwt.u;
    pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
    cards : Cards.t;
  }

  let create
      ~(name : string)
      ~(websocket : Dream.websocket)
      ~(handler :
         player_to_game_request ->
         (Jsonrpc.Json.t, Jsonrpc.Response.Error.t) result Lwt.t
         ) : unit Lwt.t * t =
    let pending = Hashtbl.create (module Int) in
    let rec listen () =
      match%lwt Dream.receive websocket with
      | None -> Lwt.return_unit
      | Some message ->
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
                response
                |> Jsonrpc.Response.yojson_of_t
                |> Yojson.Safe.to_string
                |> Dream.send websocket
            )
          | _ -> failwith "TODO"
        end;
        listen ()
    in
    Lwt.async listen;
    let cards = Cards.create () in
    let promise, resolver = Lwt.wait () in
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

  let request { websocket; pending; _ } (request : game_to_player_request) :
      Yojson.Safe.t Lwt.t =
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

  let notify { websocket; _ } (notification : game_to_player_notification) :
      unit =
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

  (** notifies and disconnects the player *)
  let fatal_error (error : Jsonrpc.Response.Error.t) (player : t) : unit =
    let Jsonrpc.Response.Error.{ message; _ } = error in
    notify player (FatalError { message });
    Lwt.wakeup player.resolver ()
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
  let expend_buy turn_status : t errorable =
    if turn_status.buys > 0 then
      return { turn_status with buys = turn_status.buys - 1 }
    else
      error "No buys left."

  let expend_action turn_status : t errorable =
    match turn_status.phase with
    | Action ->
      if turn_status.actions > 0 then
        return { turn_status with actions = turn_status.actions - 1 }
      else
        error "No actions left."
    | Buy -> error "Can't perform action in buy phase."

  (*let expend_treasure (n : int) turn_status : t errorable =
    if turn_status.treasure >= n then
      return { turn_status with treasure = turn_status.actions - n }
    else
      error "No treasure left."*)

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

type turn = {
  kingdom : Card.t list;
  supply : Supply.t;
  trash : Card.t list;
  current_player : CurrentPlayer.t;
  next_players : Player.t list;
}

type state =
  | PreStart of { players : Player.t list }
  | Turn of turn

type t = {
  state : state React.signal;
  set : state -> unit;
}

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
    ~(next_players : Player.t list) : unit Lwt.t =
  let current_player =
    let turn_status = TurnStatus.initial in
    CurrentPlayer.{ player; turn_status }
  in
  game.set (Turn { kingdom; supply; trash; current_player; next_players });
  Player.notify
    player
    (StartTurn (CurrentPlayer.turn_info current_player ~supply ~trash));
  Lwt.return_unit

type clean_up_response = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
}
[@@deriving yojson_of]

let clean_up ~(game : t) ~(name : string) : clean_up_response errorable =
  match React.S.value game.state with
  | Turn
      {
        current_player;
        next_players = next_player :: next_players;
        kingdom;
        supply;
        trash;
        _;
      }
    when String.equal (CurrentPlayer.name current_player) name ->
    let prev_player = CurrentPlayer.clean_up current_player in
    let clean_up_response =
      let hand = Player.get_hand prev_player in
      let discard = Player.get_discard prev_player in
      let deck = Player.get_deck prev_player in
      { hand; discard; deck; supply }
    in
    let next_players = next_players @ [prev_player] in
    Lwt.async (fun () ->
        start_turn
          ~game
          ~kingdom
          ~supply
          ~trash
          ~player:next_player
          ~next_players
    );
    Lwt.return (Ok clean_up_response)
  | _ -> error "It is not your turn."

(* PRECONDITION: between 2 and 4 players *)
let start_game ~(game : t) ~(players : Player.t list) : unit Lwt.t =
  let player, next_players =
    match shuffle players with p :: ps -> p, ps | _ -> failwith "unreachable"
  in
  let kingdom = List.take (shuffle randomizer_cards) 10 in
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
  start_turn ~game ~kingdom ~supply ~trash ~player ~next_players

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
    let%lwt response = Player.request player (Harbinger { discard }) in
    let%bind PlayerToGameResponse.Harbinger.{ card } =
      parse PlayerToGameResponse.Harbinger.t_of_yojson response
    in
    let%bind discard =
      match find_and_remove card discard with
      | None -> error "Card %s not in discard pile." (Card.to_string card)
      | Some discard -> return discard
    in
    let player = { player with cards = { player.cards with discard } } in
    let player = Player.topdeck card player in
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
      List.map turn.next_players ~f:(fun player ->
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
      |> Lwt.all
      |> Lwt.map List.filter_opt
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
    let%lwt next_players =
      List.map turn.next_players ~f:(fun player ->
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
      |> Lwt.all
      |> Lwt.map List.filter_opt
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
      List.map turn.next_players ~f:(fun player ->
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
      |> Lwt.all
      |> Lwt.map List.filter_opt
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
      if List.length (Player.get_hand current_player.player) >= 7 then
        return player
      else
        match Player.take_from_deck current_player.player with
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
  | Card.Witch | Card.Artisan -> failwith "unimplemented"

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

let create () : t =
  let state, set = React.S.create ~eq:phys_equal (PreStart { players = [] }) in
  { state; set }

let add_player (game : t) (name : string) (websocket : Dream.websocket) :
    unit Lwt.t =
  match React.S.value game.state with
  | PreStart { players } ->
    if
      List.exists players ~f:(fun player -> String.equal name player.Player.name)
    then
      failwith (Printf.sprintf "Player with name %s already exists." name)
    else
      let handler = function
        | CleanUp () ->
          clean_up ~game ~name
          |> Lwt.map (Result.map ~f:yojson_of_clean_up_response)
        | Play { card; data } ->
          play ~game ~card ~data ~name
          |> Lwt.map (Result.map ~f:yojson_of_play_response)
      in
      let promise, player = Player.create ~name ~websocket ~handler in
      let players = player :: players in
      if List.length players >= 2 then
        Lwt.async (fun () -> start_game ~game ~players)
      else
        game.set (PreStart { players });
      promise
  | _ -> failwith "Game has already started."
