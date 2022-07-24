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
  [@@deriving yojson_of, ord, sexp]

  (* hack around ppx_yojson_conv's weird json translation of enums *)
  let yojson_of_t card =
    match yojson_of_t card with
    | `List [ name ] -> name
    | _ -> failwith "unreachable"
end

module CardComparator = struct
  type t = Card.t

  (* what broken nonsense is this jane street *)
  include Comparator.Make (Card)
end

module Supply = struct
  type t = (Card.t, int, CardComparator.comparator_witness) Map.t

  let supply_of_card : Card.t -> int = function Card.Gardens -> 12 | _ -> 10

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
      List.map kingdom ~f:(fun card -> card, supply_of_card card)
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
           (Card.yojson_of_t key |> Yojson.Safe.to_string, `Int data) :: acc
       )
      )
end

type game_to_player_notification =
  | StartTurn of {
      hand : Card.t list;
      discard : int;
      deck : int;
      supply : Supply.t;
      buys : int;
      actions : int;
      treasure : int;
    }
[@@deriving yojson_of]

type game_to_player_request =
  | StartGame of {
      kingdom : Card.t list;
      order : string list;
    }
[@@deriving yojson_of]

type player_to_game_request = CleanUp of unit [@@deriving yojson_of]

let method_and_params_of_json = function
  | `List [ `String method_; `Assoc params ] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let shuffle (list : 'a list) : 'a list =
  let tagged = List.map ~f:(fun x -> Random.bits (), x) list in
  let sorted =
    List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) tagged
  in
  List.map ~f:snd sorted

module Player = struct
  module Cards = struct
    type t = {
      deck : Card.t list;
      hand : Card.t list;
      discard : Card.t list;
    }

    let draw =
      let shuffle_if_necessary = function
        | { deck = []; hand; discard } ->
            let deck = shuffle discard in
            let discard = [] in
            { deck; hand; discard }
        | cards -> cards
      in
      let draw = function
        | { deck = card :: deck; hand; discard } ->
            let hand = card :: hand in
            { deck; hand; discard }
        | cards -> cards
      in
      Fn.compose draw shuffle_if_necessary

    let draw_n (n : int) : t -> t = Fn.apply_n_times ~n draw

    let create () =
      let deck = [] in
      let hand = [] in
      let discard =
        List.init 3 ~f:(Fn.const Card.Estate)
        @ List.init 7 ~f:(Fn.const Card.Copper)
      in
      draw_n 5 { deck; hand; discard }

    let deck { deck; _ } : int = List.length deck

    let hand { hand; _ } : Card.t list = hand

    let discard { discard; _ } : int = List.length discard
  end

  type t = {
    name : string;
    websocket : Dream.websocket;
    pending : (int, t * Yojson.Safe.t Lwt.u) Hashtbl.t;
    emitter : (player_to_game_request * Yojson.Safe.t Lwt.u) React.event;
    cards : Cards.t;
  }

  let create ~name ~websocket =
    let pending = Hashtbl.create (module Int) in
    let emitter, emit = React.E.create () in
    let rec listen () : never_returns =
      match%lwt Dream.receive websocket with
      | None -> Lwt.return_unit
      | Some message -> (
          match
            message |> Yojson.Safe.from_string |> Jsonrpc.Packet.t_of_yojson
          with
          | Jsonrpc.Packet.Response
              Jsonrpc.Response.{ id = `Int id; result = Ok json } ->
              let resolver =
                Option.value_exn (Hashtbl.find_and_remove pending id)
              in
              Lwt.wakeup_later resolver json;
              listen ()
          | Jsonrpc.Packet.Request Jsonrpc.Request.{ id; method_; params } ->
              failwith "TODO"
          | _ -> failwith "TODO"
        )
    in
    Lwt.async listen;
    let cards = Cards.create () in
    { name; websocket; pending; cards; emitter }

  let name { name; _ } : string = name

  let deck { cards; _ } : int = Cards.deck cards

  let hand { cards; _ } : Card.t list = Cards.hand cards

  let discard { cards; _ } : int = Cards.discard cards

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
        try
          Jsonrpc.Notification.create ~method_ ~params ()
          |> Jsonrpc.Notification.yojson_of_t
          |> Yojson.Safe.to_string
          |> Dream.send websocket
        with e ->
          Printf.sprintf "ERROR SENDING %s" (Exn.to_string e)
          |> print_endline
          |> Lwt.return
    )
end

module TurnStatus = struct
  type t = {
    buys : int;
    actions : int;
    treasure : int;
  }
end

type state =
  | PreStart of {
      game_over_promise : unit Lwt.t;
      game_over_resolver : unit Lwt.u;
      players : Player.t list;
    }
  | Turn of {
      game_over_resolver : unit Lwt.u;
      kingdom : Card.t list;
      supply : Supply.t;
      current_player : Player.t;
      turn_status : TurnStatus.t;
      next_players : Player.t list;
    }

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
    ~(game_over_resolver : unit Lwt.u)
    ~(kingdom : Card.t list)
    ~(supply : Supply.t)
    ~(current_player : Player.t)
    ~(next_players : Player.t list) : unit Lwt.t =
  let buys = 1 in
  let actions = 1 in
  let treasure = 0 in
  let turn_status = TurnStatus.{ buys; actions; treasure } in
  game.set
    (Turn
       {
         game_over_resolver;
         kingdom;
         supply;
         current_player;
         next_players;
         turn_status;
       }
    );
  Player.notify
    current_player
    (StartTurn
       {
         hand = Player.hand current_player;
         discard = Player.discard current_player;
         deck = Player.deck current_player;
         supply;
         buys;
         actions;
         treasure;
       }
    );
  Lwt.return_unit

let clean_up game : unit Lwt.t = Lwt.return_unit

(* PRECONDITION: between 2 and 4 players *)
let start_game
    (game : t)
    (players : Player.t list)
    (game_over_resolver : unit Lwt.u) : unit Lwt.t =
  let current_player, next_players =
    match shuffle players with p :: ps -> p, ps | _ -> failwith "unreachable"
  in
  let kingdom = List.take (shuffle randomizer_cards) 10 in
  let supply = Supply.create ~kingdom ~n_players:(List.length players) in
  let order = List.map (current_player :: next_players) ~f:Player.name in
  let%lwt _ =
    Lwt.all
      (List.map players ~f:(fun player ->
           Player.request player (StartGame { kingdom; order })
       )
      )
  in
  start_turn
    ~game
    ~game_over_resolver
    ~kingdom
    ~supply
    ~current_player
    ~next_players

let create () : t =
  let state, set =
    let players = [] in
    let game_over_promise, game_over_resolver = Lwt.wait () in
    React.S.create
      ~eq:phys_equal
      (PreStart { players; game_over_promise; game_over_resolver })
  in
  { state; set }

let add_player (game : t) (name : string) (websocket : Dream.websocket) :
    unit Lwt.t =
  match React.S.value game.state with
  | PreStart { players; game_over_promise; game_over_resolver } ->
      if
        List.exists players ~f:(fun player ->
            String.equal name player.Player.name
        )
      then
        failwith (Printf.sprintf "Player with name %s already exists." name)
      else
        let players = Player.create ~name ~websocket :: players in
        if List.length players >= 2 then
          Lwt.async (fun () -> start_game game players game_over_resolver)
        else
          game.set (PreStart { players; game_over_promise; game_over_resolver });
        game_over_promise
  | _ -> failwith "Game has already started."
