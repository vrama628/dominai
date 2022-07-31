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
    | `List [ name ] -> name
    | _ -> failwith "unreachable"

  let t_of_yojson json = t_of_yojson (`List [ json ])
end

let error
    (fmt :
      ('r, unit, string, ('a, Jsonrpc.Response.Error.t) result Lwt.t) format4
      ) : 'r =
  Printf.ksprintf
    (fun message ->
      Lwt.return
        (Error
           Jsonrpc.Response.Error.(make ~code:Code.InvalidRequest ~message ())
        )
    )
    fmt

let it_is_not_your_turn_error = error "It is not your turn."

module Play = struct
  module Cellar = struct
    type t = Card.t list [@@deriving yojson]
  end
  module Chapel = struct
    type t = Card.t list [@@deriving yojson]
  end
  module Workshop = struct
    type t = Card.t [@@deriving yojson]
  end
  module Poacher = struct
    type t = Card.t list [@@deriving yojson]
  end
  module Remodel = struct
    type t = {
      trash : Card.t;
      gain : Card.t;
    }
    [@@deriving yojson]
  end
  module ThroneRoom = struct
    type t = Card.t [@@deriving yojson]
  end
  module Mine = struct
    type t = {
      trash : Card.t;
      gain : Card.t;
    }
    [@@deriving yojson]
  end
  module Artisan = struct
    type t = {
      gain : Card.t;
      topdeck : Card.t;
    }
    [@@deriving yojson]
  end

  type data = Yojson.Safe.t
  let data_of_yojson = Fn.id

  let parse
      ~(return : ('b, Jsonrpc.Response.Error.t) result Lwt.t -> 'a)
      (t_of_yojson : data -> 'a)
      (data : data) : 'a =
    try t_of_yojson data
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, json) ->
      return
        (error
           "Invalid format of data field:\ndata: %s\nerror: %s"
           (Yojson.Safe.to_string json)
           (Exn.to_string e)
        )
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

type player_to_game_request =
  | CleanUp of unit
  | Play of {
      card : Card.t;
      data : Play.data;
    }
[@@deriving of_yojson]

let method_and_params_of_json = function
  | `List [ `String method_; `Assoc params ] -> method_, `Assoc params
  | _ -> failwith "unreachable"

let json_of_method_and_params ~method_ ~params =
  `List (`String method_ :: Option.to_list params)

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

    let clean_up { deck; hand; discard } =
      let discard = hand @ discard in
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
  end

  type t = {
    name : string;
    websocket : Dream.websocket;
    pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
    cards : Cards.t ref;
  }

  let create
      ~(name : string)
      ~(websocket : Dream.websocket)
      ~(handler :
         player_to_game_request ->
         (Jsonrpc.Json.t, Jsonrpc.Response.Error.t) result Lwt.t
         ) : t =
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
    let cards = ref (Cards.create ()) in
    { name; websocket; pending; cards }

  let name { name; _ } : string = name

  let get_deck { cards; _ } : int = Cards.get_deck !cards

  let get_hand { cards; _ } : Card.t list = Cards.get_hand !cards

  let get_discard { cards; _ } : int = Cards.get_discard !cards

  let set_hand (hand : Card.t list) { cards; _ } : unit =
    cards := { !cards with hand }

  let draw_n (n : int) (player : t) : unit =
    Ref.replace player.cards (Cards.draw_n n)

  let clean_up (player : t) : unit = Ref.replace player.cards Cards.clean_up

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
end

module TurnStatus = struct
  type t = {
    buys : int;
    actions : int;
    treasure : int;
  }

  let add_buys (n : int) turn_status : t =
    { turn_status with buys = turn_status.buys + n }

  let add_actions (n : int) turn_status : t =
    { turn_status with actions = turn_status.actions + n }

  let add_treasure (n : int) turn_status : t =
    { turn_status with treasure = turn_status.treasure + n }

  let expend_buy turn_status : t option =
    if turn_status.buys > 0 then
      Some { turn_status with buys = turn_status.buys - 1 }
    else
      None

  let expend_action turn_status : t option =
    if turn_status.actions > 0 then
      Some { turn_status with actions = turn_status.actions - 1 }
    else
      None

  let expend_treasure (n : int) turn_status : t option =
    if turn_status.treasure >= n then
      Some { turn_status with treasure = turn_status.actions - n }
    else
      None
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
         hand = Player.get_hand current_player;
         discard = Player.get_discard current_player;
         deck = Player.get_deck current_player;
         supply;
         buys;
         actions;
         treasure;
       }
    );
  Lwt.return_unit

type clean_up_response = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
}
[@@deriving yojson_of]

let clean_up ~(game : t) ~(name : string) :
    (clean_up_response, Jsonrpc.Response.Error.t) result Lwt.t =
  match React.S.value game.state with
  | Turn
      {
        current_player;
        next_players = next_player :: next_players;
        game_over_resolver;
        kingdom;
        supply;
        _;
      }
    when String.equal (Player.name current_player) name ->
      Player.clean_up current_player;
      let clean_up_response =
        let hand = Player.get_hand current_player in
        let discard = Player.get_discard current_player in
        let deck = Player.get_deck current_player in
        { hand; discard; deck; supply }
      in
      let next_players = next_players @ [ current_player ] in
      let current_player = next_player in
      Lwt.async (fun () ->
          start_turn
            ~game
            ~game_over_resolver
            ~kingdom
            ~supply
            ~current_player
            ~next_players
      );
      Lwt.return (Ok clean_up_response)
  | _ -> it_is_not_your_turn_error

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

type play_response = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
  buys : int;
  actions : int;
  treasure : int;
}
[@@deriving yojson_of]

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

let play ~(game : t) ~(card : Card.t) ~(data : Play.data) ~(name : string) :
    (play_response, Jsonrpc.Response.Error.t) result Lwt.t =
  match React.S.value game.state with
  | Turn ({ current_player; turn_status; supply; _ } as turn)
    when String.equal (Player.name current_player) name ->
      With_return.with_return (fun With_return.{ return } ->
          begin
            match card with
            (* we could error when people try to play victory cards but we'll just noop *)
            | Card.Estate | Card.Duchy | Card.Province | Card.Gardens
            | Card.Curse ->
                return (error "")
            | Card.Copper ->
                let turn_status =
                  TurnStatus.
                    { turn_status with treasure = turn_status.treasure + 1 }
                in
                game.set (Turn { turn with turn_status })
            | Card.Silver ->
                let turn_status =
                  TurnStatus.
                    { turn_status with treasure = turn_status.treasure + 2 }
                in
                game.set (Turn { turn with turn_status })
            | Card.Gold ->
                let turn_status =
                  TurnStatus.
                    { turn_status with treasure = turn_status.treasure + 3 }
                in
                game.set (Turn { turn with turn_status })
            | Card.Cellar ->
                let turn_status =
                  match TurnStatus.expend_action turn_status with
                  | None -> return (error "You don't have any actions left.")
                  | Some turn_status -> turn_status
                in
                let cards = Play.parse ~return Play.Cellar.t_of_yojson data in
                let hand = Player.get_hand current_player in
                let hand_after_discarding =
                  match is_submultiset cards hand with
                  | None ->
                      return
                        (error
                           "Your hand %s does not contain all of the cards %s"
                           ([%yojson_of: Card.t list] hand
                           |> Yojson.Safe.to_string
                           )
                           (data |> Yojson.Safe.to_string)
                        )
                  | Some hand_after_discarding -> hand_after_discarding
                in
                Player.set_hand hand_after_discarding current_player;
                Player.draw_n (List.length cards) current_player;
                let turn_status = TurnStatus.add_actions 1 turn_status in
                game.set (Turn { turn with turn_status })
            | Card.Chapel | Card.Moat | Card.Harbinger | Card.Merchant
            | Card.Vassal | Card.Village | Card.Workshop | Card.Bureaucrat
            | Card.Militia | Card.Moneylender | Card.Poacher | Card.Remodel
            | Card.Smithy | Card.ThroneRoom | Card.Bandit | Card.CouncilRoom
            | Card.Festival | Card.Laboratory | Card.Library | Card.Market
            | Card.Mine | Card.Sentry | Card.Witch | Card.Artisan ->
                failwith "unimplemented"
          end;
          let hand = Player.get_hand current_player in
          let discard = Player.get_discard current_player in
          let deck = Player.get_deck current_player in
          let TurnStatus.{ buys; actions; treasure } = turn_status in
          Lwt.return
            (Ok { hand; discard; deck; supply; buys; actions; treasure })
      )
  | _ -> it_is_not_your_turn_error

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
        let handler = function
          | CleanUp () ->
              clean_up ~game ~name
              |> Lwt.map (Result.map ~f:yojson_of_clean_up_response)
          | Play { card; data } ->
              play ~game ~card ~data ~name
              |> Lwt.map (Result.map ~f:yojson_of_play_response)
        in
        let players = Player.create ~name ~websocket ~handler :: players in
        if List.length players >= 2 then
          Lwt.async (fun () -> start_game game players game_over_resolver)
        else
          game.set (PreStart { players; game_over_promise; game_over_resolver });
        game_over_promise
  | _ -> failwith "Game has already started."
