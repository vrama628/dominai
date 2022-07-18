open Base

type card =
  | Cellar
  | Chapel
  | Moat
  | Harbinger
  | Merchant
  | Vassal
  | Village
  | Workshop
  | Beureaucrat
  | Gardens
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
[@@deriving to_yojson]

type game_to_player_notification = GameStart of { kingdom : card list }
[@@deriving to_yojson]

let json_to_notification = function
  | `List [ `String method_; `Assoc params ] -> method_, `Assoc params
  | _ -> failwith "unreachable"

module Player = struct
  type t =
    string * Dream.websocket (* TODO: add tracking for hand, discard, etc *)

  let notify ((_, websocket) : t) (notification : game_to_player_notification) :
      unit =
    let method_, params =
      notification
      |> game_to_player_notification_to_yojson
      |> json_to_notification
    in
    Lwt.async (fun () ->
        Jsonrpc.Notification.create ~method_ ~params ()
        |> Jsonrpc.Notification.yojson_of_t
        |> Yojson.Safe.to_string
        |> Dream.send websocket
    )
end

type state =
  | PreStart of { players : Player.t list }
  | InProgress of {
      kingdom : card list;
      current_player : Player.t;
      other_player : Player.t;
    }
  | GameOver of { winner : string }

type t = {
  state : state React.signal;
  set : state -> unit;
}

let randomizer_cards =
  [
    Cellar;
    Chapel;
    Moat;
    Harbinger;
    Merchant;
    Vassal;
    Village;
    Workshop;
    Beureaucrat;
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

let shuffle (list : 'a list) : 'a list =
  let tagged = List.map ~f:(fun x -> Random.bits (), x) list in
  let sorted =
    List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) tagged
  in
  List.map ~f:snd sorted

let start (game : t) : unit =
  let current_player, other_player =
    match React.S.value game.state with
    | PreStart { players = [ p1; p2 ] } -> p1, p2
    | _ -> failwith "unreachable"
  in
  let kingdom = List.take (shuffle randomizer_cards) 10 in
  List.iter [ current_player; other_player ] ~f:(fun player ->
      Player.notify player (GameStart { kingdom })
  );
  game.set (InProgress { kingdom; current_player; other_player })

let create () : t =
  let state, set =
    let players = [] in
    React.S.create (PreStart { players })
  in
  let game = { state; set } in
  ignore
    ( React.S.trace
        (function
          | PreStart { players = [ _; _ ] } ->
              (* start the game as soon as two players have connected *)
              (* alternatively, instead of listening to the signal we could
               * just trigger this in add_player *)
              start game
          | _ -> ()
          )
        state
      : state React.signal
      );
  game

(** promise that resoles when the given game is over *)
let game_over_promise (game : t) =
  let promise, resolver = Lwt.wait () in
  ignore
    ( React.S.trace
        (function
          | GameOver _ ->
              if Lwt.is_sleeping promise then Lwt.wakeup_later resolver ()
          | _ -> ()
          )
        game.state
      : state React.signal
      );
  promise

let add_player (game : t) (name : string) (websocket : Dream.websocket) =
  match React.S.value game.state with
  | PreStart { players } ->
      if List.Assoc.find players ~equal:String.equal name |> Option.is_some then
        failwith (Printf.sprintf "Player with name %s already exists." name)
      else
        let players = (name, websocket) :: players in
        game.set (PreStart { players });
        game_over_promise game
  | _ -> failwith "Game has already started."

let _ = GameOver { winner = "" }
