open Base
open Stdio

type card =
  (* TREASURE CARDS *)
  | Copper
  | Silver
  | Gold
  (* VICTORY CARDS *)
  | Estate
  | Duchy
  | Province
  | Gardens
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
[@@deriving yojson_of]

(* hack around ppx_yojson_conv's weird json translation of enums *)
let yojson_of_card card =
  match yojson_of_card card with
  | `List [ name ] -> name
  | _ -> failwith "unreachable"

type game_to_player_notification = StartGame of { kingdom : card list }
[@@deriving yojson_of]

type game_to_player_request = StartGame of { kingdom : card list }
[@@deriving yojson_of]

let method_and_params_of_json = function
  | `List [ `String method_; `Assoc params ] -> method_, `Assoc params
  | _ -> failwith "unreachable"

module Player = struct
  type t = {
    name : string;
    websocket : Dream.websocket;
    pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
  }

  let create ~name ~websocket =
    let pending = Hashtbl.create (module Int) in
    let rec listen () =
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
          | _ -> failwith "TODO"
        )
    in
    Lwt.async listen;
    { name; websocket; pending }

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

type state =
  | PreStart of {
      game_over_promise : unit Lwt.t;
      game_over_resolver : unit Lwt.u;
      players : Player.t list;
    }
  | InProgress of {
      game_over_resolver : unit Lwt.u;
      kingdom : card list;
      current_player : Player.t;
      other_players : Player.t list;
    }

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

let shuffle (list : 'a list) : 'a list =
  let tagged = List.map ~f:(fun x -> Random.bits (), x) list in
  let sorted =
    List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) tagged
  in
  List.map ~f:snd sorted

(* PRECONDITION: between 2 and 4 players *)
let start (game : t) (players : Player.t list) (game_over_resolver : unit Lwt.u)
    : unit =
  let current_player, other_players =
    match shuffle players with p :: ps -> p, ps | _ -> failwith "unreachable"
  in
  let kingdom = List.take (shuffle randomizer_cards) 10 in
  List.iter players ~f:(fun player ->
      Player.notify player (StartGame { kingdom })
  );
  game.set
    (InProgress { kingdom; current_player; other_players; game_over_resolver })

let create () : t =
  let state, set =
    let players = [] in
    let game_over_promise, game_over_resolver = Lwt.wait () in
    React.S.create ~eq:phys_equal
      (PreStart { players; game_over_promise; game_over_resolver })
  in
  { state; set }

let add_player (game : t) (name : string) (websocket : Dream.websocket) =
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
          start game players game_over_resolver
        else
          game.set (PreStart { players; game_over_promise; game_over_resolver });
        game_over_promise
  | _ -> failwith "Game has already started."
