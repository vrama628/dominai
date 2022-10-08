open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open React
open ReactiveData
open Base
open Dominai

module Connection = struct
  type t = {
    event : Jsonrpc.Packet.t event;
    send : Jsonrpc.Packet.t -> unit;
  }

  let equal : t -> t -> bool = phys_equal

  let connect ~(game_key : string) ~(name : string) : t =
    let websocket_js =
      let host = Dom_html.window##.location##.host in
      let url =
        Printf.sprintf
          "ws://%s/join/%s?name=%s"
          (Js.to_string host)
          game_key
          name
      in
      new%js WebSockets.webSocket (Js.string url)
    in
    let event, trigger = E.create () in
    let on_message (event : 'a WebSockets.messageEvent Js.t) : bool Js.t =
      trigger
      @@ Jsonrpc.Packet.t_of_yojson
      @@ Yojson.Safe.from_string
      @@ Js.to_string
      @@ event##.data;
      Js._false
    in
    websocket_js##.onmessage := Dom.handler on_message;
    let send (packet : Jsonrpc.Packet.t) =
      let packet_js_str =
        packet
        |> Jsonrpc.Packet.yojson_of_t
        |> Yojson.Safe.to_string
        |> Js.string
      in
      websocket_js##send packet_js_str
    in
    { event; send }
end

module Requests = struct
  let fresh_id =
    let counter = ref 0 in
    fun () ->
      Ref.replace counter (( + ) 1);
      !counter

  let pending = Hashtbl.create (module Int)

  let request ~(connection : Connection.t) (request : Api.player_to_game_request)
      : (Yojson.Safe.t, Jsonrpc.Response.Error.t) Result.t Lwt.t =
    let promise, resolver = Lwt.wait () in
    let method_, params =
      request
      |> Api.yojson_of_player_to_game_request
      |> Api.method_and_params_of_json
    in
    let key = fresh_id () in
    Hashtbl.add_exn pending ~key ~data:resolver;
    let id = `Int key in
    connection.send
      (Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_ ~params ()));
    promise

  let response Jsonrpc.Response.{ id; result } : unit =
    let key =
      match id with `Int key -> key | _ -> failwith "invalid response id"
    in
    let resolver = Hashtbl.find_exn pending key in
    Lwt.wakeup_later resolver result
end

type turn =
  | YourTurn of Api.turn_info
  | NotYourTurn
[@@deriving eq]

type game_state =
  | PreStart
  | InPlay of {
      kingdom : Card.t list;
      order : string list;
      turn : turn;
    }
  | GameOver of {
      result : Api.game_over_result;
      scores : Scores.t;
    }
[@@deriving eq]

type app_state =
  | PreJoin
  | Connected of {
      name : string;
      connection : Connection.t;
      game_state : game_state;
    }
[@@deriving eq]

let (state_s, set_state) : app_state signal * (?step:step -> app_state -> unit)
    =
  S.create ~eq:equal_app_state PreJoin

let handle_notification (notification : Api.game_to_player_notification) : unit
    =
  match notification, S.value state_s with
  | ( Api.StartTurn turn_info,
      Connected { name; connection; game_state = InPlay in_play } ) ->
    set_state
      (Connected
         {
           name;
           connection;
           game_state = InPlay { in_play with turn = YourTurn turn_info };
         }
      )
  | Api.FatalError { message }, _ -> Firebug.console##log message
  | _ -> failwith "invalid state"

let handle_request (request : Api.game_to_player_request) : Yojson.Safe.t Lwt.t
    =
  match request, S.value state_s with
  | Api.StartGame { kingdom; order }, Connected connected ->
    let game_state = InPlay { kingdom; order; turn = NotYourTurn } in
    set_state (Connected { connected with game_state });
    Lwt.return (`Assoc [])
  | Api.GameOver { result; scores }, Connected connected ->
    let game_state = GameOver { result; scores } in
    set_state (Connected { connected with game_state });
    Lwt.return (`Assoc ["rematch", `Bool false])
  | _ -> failwith "invalid state"

let play_data_app () : Html_types.div Html.elt * (Card.t -> Yojson.Safe.t Lwt.t)
    =
  let card_s, set_card = S.create None in
  let promise, resolver = Lwt.wait () in
  let get_data (card : Card.t) : Yojson.Safe.t Lwt.t =
    set_card (Some card);
    promise
  in
  let render_app : Card.t option -> Html_types.div Html.elt =
    let open Html in
    function
    | None -> div []
    | Some card -> (
      match card with
      | Copper | Silver | Gold | Estate | Duchy | Province | Gardens | Curse
      | Cellar | Chapel | Moat | Harbinger | Merchant | Vassal | Village
      | Workshop | Bureaucrat | Militia | Moneylender | Poacher | Remodel
      | Smithy | ThroneRoom | Bandit | CouncilRoom | Festival | Laboratory
      | Library | Market | Mine | Sentry | Witch | Artisan ->
        Lwt.wakeup_later resolver `Null;
        div []
    )
  in
  R.Html.div @@ RList.singleton_s @@ S.map render_app card_s, get_data

let render_turn ~(connection : Connection.t) : turn -> Html_types.div Html.elt =
  let open Html in
  let play_data_app, get_play_data = play_data_app () in
  let play_card (card : Card.t) _ =
    Lwt.async (fun () ->
        let%lwt data = get_play_data card in
        let%lwt response =
          Requests.request ~connection (Api.Play { card; data })
        in
        Firebug.console##log response;
        Lwt.return_unit
    );
    false
  in
  let buy_card (card : Card.t) _ =
    Lwt.async (fun () ->
        let%lwt response = Requests.request ~connection (Api.Buy { card }) in
        Firebug.console##log response;
        Lwt.return_unit
    );
    false
  in
  function
  | NotYourTurn -> div [txt "It is not currently your turn."]
  | YourTurn
      Api.
        {
          hand : Card.t list;
          discard : int;
          deck : int;
          supply : Supply.t;
          trash : Trash.t;
          buys : int;
          actions : int;
          treasure : int;
          in_play : Card.t list;
          phase : turn_phase;
        } ->
    ignore (trash, in_play, phase);
    div
      [
        div [txt "It is your turn."];
        div
          [
            div [txt "Supply:"];
            div
              ~a:[a_class ["d-flex"; "flex-wrap"; "justify-content-around"]]
              (List.map (supply |> Map.to_alist) ~f:(fun (card, amount) ->
                   div
                     ~a:
                       [
                         a_class
                           [
                             "bg-primary";
                             "m-2";
                             "p-2";
                             "rounded";
                             "text-light";
                             "fw-semibold";
                           ];
                         a_role ["button"];
                         a_onclick (buy_card card);
                       ]
                     [
                       div [txt @@ Card.to_string card];
                       div
                         ~a:[a_class ["d-flex"; "justify-content-between"]]
                         [
                           div [txt @@ Int.to_string amount];
                           div [txt "$"; txt @@ Int.to_string @@ Card.cost card];
                         ];
                     ]
               )
              );
          ];
        div
          [
            txt
              (Printf.sprintf
                 "Cards in deck: %d. Cards in discard: %d."
                 deck
                 discard
              );
          ];
        div
          [
            txt
              (Printf.sprintf
                 "You currently have remaining: %d actions, %d buys, and %d \
                  treasure."
                 actions
                 buys
                 treasure
              );
          ];
        play_data_app;
        div
          [
            div [txt "Hand:"];
            div
              ~a:[a_class ["d-flex"; "flex-wrap"; "justify-content-center"]]
              (List.map hand ~f:(fun card ->
                   div
                     ~a:
                       [
                         a_class
                           [
                             "bg-primary";
                             "m-2";
                             "p-2";
                             "rounded";
                             "text-light";
                             "fw-semibold";
                           ];
                         a_role ["button"];
                         a_onclick (play_card card);
                       ]
                     [txt @@ Card.to_string card]
               )
              );
          ];
      ]

let game_state_app ~(game_key : string) ~(default_name : string) :
    Html_types.div Html.elt =
  let open Html in
  let render = function
    | PreJoin ->
      let join_game _ =
        let name =
          let input_opt =
            Dom_html.getElementById_exn "name" |> Dom_html.CoerceTo.input
          in
          let value_opt =
            Js.Opt.map input_opt (fun input -> Js.to_string input##.value)
          in
          Js.Opt.get value_opt (fun () ->
              failwith "could not find username element"
          )
        in
        let connection = Connection.connect ~game_key ~name in
        set_state (Connected { name; connection; game_state = PreStart });
        let connection_handler : Jsonrpc.Packet.t -> unit = function
          | Jsonrpc.Packet.Notification Jsonrpc.Notification.{ method_; params }
            ->
            handle_notification
              (Api.json_of_method_and_params
                 ~method_
                 ~params:(Option.map ~f:Jsonrpc.Structured.yojson_of_t params)
              |> Api.game_to_player_notification_of_yojson
              )
          | Jsonrpc.Packet.Request Jsonrpc.Request.{ method_; params; id } ->
            Lwt.async (fun () ->
                let%lwt response =
                  handle_request
                    (Api.json_of_method_and_params
                       ~method_
                       ~params:
                         (Option.map ~f:Jsonrpc.Structured.yojson_of_t params)
                    |> Api.game_to_player_request_of_yojson
                    )
                in
                connection.send
                  (Jsonrpc.Packet.Response (Jsonrpc.Response.ok id response));
                Lwt.return_unit
            )
          | Jsonrpc.Packet.Response response -> Requests.response response
          | _ -> failwith "received unsupported JSONRPC message from server"
        in
        ignore (E.map connection_handler connection.event);
        false
      in
      div
        [
          div
            ~a:[a_class ["form-group"]]
            [
              label ~a:[a_label_for "name"] [txt "Name:"];
              input
                ~a:
                  [
                    a_id "name";
                    a_name "name";
                    a_class ["form-control"];
                    a_value default_name;
                  ]
                ();
            ];
          button
            ~a:
              [
                a_button_type `Button;
                a_class ["btn"; "btn-primary"; "btn-lg"; "m-2"];
                a_onclick join_game;
              ]
            [txt "Join Game"];
        ]
    | Connected { game_state = PreStart; _ } ->
      div [txt "Waiting for game to start ..."]
    | Connected
        { name = _; connection; game_state = InPlay { kingdom; order; turn } }
      ->
      div
        [
          div
            [
              txt
                (Printf.sprintf
                   "Kingdom: %s"
                   (List.map ~f:Card.to_string kingdom
                   |> String.concat ~sep:", "
                   )
                );
            ];
          div
            [
              txt
                (Printf.sprintf "Turn order: %s" (String.concat ~sep:", " order));
            ];
          render_turn ~connection turn;
        ]
    | Connected { game_state = GameOver { result; scores }; _ } ->
      div
        [
          div [txt "Game over."];
          div
            [
              txt
                ( match result with
                | Api.Win -> "You win!"
                | Api.Lose -> "You lose."
                );
            ];
          div
            [
              txt "Final scores:";
              ul
                (List.map scores ~f:(fun (name, score) ->
                     li [Printf.ksprintf txt "%s: %d" name score]
                 )
                );
            ];
        ]
  in
  R.Html.div (RList.singleton_s (S.map render state_s))

let get_data_attribute (container : Dom_html.element Js.t) (attribute : string)
    : string =
  let game_key_js_opt = container##getAttribute (Js.string attribute) in
  Js.Opt.case
    game_key_js_opt
    (fun () -> failwith "attribute not set")
    Js.to_string

let main () : unit Lwt.t =
  let app_container = Dom_html.getElementById_exn "app" in
  let game_key = get_data_attribute app_container "data-game-key" in
  let default_name = get_data_attribute app_container "data-default-name" in
  Dom.appendChild
    app_container
    (To_dom.of_element (game_state_app ~game_key ~default_name));
  Lwt.return_unit

let () = Lwt.async main