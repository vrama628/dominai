open Js_of_ocaml

(* open Js_of_ocaml_lwt *)
open Js_of_ocaml_tyxml.Tyxml_js
open React

(* open ReactiveData *)
open Base
open Dominai

module Connection = struct
  type t = {
    event : Jsonrpc.Packet.t event;
    send : Jsonrpc.Packet.t -> unit;
  }

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
      Firebug.console##log url;
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

type turn =
  | YourTurn of Api.turn_info
  | NotYourTurn

type game_state =
  | PreStart
  | InPlay of {
      kingdom : Card.t list;
      order : string list;
      turn : turn;
    }

type app_state =
  | PreJoin
  | Connected of game_state

let (state_s, set_state) : app_state signal * (?step:step -> app_state -> unit)
    =
  S.create PreJoin

let handle_notification (notification : Api.game_to_player_notification) : unit
    =
  match notification, S.value state_s with
  | Api.StartTurn turn_info, Connected (InPlay in_play) ->
    set_state (Connected (InPlay { in_play with turn = YourTurn turn_info }))
  | _ -> failwith "invalid state"

let handle_request (request : Api.game_to_player_request) : Yojson.Safe.t Lwt.t
    =
  match request, S.value state_s with
  | Api.StartGame { kingdom; order }, Connected PreStart ->
    set_state (Connected (InPlay { kingdom; order; turn = NotYourTurn }));
    Lwt.return (`Assoc [])
  | _ -> failwith "invalid state"

let render_turn : turn -> Html_types.div Html.elt =
  let open Html in
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
          ~a:[a_class ["d-flex"]]
          (txt "Supply:"
          :: List.map (supply |> Map.to_alist) ~f:(fun (card, amount) ->
                 div
                   ~a:[a_class ["bg-light"]]
                   [
                     txt (Card.to_string card);
                     txt " : ";
                     txt (Int.to_string amount);
                   ]
             )
          );
        div
          [
            txt
              (Printf.sprintf
                 "Hand: %s"
                 (List.map ~f:Card.to_string hand |> String.concat ~sep:", ")
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
        set_state (Connected PreStart);
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
          | _ -> failwith "unimplemented"
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
    | Connected PreStart -> div [txt "Waiting for game to start ..."]
    | Connected (InPlay { kingdom; order; turn }) ->
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
          render_turn turn;
        ]
  in
  R.Html.div (ReactiveData.RList.singleton_s (S.map render state_s))

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
