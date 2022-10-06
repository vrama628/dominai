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

type supply = (Card.t * int) list
let supply_of_yojson (json : Yojson.Safe.t) : supply =
  json
  |> Yojson.Safe.Util.to_assoc
  |> List.map ~f:(fun (card, amount) ->
         Card.t_of_yojson (`String card), Yojson.Safe.Util.to_int amount
     )

type turn =
  | YourTurn
  | OtherTurn

type game_state =
  | PreStart
  | InPlay of {
      kingdom : Card.t list;
      supply : supply;
      turn : turn;
    }

let () =
  ignore YourTurn;
  ignore OtherTurn;
  ignore supply_of_yojson;
  ignore (InPlay { kingdom = []; supply = []; turn = OtherTurn })

type app_state =
  | PreJoin
  | Connected of game_state

let (state_s, set_state) : app_state signal * (?step:step -> app_state -> unit)
    =
  S.create PreJoin

let handle_notification : Api.game_to_player_notification -> unit = function
  | _ -> ()

let handle_request : Api.game_to_player_request -> Yojson.Safe.t Lwt.t =
  function
  | _ -> failwith "TODO"

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
    | Connected (InPlay { supply; turn; kingdom = _ }) ->
      div
        [
          div
            [
              txt "Supply:";
              div
                ~a:[a_class ["d-flex"]]
                (List.map supply ~f:(fun (card, amount) ->
                     div
                       [
                         txt (Card.to_string card);
                         txt ": ";
                         txt (Int.to_string amount);
                       ]
                 )
                );
            ];
          div
            [
              txt
                ( match turn with
                | YourTurn -> "your turn"
                | OtherTurn -> "not your turn"
                );
            ];
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
