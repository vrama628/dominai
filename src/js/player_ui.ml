open Js_of_ocaml

(* open Js_of_ocaml_lwt *)
open Js_of_ocaml_tyxml.Tyxml_js
open React

(* open ReactiveData *)
open Base
open Dominai

type connection = Jsonrpc.Packet.t event

let websocket ~(game_key : string) ~(name : string) : connection =
  let websocket_js =
    let host = Dom_html.window##.location##.host in
    let url =
      Printf.sprintf "ws://%s/join/%s?name=%s" (Js.to_string host) game_key name
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
  event

type supply = (Card.t * int) list
let supply_of_yojson (json : Yojson.Safe.t) : supply =
  json |> Yojson.Safe.Util.to_assoc |> List.Assoc.map ~f:Card.t_of_yojson

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

type app_state =
  | PreJoin
  | Connected of game_state

let (state_s, set_state) : app_state signal * (?step:step -> app_state -> unit)
    =
  S.create PreJoin

let connection_handler = function _ -> ()

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
        let connection = websocket ~game_key ~name in
        set_state (Connected PreStart);
        ignore (E.map connection_handler connection);
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
    | Connected (InPlay { supply; turn }) ->
      div [div [txt "Supply:"; div ~a:[a_class ["d-flex"]] (List.map supply ~f:(fun (name, ) -> div []))]]
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
