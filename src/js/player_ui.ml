open Js_of_ocaml

(* open Js_of_ocaml_lwt *)
open Js_of_ocaml_tyxml.Tyxml_js

open React

(* open ReactiveData *)
open Base
(* open Dominai *)

let websocket ~(game_key : string) : Jsonrpc.Packet.t event =
  let websocket_js =
    new%js WebSockets.webSocket
      (Printf.sprintf "/game/%s/join" game_key |> Js.string)
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

type state = { in_play : bool }

let initial_state : state = { in_play = false }

let state_to_string (state : state) : string =
  ignore state;
  ""

let state_s (event : Jsonrpc.Packet.t event) : state signal =
  let state_s, set_state = S.create initial_state in
  let update = function _ -> ignore set_state in
  ignore (E.map update event);
  state_s

let game_state_app ~(game_key : string) : Html_types.div Html.elt =
  let open Html in
  let state_s = state_s (websocket ~game_key) in
  div [txt game_key; R.Html.txt (S.map state_to_string state_s)]

let main () : unit Lwt.t =
  let app_container = Dom_html.getElementById_exn "app" in
  let game_key =
    let game_key_js_opt =
      app_container##getAttribute (Js.string "data-game-key")
    in
    Js.Opt.case
      game_key_js_opt
      (fun () -> failwith "data-game-key not set")
      Js.to_string
  in
  Dom.appendChild app_container (To_dom.of_element (game_state_app ~game_key));
  Lwt.return_unit

let () = Lwt.async main
