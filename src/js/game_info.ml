open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open React
open ReactiveData
open Base

(* types duplicated from game.ml *)
(* TODO: factor out types so we can reuse them in the frontend *)
type player = { name : string } [@@deriving of_yojson]

type state = PreStart of { players : player list } (*| Turn of turn*)
[@@deriving of_yojson]

let fetch_game_state () : state Lwt.t =
  let%lwt XmlHttpRequest.{ content; _ } =
    XmlHttpRequest.get (Url.Current.path_string ^ "/state")
  in
  content |> Yojson.Safe.from_string |> state_of_yojson |> Lwt.return

let game_state_s : state signal =
  let game_state_s, set_game_state = S.create (PreStart { players = [] }) in
  let rec poll_game_state_loop () =
    let%lwt game_state = fetch_game_state () in
    set_game_state game_state;
    let%lwt () = Lwt_js.sleep 2. in
    poll_game_state_loop ()
  in
  Lwt.async poll_game_state_loop;
  game_state_s

let render_game_state (game_state : state) : Html_types.div Html.elt =
  let open Html in
  match game_state with
  | PreStart { players } -> div (List.map ~f:(fun { name } -> txt name) players)

let game_state_app : Html_types.div Html.elt =
  R.Html.div (RList.singleton_s (React.S.map render_game_state game_state_s))

let main () : unit Lwt.t =
  let app_container = Dom_html.getElementById_exn "app" in
  Dom.appendChild app_container (To_dom.of_element game_state_app);
  Lwt.return_unit

let () = Lwt.async main
