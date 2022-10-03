open Js_of_ocaml

(* open Js_of_ocaml_lwt *)
open Js_of_ocaml_tyxml.Tyxml_js

(* open React *)
(* open ReactiveData *)
open Base
(* open Dominai *)

let game_state_app ~(game_key : string) : Html_types.div Html.elt =
  let open Html in
  div [txt game_key]

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
