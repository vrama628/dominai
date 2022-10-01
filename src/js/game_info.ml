open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
(* open React
   open ReactiveData *)

let main () : unit Lwt.t =
  let app_container = Dom_html.getElementById_exn "app" in
  let%lwt XmlHttpRequest.{ content; _ } =
    XmlHttpRequest.get (Url.Current.path_string ^ "/state")
  in
  Dom.appendChild app_container (To_dom.of_element (Html.txt content));
  Lwt.return_unit

let () = Lwt.async main
