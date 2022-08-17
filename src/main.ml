open Core
open Tyxml.Html

let games : (string, Game.t) Hashtbl.t = Hashtbl.create (module String)

let template
    ~title:(page_title : string)
    ~body:(page_body : [< Html_types.flow5 ] elt list) =
  html
    ~a:[a_lang "en"]
    (head
       (title (txt page_title))
       [
         meta ~a:[a_charset "utf-8"] ();
         meta
           ~a:
             [
               a_name "viewport"; a_content "width=device-width, initial-scale=1";
             ]
           ();
         link
           ~rel:[`Stylesheet]
           ~href:
             "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css"
           ~a:
             [
               a_integrity
                 "sha384-gH2yIJqKdNHPEq0n4Mqa/HGKIhSkIHeL5AyhkYV8i59U5AR6csBvApHHNl/vI1Bx";
               a_crossorigin `Anonymous;
             ]
           ();
       ]
    )
    (body page_body)

let tyxml (html : doc) : Dream.response Lwt.t =
  Format.asprintf "%a" (pp ()) html |> Dream.html

let markdown (md : string) : [> Html_types.txt ] elt =
  md |> Omd.of_string |> Omd.to_html |> entity

let index (_ : Dream.request) : Dream.response Lwt.t =
  tyxml
  @@ template
       ~title:"DominAI"
       ~body:
         [
           div
             ~a:[a_class ["container"]]
             [
               div
                 ~a:[a_class ["row"; "justify-content-center"]]
                 [
                   div
                     ~a:[a_class ["col-md-4"]]
                     [h1 [txt "DominAI"]; markdown Static.docs_md];
                 ];
             ];
         ]

let create_game : Dream.handler = fun _ -> failwith "TODO"

let game_info : Dream.handler = fun _ -> failwith "TODO"

let join_game (request : Dream.request) : Dream.response Lwt.t =
  let game_name_opt =
    let open Option.Let_syntax in
    let%bind game = Hashtbl.find games (Dream.param request "game") in
    let%bind name = Dream.query request "name" in
    return (game, name)
  in
  match game_name_opt with
  | None -> Dream.json ~status:`Bad_Request "null"
  | Some (game, name) -> Dream.websocket (Game.add_player game name)

let router : Dream.handler =
  Dream.router
    [
      Dream.get "/" index;
      Dream.post "/game" create_game;
      Dream.get "/game/:game" game_info;
      Dream.post "/join/:game" join_game;
    ]

let () = Dream.run ~interface:"0.0.0.0" @@ Dream.logger @@ router