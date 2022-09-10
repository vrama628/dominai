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
    (body (h1 [a ~a:[a_href "/"] [txt "DominAI"]] :: page_body))

let tyxml (html : doc) : Dream.response Lwt.t =
  Format.asprintf "%a" (pp ()) html |> Dream.html

let markdown (md : string) : [> Html_types.txt ] elt =
  md |> Omd.of_string |> Omd.to_html |> Unsafe.data

let index (_ : Dream.request) : Dream.response Lwt.t =
  let start_game =
    form
      ~a:
        [
          a_class ["d-grid"; "col-md-6"; "mx-auto"];
          a_method `Post;
          a_action "/game";
        ]
      [
        button
          ~a:[a_button_type `Submit; a_class ["btn"; "btn-primary"]]
          [txt "Start Game"];
      ]
  in
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
                     ~a:[a_class ["col-md-8"]]
                     [start_game; markdown Static.docs_md];
                 ];
             ];
         ]

let () = Random.self_init ()
let generate_game_key () : string =
  String.init 20 ~f:(fun _ -> Random.char ())
  |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet

let create_game (request : Dream.request) : Dream.response Lwt.t =
  let game = Game.create () in
  let key = generate_game_key () in
  Hashtbl.add_exn games ~key ~data:game;
  Dream.redirect request (Printf.sprintf "/game/%s" key)

let game_info (request : Dream.request) : Dream.response Lwt.t =
  let copy_game_uri =
    let host_uri = Dream.header request "Host" |> Option.value_exn in
    let game_key = Dream.param request "game" in
    let game_uri = Printf.sprintf "http://%s/join/%s" host_uri game_key in
    div
      [
        txt
          "To connect your code to this DominAI game, send a GET request to \
           the below URL. Make sure to add a query parameter called \"name\" \
           set to the name of your player.";
        div
          ~a:[a_class ["user-select-all"; "bg-light"; "rounded"; "p-3"]]
          [txt game_uri];
      ]
  in
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
                     ~a:[a_class ["col-md-8"]]
                     [h1 [txt "DominAI Game"]; copy_game_uri];
                 ];
             ];
         ]

let join_game (request : Dream.request) : Dream.response Lwt.t =
  let game_name_opt =
    let open Option.Let_syntax in
    let%bind game = Hashtbl.find games (Dream.param request "game") in
    let%bind name = Dream.query request "name" in
    return (game, name)
  in
  Dream.log "join_game %b" (Option.is_some game_name_opt);
  match game_name_opt with
  | None -> Dream.json ~status:`Bad_Request "null"
  | Some (game, name) -> Dream.websocket (Game.add_player game name)

let router : Dream.handler =
  Dream.router
    [
      Dream.get "/" index;
      Dream.post "/game" create_game;
      Dream.get "/game/:game" game_info;
      Dream.get "/join/:game" join_game;
    ]

let () = Dream.run ~interface:"0.0.0.0" @@ Dream.logger @@ router