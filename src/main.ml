open Core
open Tyxml.Html

let games : (string, Game.t) Hashtbl.t = Hashtbl.create (module String)

let template
    ~title:(page_title : string)
    ~(content : [< Html_types.flow5 ] elt list) =
  let navbar =
    div
      ~a:[a_class ["d-flex"; "justify-content-center"]]
      [
        h1
          ~a:[a_class ["display-1"]]
          [
            a
              ~a:[a_href "/"; a_class ["text-reset"; "text-decoration-none"]]
              [txt "DominAI"];
          ];
      ]
  in
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
    (body
       [
         div
           ~a:[a_class ["container"]]
           [
             div
               ~a:[a_class ["row"; "justify-content-center"]]
               [div ~a:[a_class ["col-md-8"]] (navbar :: content)];
           ];
       ]
    )

let tyxml ?(status : Dream.status option) (html : doc) : Dream.response Lwt.t =
  Format.asprintf "%a" (pp ()) html |> Dream.html ?status

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
        div
          ~a:[a_class ["form-group"]]
          [
            label ~a:[a_label_for "num_players"] [txt "Number of players:"];
            select
              ~a:[a_id "num_players"; a_name "num_players"]
              [
                option ~a:[a_value "2"] (txt "2");
                option ~a:[a_value "3"] (txt "3");
                option ~a:[a_value "4"] (txt "4");
                option ~a:[a_value "5"] (txt "5");
                option ~a:[a_value "6"] (txt "6");
              ];
          ];
        button
          ~a:
            [
              a_button_type `Submit;
              a_class ["btn"; "btn-primary"; "btn-lg"; "m-2"];
            ]
          [txt "Start Game"];
      ]
  in
  tyxml
  @@ template ~title:"DominAI" ~content:[start_game; markdown Static.docs_md]

let () = Random.self_init ()

let generate_game_key () : string =
  String.init 20 ~f:(fun _ -> Random.char ())
  |> Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet

let generate_username () : string =
  Printf.sprintf
    "%s_%s"
    (List.random_element_exn Static.adjectives)
    (List.random_element_exn Static.nouns)

let game_info (request : Dream.request) : Dream.response Lwt.t =
  let game_key = Dream.param request "game" in
  if Hashtbl.find games game_key |> Option.is_none then
    tyxml ~status:`Not_Found
    @@ template
         ~title:"Game Not Found | Dominai"
         ~content:
           [
             div
               ~a:[a_class ["text-center"]]
               [
                 txt "Game not found. ";
                 a ~a:[a_href "/"] [txt "Return to home."];
               ];
           ]
  else
    let copy_game_uri =
      let host_uri = Dream.header request "Host" |> Option.value_exn in
      let join_uri =
        Printf.sprintf
          "http://%s/join/%s?name=%s"
          host_uri
          game_key
          (generate_username ())
      in
      div
        [
          txt
            "To connect your code to this DominAI game, send a GET request to \
             the below URL.";
          div
            ~a:[a_class ["user-select-all"; "bg-light"; "rounded"; "p-3"]]
            [txt join_uri];
          txt
            "To invite a friend to this game, send them the URL that's in your \
             address bar.";
          div [txt "Current game status:"];
          div ~a:[a_id "app"] [];
        ]
    in
    tyxml
    @@ template
         ~title:"Game | DominAI"
         ~content:
           [
             h1 [txt "DominAI Game"];
             copy_game_uri;
             script (cdata_script Static.Js.game_info);
           ]

let create_game (request : Dream.request) : Dream.response Lwt.t =
  match%lwt Dream.form ~csrf:false request with
  | `Ok [("num_players", num_players_str)] ->
    let num_players = Int.of_string num_players_str in
    let game = Game.create ~num_players in
    let key = generate_game_key () in
    Hashtbl.add_exn games ~key ~data:game;
    Dream.redirect request (Printf.sprintf "/game/%s" key)
  | _ -> Dream.empty `Bad_Request

let join_game (request : Dream.request) : Dream.response Lwt.t =
  let game_and_username_opt =
    let open Option.Let_syntax in
    let%bind game = Hashtbl.find games (Dream.param request "game") in
    let%bind username = Dream.query request "name" in
    return (game, username)
  in
  match game_and_username_opt with
  | None -> Dream.json ~status:`Bad_Request "null"
  | Some (game, name) -> Dream.websocket (Game.add_player game name)

let game_state (request : Dream.request) : Dream.response Lwt.t =
  match Hashtbl.find games (Dream.param request "game") with
  | None -> Dream.json ~status:`Not_Found "null"
  | Some game -> Dream.json @@ Yojson.Safe.to_string @@ Game.yojson_of_t @@ game

let router : Dream.handler =
  Dream.router
    [
      Dream.get "/" index;
      Dream.post "/game" create_game;
      Dream.get "/game/:game" game_info;
      Dream.get "/game/:game/state" game_state;
      Dream.get "/join/:game" join_game;
    ]

let () = Dream.run ~interface:"0.0.0.0" @@ Dream.logger @@ router
