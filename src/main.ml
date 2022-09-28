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
        div
          ~a:[a_class ["form-group"]]
          [
            label ~a:[a_label_for "name"] [txt "Username:"];
            input
              ~a:
                [
                  a_input_type `Text;
                  a_class ["form-control"];
                  a_id "name";
                  a_name "name";
                ]
              ();
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
  let copy_game_uri =
    let host_uri = Dream.header request "Host" |> Option.value_exn in
    let game_key = Dream.param request "game" in
    let username =
      match Dream.flash_messages request with
      | [("name", username)] -> username
      | _ -> generate_username ()
    in
    let join_uri =
      Printf.sprintf "http://%s/join/%s?name=%s" host_uri game_key username
    in
    div
      [
        txt
          "To connect your code to this DominAI game, send a GET request to \
           the below URL. Make sure to add a query parameter called \"name\" \
           set to the name of your player.";
        div
          ~a:[a_class ["user-select-all"; "bg-light"; "rounded"; "p-3"]]
          [txt join_uri];
        txt
          "To invite a friend to this game, send them the URL that's in your \
           address bar.";
      ]
  in
  tyxml
  @@ template ~title:"DominAI" ~content:[h1 [txt "DominAI Game"]; copy_game_uri]

let create_game (request : Dream.request) : Dream.response Lwt.t =
  let game = Game.create () in
  let key = generate_game_key () in
  Hashtbl.add_exn games ~key ~data:game;
  let%lwt () =
    match%lwt Dream.form ~csrf:false request with
    | `Ok [("name", username)] ->
      if String.length username > 0 then
        Dream.add_flash_message request "name" username;
      Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  Dream.redirect request (Printf.sprintf "/game/%s" key)

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

let () = Dream.run ~interface:"0.0.0.0" @@ Dream.logger @@ Dream.flash @@ router