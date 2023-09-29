open Core

let games : (string, Game.t) Hashtbl.t = Hashtbl.create (module String)

let () = Random.self_init ()

let generate_game_key () : string =
  String.init 20 ~f:(fun _ -> Random.char ())
  |> Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet

module CreateGame = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives
  type request = {
    kingdom : Game.kingdom;
    num_players : int;
  }
  [@@deriving of_yojson]
  type response = { key : string } [@@deriving yojson_of]
end

let create_game (request : Dream.request) : Dream.response Lwt.t =
  let%lwt body = Dream.body request in
  let CreateGame.{ kingdom; num_players } =
    CreateGame.request_of_yojson @@ Yojson.Safe.from_string body
  in
  let game = Game.create ~num_players ~kingdom in
  let key = generate_game_key () in
  Hashtbl.add_exn games ~key ~data:game;
  Dream.json @@ Yojson.Safe.to_string @@ CreateGame.yojson_of_response { key }

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
      Dream.post "/game" create_game;
      Dream.get "/game/:game/state" game_state;
      Dream.get "/join/:game" join_game;
    ]

let cors_middleware (handler : Dream.handler) (request : Dream.request) :
    Dream.response Lwt.t =
  let%lwt response =
    match Dream.method_ request with
    | `OPTIONS -> Dream.empty `OK
    | _ -> handler request
  in
  Dream.add_header response "Access-Control-Allow-Origin" "*";
  Dream.add_header response "Access-Control-Allow-Headers" "*";
  Dream.add_header response "Access-Control-Allow-Methods" "*";
  Lwt.return response

let () =
  Dream.run ~interface:"0.0.0.0" ~port:3001
  @@ Dream.logger
  @@ cors_middleware
  @@ router
