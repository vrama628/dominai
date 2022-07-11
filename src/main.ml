let game = Game.create ()

let handler request =
  match Dream.query request "name" with
  | None -> Dream.json ~status:`Bad_Request "null"
  | Some name -> Dream.websocket (Game.add_player game name)

let () = Dream.run @@ Dream.logger @@ handler
