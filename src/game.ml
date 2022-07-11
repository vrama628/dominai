open Base

type state =
  | PreStart of { players : (string * Dream.websocket) list }
  | GameOver of { winner : string }

type t = {
  state : state React.signal;
  set : state -> unit;
}

let create () =
  let state, set =
    let players = [] in
    React.S.create (PreStart { players })
  in
  { state; set }

(** promise that resoles when the given game is over *)
let game_over_promise game =
  let promise, resolver = Lwt.wait () in
  ignore
    (React.S.trace
       (function
         | GameOver _ ->
             if Lwt.is_sleeping promise then Lwt.wakeup_later resolver ()
         | _ -> ())
       game.state
      : state React.signal);
  promise

let add_player (game : t) (name : string) (websocket : Dream.websocket) =
  match React.S.value game.state with
  | PreStart { players } ->
      if List.Assoc.find players ~equal:String.equal name |> Option.is_some then
        failwith (Printf.sprintf "Player with name %s already exists." name)
      else
        let players = (name, websocket) :: players in
        game.set (PreStart { players });
        game_over_promise game
  | _ -> failwith "Game has already started."
