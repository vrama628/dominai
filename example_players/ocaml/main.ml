open Core

let fresh_name () = Printf.sprintf "example_%d" (Random.int Int.max_value)

module Conn = struct
  type t = Websocket_lwt_unix.conn

  let connect ~(url : string) : t Lwt.t =
    let name = fresh_name () in
    Printf.printf "Joining as %s ..." name;
    let uri : Uri.t =
      Uri.add_query_param (Uri.of_string url) ("name", [name])
    in
    let%lwt (endp : Conduit.endp) =
      Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
    in
    let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    let%lwt (client : Conduit_lwt_unix.client) =
      Conduit_lwt_unix.endp_to_client ~ctx endp
    in
    Websocket_lwt_unix.connect client uri

  let read (conn : t) : Jsonrpc.Packet.t Lwt.t =
    let%lwt (frame : Websocket.Frame.t) = Websocket_lwt_unix.read conn in
    Lwt.return
      (frame.Websocket.Frame.content
      |> Yojson.Safe.from_string
      |> Jsonrpc.Packet.t_of_yojson
      )
end

type start_game = {
  kingdom : Dominai.Card.t list;
  order : string list;
}
[@@deriving yojson]

let play ~(conn : Conn.t) : unit Lwt.t =
  print_endline "Connected to server. Waiting for game to start ...";
  let%lwt { kingdom; order } =
    match%lwt Conn.read conn with
    | Jsonrpc.Packet.Request
        Jsonrpc.Request.{ method_ = "StartGame"; params = Some params; _ } ->
      params
      |> Jsonrpc.Structured.yojson_of_t
      |> start_game_of_yojson
      |> Lwt.return
    | malformed ->
      Printf.eprintf
        "Was expecting StartGame but received %s"
        (malformed |> Jsonrpc.Packet.yojson_of_t |> Yojson.Safe.to_string);
      failwith "Malformed"
  in
  ignore kingdom;
  ignore order;
  failwith "TODO"

let main (url : string) : unit Lwt.t =
  let%lwt conn = Conn.connect ~url in
  play ~conn

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = "url" %: string |> anon in
  fun () -> Lwt_main.run (main url)

let () =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server"
    params
  |> Command_unix.run
