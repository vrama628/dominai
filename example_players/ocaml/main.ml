open Core

let main (name : string) : unit Lwt.t =
  let uri : Uri.t =
    Uri.add_query_param' (Uri.of_string "http://localhost:8080") ("name", name)
  in
  let%lwt (endp : Conduit.endp) =
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  in
  let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt (client : Conduit_lwt_unix.client) =
    Conduit_lwt_unix.endp_to_client ~ctx endp
  in
  let%lwt (conn : Websocket_lwt_unix.conn) =
    Websocket_lwt_unix.connect client uri
  in
  let%lwt (frame : Websocket.Frame.t) = Websocket_lwt_unix.read conn in
  Printf.printf "%s\n" (Websocket.Frame.show frame);
  Lwt.return_unit

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map name = "name" %: string |> anon in
  fun () -> Lwt_main.run (main name)

let () =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
  |> Command.run
