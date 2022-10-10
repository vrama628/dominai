open Core

type t = {
  conn : Websocket_lwt_unix.conn;
  open_requests : Jsonrpc.Response.t Lwt.u String.Table.t;
  log_traffic : bool;
}

let read (t : t) : Jsonrpc.Packet.t Lwt.t =
  let%lwt (frame : Websocket.Frame.t) = Websocket_lwt_unix.read t.conn in
  let packet =
    try
      frame.Websocket.Frame.content |> Yojson.Safe.from_string
      |> Jsonrpc.Packet.t_of_yojson
    with exn ->
      printf "Internal Parse Error: %s\n" (Exn.to_string exn);
      printf "Original Message: %s\n" frame.Websocket.Frame.content;
      Jsonrpc.Packet.Notification
        (Jsonrpc.Notification.create ~method_:"InternalParseError" ())
  in
  Lwt.return packet

let respond (t : t) (packet : Jsonrpc.Response.t) : unit Lwt.t =
  let content = Yojson.Safe.to_string (Jsonrpc.Response.yojson_of_t packet) in
  if t.log_traffic then
    Ocolor_format.printf "@{<blue> Sending response: %s @}\n" content;
  let frame = Websocket.Frame.create ~content () in
  Websocket_lwt_unix.write t.conn frame

let create ~(url : string) ~on_notification ~on_request ~log_traffic : t Lwt.t =
  let uri : Uri.t = Uri.of_string url in
  let%lwt (endp : Conduit.endp) =
    Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  in
  let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt (client : Conduit_lwt_unix.client) =
    Conduit_lwt_unix.endp_to_client ~ctx endp
  in
  let%lwt conn = Websocket_lwt_unix.connect client uri in
  let open_requests = String.Table.create () in
  let t = { conn; open_requests; log_traffic } in
  let () =
    let rec loop () =
      let%lwt packet = read t in
      if t.log_traffic then
        Ocolor_format.printf "@{<blue> New message from server: %s @}\n"
          (packet |> Jsonrpc.Packet.yojson_of_t |> Yojson.Safe.to_string);
      printf "%!";
      match packet with
      | Jsonrpc.Packet.Notification notification ->
          on_notification notification;
          loop ()
      | Response ({ id = `String id; _ } as response) ->
          (match Hashtbl.find_and_remove t.open_requests id with
          | None ->
              Printf.eprintf "BUG! Received response for unknown request: %s"
                (response |> Jsonrpc.Response.yojson_of_t
               |> Yojson.Safe.to_string)
          | Some resolver -> Lwt.wakeup resolver response);
          loop ()
      | Response ({ id = `Int _; _ } as response) ->
          Printf.eprintf "BUG! Received response for unknown request: %s"
            (response |> Jsonrpc.Response.yojson_of_t |> Yojson.Safe.to_string);
          loop ()
      | Request request ->
          Lwt.async (fun () ->
              let%lwt response = on_request request in
              respond t response);
          loop ()
      | (Batch_response _ | Batch_call _) as malformed ->
          Printf.eprintf "Unsupported Batch from server: %s\n"
            (malformed |> Jsonrpc.Packet.yojson_of_t |> Yojson.Safe.to_string);
          loop ()
    in
    Lwt.async loop
  in
  Lwt.return t

let create_id () =
  let rand = List.permute (List.init 20 ~f:Fn.id) in
  "REQ" ^ String.concat (List.map rand ~f:Int.to_string)

let dispatch (t : t) ~(with_request : Jsonrpc.Id.t -> Jsonrpc.Request.t) :
    Jsonrpc.Response.t Lwt.t =
  let promise, resolver = Lwt.wait () in
  let id = create_id () in
  let packet = with_request (`String id) in
  if t.log_traffic then
    Ocolor_format.printf "@{<blue> Sending request: %s @}\n"
      (packet |> Jsonrpc.Request.yojson_of_t |> Yojson.Safe.to_string);
  printf "%!";
  Hashtbl.set t.open_requests ~key:id ~data:resolver;
  let content = Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t packet) in
  let frame = Websocket.Frame.create ~content () in
  let%lwt () = Websocket_lwt_unix.write t.conn frame in
  promise
