type t

val create :
  url:string ->
  on_notification:(Jsonrpc.Notification.t -> unit) ->
  on_request:(Jsonrpc.Request.t -> Jsonrpc.Response.t Lwt.t) ->
  log_traffic:bool ->
  t Lwt.t

val dispatch :
  t ->
  with_request:(Jsonrpc.Id.t -> Jsonrpc.Request.t) ->
  Jsonrpc.Response.t Lwt.t
