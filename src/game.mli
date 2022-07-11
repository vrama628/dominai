type t

val create : unit -> t

val add_player : t -> string -> Dream.websocket -> unit Lwt.t
