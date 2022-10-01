type t

val yojson_of_t : t -> Yojson.Safe.t
(** public state *)

val create : unit -> t

val add_player : t -> string -> Dream.websocket -> unit Lwt.t
