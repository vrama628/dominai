type t

val yojson_of_t : t -> Yojson.Safe.t
(** public state *)

type kingdom

val kingdom_of_yojson : Yojson.Safe.t -> kingdom

val create : num_players:int -> kingdom:kingdom -> t

val add_player : t -> string -> Dream.websocket -> unit Lwt.t
