type t

val yojson_of_t : t -> Yojson.Safe.t
(** public state *)

type kingdom_selection

val kingdom_selection_of_string : string -> kingdom_selection

val create : num_players:int -> kingdom_selection:kingdom_selection -> t

val add_player : t -> string -> Dream.websocket -> unit Lwt.t
