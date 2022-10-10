open Core

type t = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
  trash : Card.t list;
  buys : int;
  actions : int;
  treasure : int;
  in_play : Card.t list;
  phase : string list;
}
[@@deriving yojson, sexp] [@@yojson.allow_extra_fields]
