open! Core
open Import

module type S = sig
  val main :
    url:string -> log_traffic:bool -> log_events:bool -> games:int -> unit Lwt.t
end

module Make (Ai : Ai_intf.S) : S
