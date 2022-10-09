open Core
open Import

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = flag "-url" (required string) ~doc:""
  and log_traffic = flag "-log-traffic" no_arg ~doc:""
  and log_events = flag "-log-events" no_arg ~doc:""
  and strategy = flag "-strategy" (required string) ~doc:""
  and games = flag "-games" (optional int) ~doc:"" in
  fun () ->
    let games = Option.value ~default:1 games in
    let (module Ai) =
      (* Map your strategy's name to its module *)
      match strategy with
      | "big-money" -> (module Dominai_ai.Big_money : Ai_intf.S)
      | "one-action" -> (module Dominai_ai.One_action : Ai_intf.S)
      | _ -> failwith "Unknown strategy type"
    in
    let (module Client) = (module Client.Make (Ai) : Client.S) in
    Lwt_main.run (Client.main ~url ~log_traffic ~log_events ~games)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server"
    params
