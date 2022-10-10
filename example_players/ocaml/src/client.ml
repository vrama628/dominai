open Core
open Import

module type S = sig
  val main :
    url:string -> log_traffic:bool -> log_events:bool -> games:int -> unit Lwt.t
end

module Make (Ai : Ai_intf.S) = struct
  type t = {
    conn : Conn.t;
    play_request : Jsonrpc.Request.t Lwt_mvar.t;
    play_response : Jsonrpc.Response.t Lwt_mvar.t;
    ai : Ai.t;
    mutable current_hand : Card.t list;
    rematch : bool;
    log_events : bool;
  }

  let create ~conn ~ai ~rematch ~log_events =
    {
      conn;
      ai;
      play_request = Lwt_mvar.create_empty ();
      play_response = Lwt_mvar.create_empty ();
      current_hand = [];
      rematch;
      log_events;
    }

  let printf t = ksprintf (fun str -> if t.log_events then printf "%s" str)
  let print_endline t = if t.log_events then print_endline else fun _ -> ()

  let parse_params_exn params ~of_yojson =
    match params with
    | None -> failwith "Null params"
    | Some params -> params |> Jsonrpc.Structured.yojson_of_t |> of_yojson

  let parse_result_exn (response : Jsonrpc.Response.t) ~of_yojson =
    match response.result with
    | Error err -> Jsonrpc.Response.Error.raise err
    | Ok result -> of_yojson result

  (* It usually takes < 0.1s for the server to send an action request
     after we send it a play request. We wait up to 0.5s, just to be
     sure. *)
  let take_request_with_timeout t =
    let alarms = [ 0.01; 0.09; 0.11; 0.39 ] in
    let rec loop = function
      | [] -> Lwt.return None
      | alarm :: alarms -> (
          let%lwt () = Lwt_unix.sleep alarm in
          match Lwt_mvar.take_available t.play_request with
          | None -> loop alarms
          | Some request -> Lwt.return (Some request))
    in
    loop alarms

  let end_turn (t : t) : unit Lwt.t =
    print_endline t "Ending turn...";
    let%lwt response =
      Conn.dispatch t.conn ~with_request:(fun id ->
          Jsonrpc.Request.create ~id ~method_:"EndTurn" ())
    in
    let data =
      parse_result_exn response ~of_yojson:Protocol.EndTurn.Response.t_of_yojson
    in
    t.current_hand <- data.hand;
    Lwt.return ()

  let on_harbinger t ~(game_state : Game_state.t) ~card_to_topdeck =
    if game_state.discard <= 0 then Lwt.return ()
    else
      let%lwt request = Lwt_mvar.take t.play_request in
      let { Jsonrpc.Request.params; id; _ } = request in
      let { Protocol.Harbinger.Request.discard } =
        parse_params_exn params
          ~of_yojson:Protocol.Harbinger.Request.t_of_yojson
      in
      let topdeck = card_to_topdeck ~discard in
      let response =
        Jsonrpc.Response.ok id
          (Protocol.Harbinger.Response.yojson_of_t { card = topdeck })
      in
      Lwt_mvar.put t.play_response response

  let on_poacher t ~(game_state : Game_state.t) ~cards_to_discard =
    let has_empty_supply_piles =
      Map.exists game_state.supply ~f:(fun n -> n > 0)
    in
    if not has_empty_supply_piles then Lwt.return ()
    else
      let%lwt request = Lwt_mvar.take t.play_request in
      let { Jsonrpc.Request.params; id; _ } = request in
      let { Protocol.Poacher.Request.hand; empty_supply_piles } =
        parse_params_exn params ~of_yojson:Protocol.Poacher.Request.t_of_yojson
      in
      let discard =
        cards_to_discard ~number_to_discard:empty_supply_piles ~hand
      in
      let response =
        Jsonrpc.Response.ok id
          (Protocol.Poacher.Response.yojson_of_t { discard })
      in
      Lwt_mvar.put t.play_response response

  let on_sentry t ~game_state:_ ~what_to_do =
    let%lwt request = Lwt_mvar.take t.play_request in
    let { Jsonrpc.Request.params; id; _ } = request in
    let { Protocol.Sentry.Request.hand; cards } =
      parse_params_exn params ~of_yojson:Protocol.Sentry.Request.t_of_yojson
    in
    let to_do = what_to_do ~hand ~top_two_cards:cards in
    let response =
      Jsonrpc.Response.ok id
        (Protocol.Sentry.Response.yojson_of_t
           (List.map to_do ~f:(fun (card, to_do) ->
                let open Protocol.Sentry.Response.Placement in
                match to_do with
                | `Trash -> { card; placement = "trash" }
                | `Topdeck -> { card; placement = "topdeck" }
                | `Discard -> { card; placement = "discard" })))
    in
    Lwt_mvar.put t.play_response response

  let rec on_library t ~game_state ~skip_action_card =
    match%lwt take_request_with_timeout t with
    | None -> Lwt.return ()
    | Some request ->
        let { Jsonrpc.Request.params; id; _ } = request in
        let { Protocol.Library.Request.hand; card } =
          parse_params_exn params
            ~of_yojson:Protocol.Library.Request.t_of_yojson
        in
        let skip =
          if Card.is_treasure card then false else skip_action_card ~hand card
        in
        let response =
          Jsonrpc.Response.ok id
            (Protocol.Library.Response.yojson_of_t { skip })
        in
        let%lwt () = Lwt_mvar.put t.play_response response in
        on_library t ~game_state ~skip_action_card

  let rec on_vassal t ~game_state ~play_card_from_topdeck =
    match%lwt take_request_with_timeout t with
    | None ->
        printf t "No action card to Vassal. A card is being discarded...\n";
        Lwt.return ()
    | Some request -> (
        let { Jsonrpc.Request.params; id; _ } = request in
        let { Protocol.Vassal.Request.card } =
          parse_params_exn params ~of_yojson:Protocol.Vassal.Request.t_of_yojson
        in
        printf t "Can Vassal an action card %s...\n" (Card.to_string card);
        let play = play_card_from_topdeck game_state card in
        let response =
          Jsonrpc.Response.ok id
            (Protocol.Vassal.Response.yojson_of_t
               { play = Option.is_some play; data = play })
        in
        let%lwt () = Lwt_mvar.put t.play_response response in
        match play with
        | None -> Lwt.return ()
        | Some play -> handle_card_specific_request t play ~game_state)

  and on_throne_room t ~game_state ~card_to_play =
    let%lwt request = Lwt_mvar.take t.play_request in
    let { Jsonrpc.Request.id; _ } = request in
    let response =
      Jsonrpc.Response.ok id
        (Protocol.ThroneRoom.Response.yojson_of_t card_to_play)
    in
    let%lwt () = Lwt_mvar.put t.play_response response in
    handle_card_specific_request t card_to_play ~game_state

  and handle_card_specific_request (t : t) card ~game_state =
    match card with
    | Play.Harbinger { card_to_topdeck } ->
        on_harbinger t ~game_state ~card_to_topdeck
    | Vassal { play_card_from_topdeck } ->
        on_vassal t ~game_state ~play_card_from_topdeck
    | Poacher { cards_to_discard } -> on_poacher t ~game_state ~cards_to_discard
    | ThroneRoom card_to_play -> on_throne_room t ~game_state ~card_to_play
    | Library { skip_action_card } -> on_library t ~game_state ~skip_action_card
    | Sentry { what_to_do } -> on_sentry t ~game_state ~what_to_do
    | _ -> Lwt.return ()

  let play_card (t : t) card ~(game_state : Game_state.t) : Game_state.t Lwt.t =
    printf t "Playing a card: %s\n" (Sexp.to_string (Play.sexp_of_t card));
    let response =
      Conn.dispatch t.conn ~with_request:(fun id ->
          Jsonrpc.Request.create ~id ~method_:"Play"
            ~params:
              (Jsonrpc.Structured.t_of_yojson
                 (Protocol.Play.Request.yojson_of_t card))
            ())
    in
    let%lwt () = handle_card_specific_request t card ~game_state in
    let%lwt response = response in
    parse_result_exn response ~of_yojson:Protocol.Play.Response.t_of_yojson
    |> Lwt.return

  let buy_card (t : t) card : Game_state.t Lwt.t =
    printf t "Buying a card: %s\n" (Card.to_string card);
    let%lwt (response : Jsonrpc.Response.t) =
      Conn.dispatch t.conn ~with_request:(fun id ->
          Jsonrpc.Request.create ~id ~method_:"Buy"
            ~params:
              (Jsonrpc.Structured.t_of_yojson
                 (Protocol.Buy.Request.yojson_of_t { card }))
            ())
    in
    parse_result_exn response ~of_yojson:Protocol.Play.Response.t_of_yojson
    |> Lwt.return

  let rec play_cards t ~game_state =
    match Ai.next_play t.ai ~game_state with
    | None -> Lwt.return game_state
    | Some play ->
        let%lwt new_game_state = play_card t play ~game_state in
        play_cards t ~game_state:new_game_state

  let rec buy_cards t ~game_state =
    match Ai.next_buy t.ai ~game_state with
    | None -> Lwt.return game_state
    | Some card ->
        let%lwt new_game_state = buy_card t card in
        buy_cards t ~game_state:new_game_state

  let play_turn t ~(game_state : Game_state.t) : unit Lwt.t =
    let%lwt game_state = play_cards t ~game_state in
    let%lwt (_ : Game_state.t) = buy_cards t ~game_state in
    let%lwt () = end_turn t in
    Lwt.return ()

  let on_notification (t : t) (notification : Jsonrpc.Notification.t) =
    let { Jsonrpc.Notification.method_; params; _ } = notification in
    match method_ with
    | "StartTurn" ->
        print_endline t "It's my turn! Deciding what to do...";
        let game_state =
          parse_params_exn params ~of_yojson:Game_state.t_of_yojson
        in
        Lwt.async (fun () ->
            let%lwt () = play_turn t ~game_state in
            print_endline t "Waiting for my turn to come around...";
            Lwt.return ())
    | _ -> printf t "Unknown notification: %s\n" method_

  let on_attack (t : t) (id : Jsonrpc.Id.t) (attack : Attack.Request.t) :
      Jsonrpc.Response.t =
    let { Attack.Request.card; data } = attack in
    printf t "Attacked! %s\n" (Card.to_string card);
    let ok ?reaction ?data () =
      Jsonrpc.Response.ok id (Attack.Response.yojson_of_t { reaction; data })
    in
    match List.find t.current_hand ~f:(Card.equal Card.Moat) with
    | Some _ -> ok ~reaction:Moat ()
    | None -> (
        match card with
        | Bureaucrat -> (
            match Ai.on_bureaucrat t.ai ~current_hand:t.current_hand with
            | None -> ok ~data:(Bureaucrat Reveal) ()
            | Some victory ->
                t.current_hand <- List.diff t.current_hand [ victory ];
                ok ~data:(Bureaucrat (Card victory)) ())
        | Militia ->
            let discard = Ai.on_militia t.ai ~current_hand:t.current_hand in
            t.current_hand <- List.diff t.current_hand discard;
            ok ~data:(Militia discard) ()
        | Witch ->
            Ai.on_witch t.ai;
            ok ()
        | Bandit ->
            let (Bandit top_two_cards) = Option.value_exn data in
            let trash = Ai.on_bandit t.ai ~top_two_cards in
            ok ~data:(Bandit trash) ()
        | card ->
            printf t "Received Attack request with non-attack card: %s"
              (Card.to_string card);
            Jsonrpc.Response.error id
              (Jsonrpc.Response.Error.make ~code:InvalidRequest
                 ~message:"Attacked with unknown attack card" ()))

  let on_request (t : t) (request : Jsonrpc.Request.t) ~game_over :
      Jsonrpc.Response.t Lwt.t =
    let { Jsonrpc.Request.method_; params; id; _ } = request in
    match method_ with
    | "StartGame" ->
        let request =
          parse_params_exn params
            ~of_yojson:Protocol.StartGame.Request.t_of_yojson
        in
        Printf.printf "The game has begun!\n";
        print_s (Protocol.StartGame.Request.sexp_of_t request);
        Jsonrpc.Response.ok id (Protocol.StartGame.Response.yojson_of_t ())
        |> Lwt.return
    | "Harbinger" | "Vassal" | "Poacher" | "ThroneRoom" | "Library" | "Sentry"
      ->
        let%lwt () = Lwt_mvar.put t.play_request request in
        Lwt_mvar.take t.play_response
    | "Attack" ->
        let attack =
          parse_params_exn params ~of_yojson:Attack.Request.t_of_yojson
        in
        on_attack t id attack |> Lwt.return
    | "GameOver" ->
        let { Protocol.GameOver.Request.result; scores } =
          parse_params_exn params
            ~of_yojson:Protocol.GameOver.Request.t_of_yojson
        in
        Printf.printf "Game Over! Result: %s\n" result;
        Map.iteri scores ~f:(fun ~key ~data ->
            Printf.printf "%s: %n\n" key data);
        Lwt.wakeup game_over result;
        Jsonrpc.Response.ok id
          (Protocol.GameOver.Response.yojson_of_t { rematch = t.rematch })
        |> Lwt.return
    | _ -> failwith (sprintf "Received unknown request: %s" method_)

  let main ~(url : string) ~(log_traffic : bool) ~(log_events : bool)
      ~(games : int) : unit Lwt.t =
    (* Awkward recursive knot, so that we can use t in on_notification
       and on_request. *)
    let tref = ref None in
    let game_over_ref = ref None in
    let get_game_over () = Option.value_exn !game_over_ref in
    let get_t () = Option.value_exn !tref in
    let%lwt conn =
      Conn.create ~url
        ~on_request:(fun req ->
          on_request (get_t ()) req ~game_over:(get_game_over ()))
        ~on_notification:(fun notif -> on_notification (get_t ()) notif)
        ~log_traffic
    in
    let results = String.Table.create () in
    let set result =
      Hashtbl.update results result ~f:(function None -> 1 | Some v -> v + 1)
    in
    let rec loop ~games =
      let run_until, game_over = Lwt.wait () in
      let ai = Ai.create () in
      let t = create ~conn ~ai ~rematch:(games > 1) ~log_events in
      tref := Some t;
      game_over_ref := Some game_over;
      let%lwt result = run_until in
      set result;
      if games > 1 then loop ~games:(games - 1) else Lwt.return ()
    in
    let%lwt () = loop ~games in
    print_s (String.Table.sexp_of_t sexp_of_int results);
    Lwt.return ()
end
