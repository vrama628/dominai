module type S = sig
  type t

  val create : unit -> t
  val next_play : t -> game_state:Game_state.t -> Play.t option
  val next_buy : t -> game_state:Game_state.t -> Card.t option
  val on_militia : t -> current_hand:Card.t list -> Card.t list
  val on_bureaucrat : t -> current_hand:Card.t list -> Card.t option
  val on_bandit : t -> top_two_cards:Card.t list -> Card.t option
  val on_witch : t -> unit
end
