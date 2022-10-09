module Request = struct
  module Bandit = struct
    type t = Card.t list [@@deriving yojson]
  end

  module Data = struct
    type t = Bandit of Bandit.t

    let yojson_of_t (Bandit t) = Bandit.yojson_of_t t
    let t_of_yojson y = Bandit (Bandit.t_of_yojson y)
  end

  type t = { card : Card.t; data : (Data.t option[@yojson.option]) }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type just_card = { card : Card.t }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let t_of_yojson y =
    try t_of_yojson y
    with _ ->
      let { card } = just_card_of_yojson y in
      { card; data = None }
end

module Response = struct
  module Bureaucrat = struct
    type t = Card of Card.t | Reveal

    let yojson_of_t = function
      | Reveal -> yojson_of_string "reveal"
      | Card card -> Card.yojson_of_t card

    let t_of_yojson yojson =
      match string_of_yojson yojson with
      | "reveal" -> Reveal
      | _ | (exception _) -> Card (Card.t_of_yojson yojson)
  end

  module Militia = struct
    type t = Card.t list [@@deriving yojson]
  end

  module Bandit = struct
    type t = Card.t option

    let yojson_of_t = function
      | None -> yojson_of_string "null"
      | Some card -> Card.yojson_of_t card

    let t_of_yojson yojson =
      match string_of_yojson yojson with
      | "null" -> None
      | _ | (exception _) -> Some (Card.t_of_yojson yojson)
  end

  module Data = struct
    type t =
      | Bureaucrat of Bureaucrat.t
      | Militia of Militia.t
      | Bandit of Bandit.t

    let yojson_of_t = function
      | Bureaucrat t -> Bureaucrat.yojson_of_t t
      | Militia t -> Militia.yojson_of_t t
      | Bandit t -> Bandit.yojson_of_t t

    let t_of_yojson y =
      match Bureaucrat.t_of_yojson y with
      | exception _ -> (
          match Militia.t_of_yojson y with
          | exception _ -> Bandit (Bandit.t_of_yojson y)
          | m -> Militia m)
      | m -> Bureaucrat m
  end

  type t = {
    reaction : Card.t option; [@yojson.option]
    data : Data.t option; [@yojson.option]
  }
  [@@deriving yojson]
end
