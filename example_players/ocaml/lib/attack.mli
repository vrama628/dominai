module Request : sig
  module Bandit : sig
    type t = Card.t list [@@deriving yojson]
  end

  module Data : sig
    type t = Bandit of Bandit.t [@@deriving yojson]
  end

  type t = { card : Card.t; data : Data.t option } [@@deriving yojson]
end

module Response : sig
  module Bureaucrat : sig
    type t = Card of Card.t | Reveal [@@deriving yojson]
  end

  module Militia : sig
    type t = Card.t list [@@deriving yojson]
  end

  module Bandit : sig
    type t = Card.t option [@@deriing yojson]
  end

  module Data : sig
    type t =
      | Bureaucrat of Bureaucrat.t
      | Militia of Militia.t
      | Bandit of Bandit.t
    [@@deriving yojson]
  end

  type t = { reaction : Card.t option; data : Data.t option }
  [@@deriving yojson]
end
