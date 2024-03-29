export type Card =
  // TREASURE CARDS
  | "Copper"
  | "Silver"
  | "Gold"
  // VICTORY CARDS
  | "Estate"
  | "Duchy"
  | "Province"
  | "Gardens"
  // CURSE CARD
  | "Curse"
  // ACTION CARDS
  | "Cellar"
  | "Chapel"
  | "Moat"
  | "Harbinger"
  | "Merchant"
  | "Vassal"
  | "Village"
  | "Workshop"
  | "Bureaucrat"
  | "Militia"
  | "Moneylender"
  | "Poacher"
  | "Remodel"
  | "Smithy"
  | "ThroneRoom"
  | "Bandit"
  | "CouncilRoom"
  | "Festival"
  | "Laboratory"
  | "Library"
  | "Market"
  | "Mine"
  | "Sentry"
  | "Witch"
  | "Artisan";

export type Kingdom = Card[];

export type Player = {
  name: string;
};

export type Supply = { [card in Card]: number };

export type CreateGameRequest = {
  kingdom: Kingdom;
  num_players: number;
};

export type CreateGameResponse = {
  key: string;
};

export type GameStateResponse =
  | ["PreStart", { num_players: number; kingdom: Kingdom; players: Player[] }]
  | [
      "Turn",
      {
        kingdom: Kingdom;
        supply: Supply;
        trash: Card[];
        current_player: Player;
        next_players: Player[];
      }
    ];
