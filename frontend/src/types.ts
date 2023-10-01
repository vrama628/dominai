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

export type CreateGameRequest = {
  kingdom: Kingdom;
  num_players: number;
};

export type CreateGameResponse = {
  key: string;
};
