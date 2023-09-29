type Card =
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

type Kingdom = Card[];

type CreateGameRequest = {
  kingdom: Kingdom;
  num_players: number;
};

type CreateGameResponse = {
  key: string;
};
