import type { Card, Kingdom } from "./types";

type AttackRequest =
  | {
      card: "Militia";
    }
  | {
      card: "Bureaucrat";
    }
  | {
      card: "Bandit";
      data: Card[];
    };

export interface PlayerMethods {
  onStartGame(params: { kingdom: Kingdom; order: string[] }): Promise<void>;
  onAttack(params: AttackRequest): Promise<>;
}
