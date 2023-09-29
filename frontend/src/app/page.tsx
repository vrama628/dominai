"use client";

import { createGame } from "@/game";

export default function Home() {
  return (
    <div>
      Hello
      <button
        onClick={(e) => {
          e.preventDefault();
          createGame({
            kingdom: [
              "Cellar",
              "Chapel",
              "Moat",
              "Harbinger",
              "Merchant",
              "Vassal",
              "Village",
              "Workshop",
              "Bureaucrat",
              "Militia",
            ],
            num_players: 2,
          }).then((resp) => console.log(resp));
        }}
      >
        Click
      </button>
    </div>
  );
}
