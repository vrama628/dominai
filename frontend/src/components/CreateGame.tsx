"use client";
import type { Card, CreateGameResponse } from "@/types";
import { Listbox, Menu, Transition } from "@headlessui/react";
import { useState } from "react";
import { HiOutlineSelector } from "react-icons/hi";
import { FaDice } from "react-icons/fa";
import CardView from "./CardView";
import { createGame } from "@/game";
import MenuTransition from "./MenuTransition";

const NumPlayersSelector: React.FC<{
  numPlayers: number;
  setNumPlayers: (numPlayers: number) => void;
}> = ({ numPlayers, setNumPlayers }) => {
  return (
    <Listbox
      value={numPlayers}
      onChange={setNumPlayers}
      as="div"
      className="relative"
    >
      <Listbox.Button className="px-4 py-2 border rounded flex items-center gap-4 transition-colors hover:bg-gray-100">
        {numPlayers} Players <HiOutlineSelector />
      </Listbox.Button>
      <MenuTransition>
        <Listbox.Options className="absolute top-full bg-white rounded shadow z-10">
          {[2, 3, 4, 5, 6].map((n) => (
            <Listbox.Option
              value={n}
              className="cursor-pointer px-4 py-2 rounded transition-colors hover:bg-gray-100"
            >
              {n} Players
            </Listbox.Option>
          ))}
        </Listbox.Options>
      </MenuTransition>
    </Listbox>
  );
};

const kindgomCards: Card[] = [
  "Gardens",
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
  "Moneylender",
  "Poacher",
  "Remodel",
  "Smithy",
  "ThroneRoom",
  "Bandit",
  "CouncilRoom",
  "Festival",
  "Laboratory",
  "Library",
  "Market",
  "Mine",
  "Sentry",
  "Witch",
  "Artisan",
];

function randomKingdom(): Card[] {
  const kingdom: Card[] = [];
  while (kingdom.length < 10) {
    const card = kindgomCards[Math.floor(Math.random() * kindgomCards.length)];
    if (!kingdom.includes(card)) {
      kingdom.push(card);
    }
  }
  return kingdom;
}

const KingdomSelector: React.FC<{
  kingdom: Card[];
  setKingdom: (kingdom: Card[]) => void;
}> = ({ kingdom, setKingdom }) => {
  return (
    <div>
      {kingdom.map((card, key) => (
        <Menu key={key} as="div" className="inline-block w-1/5 relative">
          <Menu.Button>
            <CardView card={card} />
          </Menu.Button>
          <MenuTransition>
            <Menu.Items className="absolute top-4 left-4 z-10 bg-white rounded shadow h-96 overflow-y-auto">
              {kindgomCards.map((card) => {
                const onClick = kingdom.includes(card)
                  ? undefined
                  : () => {
                      setKingdom(kingdom.map((c, i) => (i === key ? card : c)));
                    };
                return (
                  <Menu.Item
                    as="button"
                    className={`px-2 py-1 transition-colors w-full ${
                      onClick
                        ? "hover:bg-gray-100"
                        : "text-gray-500 bg-gray-100"
                    }`}
                    onClick={onClick}
                    disabled={!onClick}
                  >
                    {card}
                  </Menu.Item>
                );
              })}
            </Menu.Items>
          </MenuTransition>
        </Menu>
      ))}
    </div>
  );
};

const CreateGame: React.FC<{
  onSubmit: (createGameResponse: CreateGameResponse) => void;
}> = ({ onSubmit }) => {
  const [numPlayers, setNumPlayers] = useState<number>(2);
  const [kingdom, setKingdom] = useState<Card[]>(randomKingdom());
  const onCreateGame = () => {
    createGame({
      num_players: numPlayers,
      kingdom,
    }).then(onSubmit);
  };
  return (
    <div className="w-full md:w-[768px] mx-auto border rounded">
      <div className="m-4 flex">
        <NumPlayersSelector
          numPlayers={numPlayers}
          setNumPlayers={setNumPlayers}
        />
        <div className="grow" />
        <button
          className="border rounded px-4 py-2 transition-colors hover:bg-gray-100 flex items-center gap-4"
          onClick={(e) => {
            e.preventDefault();
            setKingdom(randomKingdom());
          }}
        >
          Randomize Kingdom
          <FaDice />
        </button>
      </div>
      <div className="m-4">
        <KingdomSelector kingdom={kingdom} setKingdom={setKingdom} />
      </div>
      <div className="text-end m-4">
        <button
          className="rounded px-4 py-2 transition-colors text-white bg-gray-400 hover:bg-gray-500"
          onClick={onCreateGame}
        >
          Create Game
        </button>
      </div>
    </div>
  );
};

export default CreateGame;
