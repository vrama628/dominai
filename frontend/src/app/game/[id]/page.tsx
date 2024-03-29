"use client";
import { getGameState, joinGame } from "@/game";
import { GameStateResponse } from "@/types";
import { Transition } from "@headlessui/react";
import React, { ReactNode, useEffect, useState } from "react";

type Props = {
  params: { id: string };
};

const TransitionInOut: React.FC<{ show: boolean; children: ReactNode }> = ({
  show,
  children,
}) => {
  return (
    <Transition
      show={show}
      enter="transition"
      enterFrom="opacity-0 scale-95 translate-y-full"
      enterTo="opacity-100 scale-100 translate-y-0"
      leave="transition"
      leaveFrom="opacity-100 scale-100 translate-y-0"
      leaveTo="opacity-0 scale-95 -translate-y-full"
    >
      {children}
    </Transition>
  );
};

const JoinAndPlayGame: React.FC<{ id: string }> = ({ id }) => {
  const [username, setUsername] = useState("");
  const [socket, setSocket] = useState<WebSocket | null>(null);
  return (
    <>
      <TransitionInOut show={socket === null}>
        <div className="flex flex-col items-center">
          Waiting for game to start.
          <div className="flex gap-2">
            Username:
            <input
              type="text"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
            />
            <button
              onClick={(e) => {
                e.preventDefault();
                setSocket(joinGame(id, username));
              }}
            >
              Join Game
            </button>
          </div>
        </div>
      </TransitionInOut>
    </>
  );
};

const GameState: React.FC<{ id: string; gameState: GameStateResponse }> = ({
  id,
  gameState,
}) => {
  switch (gameState[0]) {
    case "PreStart":
      const { num_players, kingdom, players } = gameState[1];
      return <JoinAndPlayGame id={id} />;
    case "Turn":
      return <div>Game is in progress.</div>;
  }
};

export default function Game({ params: { id } }: Props) {
  const [gameState, setGameState] = useState<GameStateResponse>([
    "PreStart",
    {
      num_players: 0,
      players: [],
      kingdom: [],
    },
  ]);
  useEffect(() => {
    getGameState(id).then(setGameState);
  }, [id]);
  return (
    <div className="w-full md:w-[768px] mx-auto">
      <GameState id={id} gameState={gameState} />
    </div>
  );
}
