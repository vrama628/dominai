"use client";

import { getGameState, joinGame } from "@/game";
import { GameStateResponse } from "@/types";
import { Transition } from "@headlessui/react";
import { ReactNode, useEffect, useState } from "react";

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
                const socket = joinGame(id, username);
                setSocket(socket);
                socket.addEventListener("message", (event) => {
                  console.log("messsage", event);
                });
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

type GameStateProps = { id: string };
export default function GameState({ id }: GameStateProps) {
  const [gameState, setGameState] = useState<GameStateResponse | null>(null);

  useEffect(() => {
    getGameState(id).then(setGameState);
  }, [id]);

  if (!gameState) return <div>Loading...</div>;
  switch (gameState[0]) {
    case "PreStart":
      const { num_players, kingdom, players } = gameState[1];
      return <JoinAndPlayGame id={id} />;
    case "Turn":
      return <div>Game is in progress.</div>;
  }
}
