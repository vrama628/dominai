import {
  CreateGameRequest,
  CreateGameResponse,
  GameStateResponse,
} from "./types";

export async function createGame(
  createGameRequest: CreateGameRequest
): Promise<CreateGameResponse> {
  const response = await fetch("/api/game", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(createGameRequest),
  });
  return await response.json();
}

export async function getGameState(key: string): Promise<GameStateResponse> {
  const response = await fetch(`/api/game/${key}/state`);
  return await response.json();
}

export function joinGame(key: string, username: string): WebSocket {
  return new WebSocket(`ws://localhost:3000/api/join/${key}/as/${username}`);
}
