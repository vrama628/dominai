import { CreateGameRequest, CreateGameResponse } from "./types";

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
