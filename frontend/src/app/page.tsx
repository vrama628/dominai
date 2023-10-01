"use client";
import CreateGame from "@/components/CreateGame";
import { createGame } from "@/game";

export default function Home() {
  return (
    <div>
      <div className="text-center text-3xl my-4">DominAI</div>
      <CreateGame onSubmit={({ key }) => alert(key)} />
    </div>
  );
}
