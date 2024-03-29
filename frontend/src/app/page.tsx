"use client";
import CreateGame from "@/components/CreateGame";
import { createGame } from "@/game";
import { useRouter } from "next/navigation";

export default function Home() {
  const router = useRouter();
  return (
    <CreateGame
      onSubmit={({ key }) => {
        router.push(`/game/${key}`);
      }}
    />
  );
}
