import CreateGame from "@/components/CreateGame";
import { createGame } from "@/game";
import { redirect } from "next/navigation";

export default function Home() {
  return (
    <CreateGame
      onSubmit={async ({ key }) => {
        "use server";
        redirect(`/game/${key}`);
      }}
    />
  );
}
