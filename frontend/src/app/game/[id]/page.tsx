import React, { ReactNode, useEffect, useState } from "react";
import GameState from "./GameState";

type GameProps = {
  params: Promise<{ id: string }>;
};
export default async function Game({ params }: GameProps) {
  const { id } = await params;
  return (
    <div className="w-full md:w-[768px] mx-auto">
      <GameState id={id} />
    </div>
  );
}
