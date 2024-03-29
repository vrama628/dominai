"use client";
import { Card } from "@/types";
import React from "react";

const CardView: React.FC<{
  card?: Card;
}> = ({ card }) => {
  return <img src={`/cards/${card}.jpg`} alt={card} />;
};

export default CardView;
