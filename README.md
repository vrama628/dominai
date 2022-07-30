# DominAI

Welcome to DominAI, a server for playing Dominion games.
This server hosts a game of Dominion (using only the cards from the base game).
It is designed so that you can write a Dominion player in any language of your
choosing and connect to it to play against another player.
I plan to host this server online soon so that we can play our dominion players
against one another,
but until then you can run this server locally to play your dominion players
against themselves.
We also hope to soon have a UI for the Dominion player interface so that you
can yourself play against your (or other peoples') code players.

If you have questions, reach out to me directly or post an Issue to this
repository.
If you have contributions, Pull requests are welcome!

# How to run the server

1. Clone this repository and go to it in your terminal.
2. Install [opam](https://opam.ocaml.org/doc/Install.html), the package manager
    for OCaml.
3. Run `opam init` (this may take a few minutes). Then run `opam install . --deps-only` to install this project's dependencies.
4. You may need to install `conf-libev` and `conf-pkg-config`. This is system-dependent. 
5. Run the server with `dune exec dominai`

You only need to do each of steps 1-3 once; once you've done them you can do
step 4 at any time to run the server again.

# How to make a Dominion player

You don't need to know anything about AI to write code that plays dominion!
You may write your code in any language.
Your code will communicate with the server using
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) via
[WebSocket](https://en.wikipedia.org/wiki/WebSocket).
I'm happy to help you get started.

The rest of this document will describe the format of how your code
and the game server will communicate to each other.

When you run the game server it'll by default serve at `http://localhost:8080`.
Your code will connect a WebSocket at this address.

## Start of game

The game will automatically start once two players are connected to the server.
At this time, the server will send a `StartGame` request to all players.

### `StartGame`

```
"StartGame" // game to player request
Request:
{
    kingdom: [Card];
    order: [string]; // player names in turn order
}
Reponse:
{}
```
The game sends this notification to all players to inform them of this game's
kingdom and turn order.
Once all players have responded to this request, the game will begin.

## Turns

When it is your turn, the game will send your player a `StartTurn` notification.
Your turn lasts from the time you receive this notification until
the time you end your turn with a `CleanUp` request.

### `StartTurn`
```
"StartTurn" // game to player notification
Request:
{
    hand: [Card];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
    buys: number; // always 1
    actions: number; // always 1
    treasure: number; // always 0
}
```

### `CleanUp`
```
"CleanUp" // player to game request
Request:
{}
Response:
{
    hand: [Card];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
}
```

During your turn, you may play and buy cards.
Note that once you've played a treasure card or bought a card,
you may not play actions for the remainder of your turn.
You may only play and buy cards during your turn.

### `Play`
```
"Play" // player to game request
Request:
{
    card: Card;
}
Response:
{
    hand: [Card];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
    buys: number;
    actions: number;
    treasure: number;
}
```

### `Buy`
```
"Play" // player to game request
Request:
{
    card: Card;
}
Response:
{
    hand: [Card];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
    buys: number;
    actions: number;
    treasure: number;
}
```

## Other requests and notifications

When another player attacks, the game will send you an `Attack` request.
If you currently have a moat in your hand, you may respond with the
`"Moat"` reaction, otherwise you must respond with no reaction.

### `Attack`
```
"Attack" // game to player request
Request:
{
    card: Card;
}
Response:
{
    reaction?: "Moat" | null;
}
```

When another player plays a card, the game sends you a `Played`
notification to let you know.

### `Played`
```
"Played" // game to player notification
Notification:
{
    player: string;
    card: Card;
}
```

When your discard pile is shuffled, the game sends you a `Shuffle` notification.

### `Shuffle`
```
"Shuffle" // game to player notification
Notification:
{}
```

## End of game

Once the game is over, the game will send you a `GameOver` notification.

### `GameOver`
```
"GameOver" // game to player notification
Notification:
{
    result: "win" | "lose" | "draw";
    scores: { [string]: number };
}
```

## Cards
```
type Card =
  // TREASURE CARDS
  | "Copper"
  | "Silver"
  | "Gold"
  // VICTORY CARDS
  | "Estate"
  | "Duchy"
  | "Province"
  | "Gardens"
  // CURSE CARD
  | "Curse"
  // ACTION CARDS
  | "Cellar"
  | "Chapel"
  | "Moat"
  | "Harbinger"
  | "Merchant"
  | "Vassal"
  | "Village"
  | "Workshop"
  | "Bureaucrat"
  | "Militia"
  | "Moneylender"
  | "Poacher"
  | "Remodel"
  | "Smithy"
  | "ThroneRoom"
  | "Bandit"
  | "CouncilRoom"
  | "Festival"
  | "Laboratory"
  | "Library"
  | "Market"
  | "Mine"
  | "Sentry"
  | "Witch"
  | "Artisan"
```
