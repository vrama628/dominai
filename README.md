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
    kingdom: Card[];
    order: string[]; // player names in turn order
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
the time you end your turn with an `EndTurn` request.

### `StartTurn`
```
"StartTurn" // game to player notification
Request:
{
    hand: Card[];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
    buys: number; // always 1
    actions: number; // always 1
    treasure: number; // always 0
}
```

### `EndTurn`
```
"EndTurn" // player to game request
Request:
{}
Response:
{
    hand: Card[];
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
    data: <see below>;
}
Response:
{
    hand: Card[];
    discard: number;
    deck: number;
    supply: { [Card]: number; };
    buys: number;
    actions: number;
    treasure: number;
}
```
Depending on which card you play, you may need to supply additional information
in the `data` field of the request.
The cards for which this is the case and the expected data are listed below.
In all other cases, the `data` field must be `null`.
```
{
    card: "Cellar";
    data: Card[]; // the cards to discard
}
{
    card: "Chapel";
    data: Card[]; // the cards to trash
}
{
    card: "Workshop";
    data: Card; // the card to gain
}
{
    card: "Moneylender";
    data: boolean; // whether to trash a copper for +3
}
{
    card: "Poacher";
}
{
    card: "Remodel";
    data: {
        trash: Card; // the card from your hand to trash
        gain: Card; // the card from the supply to gain
    };
}
{
    card: "ThroneRoom";
    data: {
        card: Card; // the card to throne
        data: <data>; // the data to play the throned card with the first time
    }; 
}
{
    card: "Mine";
    data: {
        trash: Card; // the card from your hand to trash
        gain: Card; // the treasure from the supply to gain to your hand
    };
}
{
    card: "Artisan";
    data: {
        gain: Card; // the card from the supply to gain to your hand
        topdeck: Card; // the card from your hand to topdeck
    };
}
```
In some situations, the server may send the player a request
with additional information which the player must respond to in order to
complete playing the card.
The cards for which this is may apply and the associated request/response formats
are listed below.
```
"Harbinger" // game to player request
Request:
{
    discard: Card[]; // the player's discard pile
}
Response:
{
    card: Card; // the card to topdeck
}

"Vassal" // game to player request
Request:
{
    card: Card; // the action card from the top of your deck
}
Response:
{
    play: boolean; // whether to play the card.
    data: <see above>; // data required to play this card
}

"Poacher" // game to player request
Request:
{
    hand: Card[]; // your current hand
    empty_supply_piles: number; // the number of cards you must discard
}
Response:
{
    discard: Card[]; // the cards to discard from your hand
}

"ThroneRoom" // game to player request
Request:
{
    card: Card; // the card you throned
}
Response:
{
    data: Card[]; // the data to play the card with the second time
}

"Library" // game to player request
Request:
{
    card: Card; // an action card that you may choose to skip
    hand: Card[]; // the current contents of your hand
}
Response:
{
    skip: boolean; // whether to skip placing this card into your hand
}

"Sentry" // game to player request
Request:
{
    hand: Card[]; // your current hand
    cards: Card[]; // the top 2 cards of your deck
}
Response:
{
    card: Card; // which of the revealed cards this pertains to
    placement: "trash" | "discard" | "topdeck"; // what to do to this card
}[] // NOTE: the response is a two-element array.
    // The actions in the array are performed in order, so if you wish
    // to topdeck both cards in a particular order,
    // then put the card you wish to be on top second in this array.
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
    data?: <see below>;
}
Response:
{
    reaction?: "Moat";
    data?: <see below>;
}
```
Depending on which card you're attacked with, you may be given additional information in the `data`
field of the request and/or (if you do not react with a Moat) need to supply additional information
in the `data` field of the response.
The cards for which this is the case and the expected data are listed below.
In all other cases, the `data` field must be `null` or not present.
```
Request:
{
    card: "Bureaucrat";
}
Response:
{
    data: Card | "reveal";
    // the victory card you will topdeck, or
    // "reveal" if you have no victory cards in your hand.
}

Request:
{
    card: "Militia";
}
Response:
{
    data: Card[];
    // the cards to discard from your hand
}

Request:
{
    card: "Bandit";
    data: Card[]; // the top 2 cards from your deck
}
Response:
{
    data: Card | null;
    // the non-copper treasure to trash, or `null` if there isn't one
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

When you send the game malformed data,
the game sends you a `FatalError` notification
then immediately forfeits and disconnects you from the game.

### `FatalError`
```
"FatalError" // game to player notification
Notification:
{
    message: string;
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
    result: "Win" | "Lose";
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
