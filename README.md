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
3. Run `opam init` (this may take a few minutes).
4. Run `opam install . --deps-only` to install this project's dependencies.
   If this doesn't succeed, you may need to install `conf-libev` and `conf-pkg-config` and try again.
   This is system-dependent.
5. Run the server with `dune exec dominai`

You only need to do each of steps 1-4 once; once you've done them you can do
step 5 at any time to run the server again.