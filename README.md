# ttt - TicTacToe

This repo is an exploration in custom monads, specifically to represent the game of TicTacToe,
following the "Book of Monads" descriptions in Chapter 13. Implementations are in scala, using
the cats library.

## [Common](src/main/scala/sumidiot/bom/ttt/Common.scala)

This object contains some helpers that are used across a few implementations. In particular,
representations of the players and board positions, as well as some logic for calculating
done-ness and winners.

My concrete implementations of the versions below tend to rely on a `State[GameState, _]`,
where `GameState` captures who'se turn it is and the state of the board. This is a bit different
from the book, which went with `TicTacToe (ReaderT Player (StateT Board IO))`. That version wasn't
as easy for me to get my head around, but maybe I'll return to it later.

## [Final](src/main/scala/sumidiot/bom/ttt/Final.scala)

This object contains the 'final-style' implementation for the custom TicTacToe monad. It's pretty
trippy, honestly.

## [Initial](src/main/scala/sumidiot/bom/ttt/Initial.scala)

This object contains the 'initial-style' implementation for the custom TicTacToe monad. Actually,
it only contains most of it, I haven't worked out the `@tailrec` implementation of `flatMap` which
would be required generally for cats instances of `Monad`.

## [Free](src/main/scala/sumidiot/bom/ttt/Free.scala)

This one is still in progress, and might be slow going.


