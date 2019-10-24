# ttt - TicTacToe

This repo is an exploration in custom monads, specifically to represent the game of TicTacToe,
following the "Book of Monads" descriptions in Chapter 13. Implementations are in scala, using
the cats library.

The book implements `takeIfNotTaken` for its custom monads. We extend this with `runRandom`,
which probably doesn't do really the right thing with `Random` as a monad, but it's a start.
We've also added in generic `winner` and `gameEnded` methods.

The current `GameState` is somewhat insufficient, really there should be two classes of states,
one with a 'next player' and one representing a 'done' game (with either draw or winner,
and possibly, for completeness/verifiability, the winning combo).

## [Common](src/main/scala/sumidiot/bom/ttt/Common.scala)

This object contains some helpers that are used across a few implementations. In particular,
representations of the players and board positions, as well as some logic for calculating
done-ness and winners. Note, however, that we intend to pull that logic out for use by any of
the monad implementations.

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

I'll probably evolve the Final and Free implementations, leaving this one alone for a while,
because mostly this is used to motivate the Free monad.

## [Free](src/main/scala/sumidiot/bom/ttt/TTTFree.scala)

This object contains the `Free`-based implementation of the TicTacToe monad. It's much nicer than
the custom initial one. It's also fun to have written the Final version first, and then see that
you can copy over the implementation for Free, and simply change the type declarations to remove
the generic `F` and replace it with `TicTacToe`.

## [OO](src/main/scala/sumidiot/bom/ttt/OO.scala)

I wanted to get a comparison to something maybe more "normal" looking, maybe more "object-oriented"
and without all the `Monad` bits. Basically I copied over Final and then made it compile.
I'd certainly expect more folks to have lower cognitive overhead reading this version than the Free
or Final versions.

## [OOTypeclass](src/main/scala/sumidiot/bom/ttt/OOTypeclass.scala)

Here's what I thought might be a minor variation on the OO implementation, using a typeclass
instead of inheritance. I found, however, that since the OO implementation sort of assumed a hidden
state, the `OOTypeclass` version that didn't had a few additional changes.

## The Laws of the Algebra

Here we note, until we can encode in tests, some laws for the TicTacToe algebra.
We assume the `turn`, `take`, and `info` methods. The notation below is a bit brief:
1. info is idempotent
2. turn is idempotent
3. info then turn is "the same as" turn then info
4. info(pos) is none && p = turn then gentake(pos) is NextTurn or GameEnded and info(pos) = p and p' = turn
5. info(pos) is some then gentake(pos) is AlreadyTaken and info(pos) is same some and turn is unchanged
6. win / draw states
