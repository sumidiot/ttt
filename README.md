# ttt - TicTacToe

This repo is an exploration in custom monads, specifically to represent the game of TicTacToe,
following the "Book of Monads" descriptions in Chapter 13. Implementations are in scala, using
the cats library. The general setup of the book is to define a TicTacToe with methods
* `info :: Position -> Option[Player]`
* `take :: Position -> Result`
where `Position`, `Player`, and `Result` are some types left as an exercise to the reader.
Actually, the above leaves out the `TicTacToe` monadic "wrapper" of the result types, but that's
because for this repo we sometimes have exactly the signatures above, as we compare the monad
versions with more "OO"-style ones.

In the book, the suggested implementation is based on `ReaderT Player (StateT Board IO)`. In
my original understanding and implementation, the monad was supposed to sort of know about the
whole game state, not just the board state but also whose turn it is. As I worked my way through
what's here, I came to realize that possibly this was not the original intention, that the
`TicTacToe` monad proposed in the exercise was really for the _board_, and it was a separate
concern to manage whose turn it is. I may return to implementing this sort of separation,
but for now, all of my implementations aim for the broader goal of the `TicTacToe` knowing the
board and player state.

Additionally, in my playing with things, I came to desire that the `take` method of the monad
be a "forceful" take, disregarding if a player already occupied the position. So in my
abstractions, I have `forceTake` (which returns `Unit`), and then a separate `genTake` which
returns the (monad-wrapped) `Result`. The point of this is was that I didn't want to leave it up
to any specific implementations to have to worry about all the checks that should happen when you
try to take a position (is the game already over? is the spot already taken? if you take the spot,
does it cause you to win?).

After the methods above, the book implements `takeIfNotTaken` based on those primitives.
We extend this with `runRandom`, which probably doesn't do really the right thing with
`Random` as a monad, but it's a start. We've also added in generic `winner` and `gameEnded` methods.
These are duplicated by "simpler" methods in the `Common.BoardHelpers` (more on `Common` below),
but the point is that without those "simpler" methods, we can still implement basically the same
logic, even in custom monads.

The current `GameState` is somewhat insufficient, really there should be two classes of states,
one with a 'next player' and one representing a 'done' game (with either draw or winner,
and possibly, for completeness/verifiability, the winning combo).

To get started with this codebase, begin in `Common` for the various types. Then, to follow
roughly the order in which variants were added, follow the other sections below. There's also
some [tests](src/test/scala/sumidiot/bom/ttt/), mostly to demonstrate using property-based tests.

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

## [ROF](src/main/scala/sumidiot/bom/ttt/ROF.scala)

This "Record Of Functions" version is just a quick way to play with what I take to be what
"record of functions" means, as a thing to do instead of making new typeclasses (e.g., in Haskell).
It seems like a pretty nice version, honestly.

## Tests

There's some property-based [tests](src/test/scala/sumidiot/bom/ttt/) set up, mostly as an excuse
to play around with such things. The main thing it checks is that `genTake` is well-behaved for
different implementations.
