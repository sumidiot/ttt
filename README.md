# Final Style

This corresponds to section 13.2 of the Book of Monads, mostly.

I wasn't quite ready for the
`instance TicTacToe (ReaderT Player (StateT Board IO))` conversion,
instead opting (so far) for the simpler `State[(Board, Player), _]`.
There's plenty of things I'm uncertain enough with that version, some of which
show up as inline comments in the [code](src/main/scala/sumidiot/bom/ttt/Main.scala)

