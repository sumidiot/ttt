# ttt

Working through the TicTacToe example in "The Book of Monads", in chapter 13
(Defining Custom Monads).

## Final Style

The first version is in [final style](final). There's still a handful of things
I'd think I should try in this style:
* Implement a player's logic (perhaps just random to start), and then have two
    players actually play each other and obtain a result
* Same as above, but maybe display each move as IO or logging or something
* And/or have one of the player's logic be determined by IO (i.e., asking the user)

Honestly, I find the final style example to be pretty trippy.
* In `TicTacToe[F[_]]` there's never an explicit instance of an `F`.
* There's only one implementation of `TicTacToe[State[Game, _]]`, as long as `Game`
    provides an implementation of the mechanics (info and take... which makes me sorta
    feel like `State` isn't the important bit, somehow, it's more the `Game`. Or, like,
    `State` is the monad, and `Game` is the logic. I don't really know what I'm talking about.

## Initial Style

