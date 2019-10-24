package sumidiot.bom.ttt

import Common._

import cats._
import cats.implicits._
import cats.data.State


/**
 * With final-style, we set up a typeclass for being a TicTacToe, where the methods
 * return things "wrapped" in the typeclass instance. When we need to chain those methods to define
 * higher-level methods (e.g., takeIfNotTaken below), we add Monad (or Applicative)
 * requirements to F.
 */
object Final extends App {

  /**
   * The original version of this was supposed to have a
   *   take(p: Position): F[Result]
   *
   * However, we have since implemented `genTake`, which requires the `forceTake` version
   *
   * Now, genTake requires a monad, where the original `take` suggestion possibly
   * got around that. However, all `take` implementations would likely have the same
   * (`genTake`) pattern.
   *
   * In the real world, we'd probably set visibility restrictions on `forceTake`,
   * and require users to rely on `genTake` (and maybe rename it back to just `take`).
   *
   * Note that we have also added `turn`. We anticipate updating this to be Option[Player]
   * in the near future, to account for games which have completed.
   */
  trait TicTacToe[F[_]] {
    def info(p: Position): F[Option[Player]]
    def forceTake(p: Position): F[Unit]
    def turn(): F[Player]
  }

  /**
   * This syntax ends up letting us write exactly the same implementation of
   * takeIfNotTaken between Final and Free implementations.
   * This makes me think of these a little like the smart constructors.
   */
  object TicTacToeSyntax {
    def info[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Option[Player]] =
      ev.info(p)

    def forceTake[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Unit] =
      ev.forceTake(p)

    def turn[F[_]]()(implicit ev: TicTacToe[F]): F[Player] =
      ev.turn()
  }

  import TicTacToeSyntax._

  /**
   * This was the method suggested as an exercise in the book. Note, here,
   * we rely on our `genTake`, instead of the original `take`.
   */
  def takeIfNotTaken[F[_] : TicTacToe : Monad](p: Position): F[Option[Result]] =
    for {
      op <- info(p)
      or <- op.fold(genTake(p).map(_.some))(p => none.pure[F])
    } yield {
      or
    }

  /**
   * This method is roughly just recursive, taking a random step until that
   * causes the game to be over
   */
  def runRandom[F[_] : TicTacToe : Monad](exceptions: Set[Position] = Set.empty): F[Option[Player]] = {
    val rpos = randomPosition(exceptions)
    def cont(r: Result): F[Option[Player]] =
      r match {
        case Result.GameEnded(op) => op.pure[F]
        case _                    => runRandom(exceptions + rpos)
      }
    for {
      r <- genTake(rpos)
      res <- cont(r)
    } yield {
      res
    }
  }

  /**
   * We can generically check for a winner by running through all the
   * winning combinations. This relies only on the `info` method of the `TicTacToe`,
   * adding also the Applicative constraint.
   */
  def winner[F[_] : TicTacToe : Applicative]: F[Option[Player]] = {
    
    /**
     * We implement this method with two helper methods, and the implementation
     * works bottom-up - the final evaluated expression is a .traverse at the bottom,
     * calling the method in the middle, which calls the top function. Basically,
     * the idea is to, for each winning combination, pull out which player occupies
     * each cell in that combo (this is the middle function, `comboWinner`), and
     * given those three `Option[Player]` see if they're all defined and the same player.
     */

    def playerWins(op1: Option[Player], op2: Option[Player], op3: Option[Player]): Option[Player] =
      for {
        p1 <- op1
        p2 <- op2
        p3 <- op3 if p1 == p2 && p2 == p3
      } yield {
        p3
      }
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): F[Option[Player]] =
      (info(pos1), info(pos2), info(pos3)).mapN(playerWins)

    winningCombos
      .traverse(t => comboWinner(t._1, t._2, t._3))
      .map(_.flatten.headOption)
  }

  /**
   * Given just the `info` method of a `TicTacToe` we can check if the game has ended and,
   * if it has, who has won. Like winner, this relies on the extra `Applicative` contraint.
   */
  def gameEnded[F[_] : TicTacToe : Applicative]: F[Option[Result.GameEnded]] = {

    /**
     * This method is implemented somewhat bottom-up. The final for-comprehension at the bottom
     * relies on the `drawResult` method in the middle, which calls the top `isDraw` method.
     * Basically the idea is:
     *   1. See if there's a winner - if so, we're done
     *   2. Otherwise, see if we're in a draw (all positions taken) and return as appropriate
     */

    def isDraw: F[Boolean] =
      allPositions
        .traverse(pos => info(pos))
        .map(_.traverse(x => x).isDefined)

    def drawResult: F[Option[Result.GameEnded]] =
      isDraw.map(d =>
          if (d) {
            Some(Result.GameEnded(None))
          } else {
            None
          }
      )

    def result(op: Option[Player], or: Option[Result.GameEnded]): Option[Result.GameEnded] =
      op.map(p => Result.GameEnded(Some(p))).orElse(or)

    (winner, drawResult).mapN(result)
  }
  
  /**
   * We know most of the steps required to generically 'take' a position,
   * if somebody can "force" the move. In particular, we must first check
   * if the game is already done, sort of as an initial guard. If the game
   * isn't done, we can check if the position is already taken. If it's not
   * then we can forceTake, and then check the game state.
   */
  def genTake[F[_] : TicTacToe : Monad](pos: Position): F[Result] = {

    /**
     * This implementation reads sort of from the bottom up. The first def
     * is used in the next def, which is used in the final `for`-comprehension.
     *
     * Note that there are 3 types of Result that can be returned:
     *   1. GameEnded, which we basically pass through from `gameEnded`
     *   2. AlreadyTaken, if the position is already taken
     *   3. NextTurn, if we successfully took the position and the game isn't over
     *       (or, if it is, we'll return that GameEnded)
     */

    def forceTakeAndCheck(pos: Position): F[Result] =
      for {
        _ <- forceTake(pos)
        ge <- gameEnded.map(_.getOrElse(Result.NextTurn))
      } yield {
        ge
      }

    def takeSinceNotDone(pos: Position): F[Result] =
      for {
        op <- info(pos) // op is an Option[Player]
        res <- op.fold(forceTakeAndCheck(pos))(p => (Result.AlreadyTaken(p) : Result).pure[F])
      } yield {
        res
      }

    for {
      ge <- gameEnded
      res <- ge.fold(takeSinceNotDone(pos))(r => (r: Result).pure[F])
    } yield {
      res
    }
  }


  object TicTacToe {

    /**
     * This provides an implementation of SGS[_] as a TicTacToe.
     */
    implicit case object SGSIsTicTacToe extends TicTacToe[SGS] {
      override def info(p: Position): State[GameState, Option[Player]] =
        for {
          game <- State.get[GameState]
        } yield {
          game.b.get(p)
        }

      override def forceTake(pos: Position): State[GameState, Unit] =
        for {
          game <- State.get[GameState]
          nb = game.b + (pos -> game.p)
          ng = GameState(Player.other(game.p), nb)
          _ <- State.set(ng)
        } yield { () }

      override def turn(): State[GameState, Player] =
        for {
          game <- State.get[GameState]
        } yield {
          game.p
        }

    }
  }


  /**
   * This is the 'main' of the 'App', just a quick demo
   */
  println(runRandom().run(StartingGame).value)

}
